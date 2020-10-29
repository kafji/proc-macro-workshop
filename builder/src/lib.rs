extern crate proc_macro;

use crate::field::*;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::{
    convert::{self, TryInto},
    ops,
};
use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Field, FieldValue, GenericArgument, Ident,
    ImplItemMethod, ItemImpl, ItemStruct, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let output = match derive_impl(input) {
        Ok(data) => data,
        Err(err) => err.to_compile_error(),
    };
    proc_macro::TokenStream::from(output)
}

type Result<T> = std::result::Result<T, error::Error>;

fn derive_impl(input: DeriveInput) -> Result<TokenStream> {
    let strukt: BuilderStruct = (&input).try_into()?;
    let struct_definition = def_builder_struct(&strukt)?;
    let setters = impl_builder_setters(&strukt)?;
    let builder_ctor = impl_builder_ctor(&strukt)?;
    let target_ctor = impl_target_ctor(&strukt)?;
    let output = quote! {
        #struct_definition
        #setters
        #builder_ctor
        #target_ctor
    };
    Ok(output)
}

#[cfg(test)]
fn create_input() -> DeriveInput {
    parse_quote! {
        #[derive(Builder)]
        pub struct Command {
            executable: String,
            #[builder(each = "arg")]
            args: Vec<String>,
            #[builder(each = "env")]
            env: Vec<String>,
            current_dir: Option<String>,
        }
    }
}

fn def_builder_struct<'a>(strukt: &'a BuilderStruct) -> Result<ItemStruct> {
    let struct_name = &strukt.builder_name;
    let (field_name, field_ty): (Vec<_>, Vec<Type>) = strukt
        .fields
        .iter()
        .map(|field| -> (_, Type) {
            let ty = field.ty();
            let ty = if field.is_repeatable() {
                parse_quote! { std::vec::Vec<#ty> }
            } else {
                parse_quote! { std::option::Option<#ty> }
            };
            (field.name(), ty)
        })
        .unzip();
    let output = syn::parse_quote! {
        #[derive(std::default::Default)]
        pub struct #struct_name {
            #(#field_name: #field_ty,)*
        }
    };
    Ok(output)
}

#[cfg(test)]
#[test]
fn test_def_builder_struct() {
    let input = create_input();
    let builder_struct: BuilderStruct = (&input).try_into().unwrap();

    let output = def_builder_struct(&builder_struct);

    let expected: Result<ItemStruct> = Ok(parse_quote! {
        #[derive(std::default::Default)]
        pub struct CommandBuilder {
            executable: std::option::Option<String>,
            args: std::vec::Vec<String>,
            env: std::vec::Vec<String>,
            current_dir: std::option::Option<String>,
        }
    });
    assert_eq!(expected.stringify(), output.stringify());
}

fn impl_builder_setters<'a>(strukt: &'a BuilderStruct) -> Result<ItemImpl> {
    let struct_name = &strukt.builder_name;

    fn gen_setters(field: &BuilderField) -> impl Iterator<Item = ImplItemMethod> {
        match field {
            BuilderField::Repeatable { name, ty, setter } => {
                let mut setters = Vec::with_capacity(2);
                setters.push(parse_quote! {
                    pub fn #setter(&mut self, #setter: #ty) -> &mut Self {
                        self.#name.push(#setter);
                        self
                    }
                });
                if name != setter {
                    setters.push(parse_quote! {
                        pub fn #name(&mut self, #name: std::vec::Vec<#ty>) -> &mut Self {
                            self.#name = #name;
                            self
                        }
                    });
                }
                setters
            }
            _ => {
                let name = field.name();
                let ty = field.ty();
                vec![parse_quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(#name);
                        self
                    }
                }]
            }
        }
        .into_iter()
    }

    let method = strukt.fields.iter().flat_map(gen_setters);

    let output = parse_quote! {
        impl #struct_name {
            #(#method)*
        }
    };
    Ok(output)
}

#[cfg(test)]
#[test]
fn test_impl_builder_setters() {
    let input = create_input();
    let builder_struct: BuilderStruct = (&input).try_into().unwrap();

    let output = impl_builder_setters(&builder_struct);

    let expected: Result<ItemImpl> = Ok(parse_quote! {
        impl CommandBuilder {
            pub fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = std::option::Option::Some(executable);
                self
            }

            pub fn arg(&mut self, arg: String) -> &mut Self {
                self.args.push(arg);
                self
            }

            pub fn args(&mut self, args: std::vec::Vec<String>) -> &mut Self {
                self.args = args;
                self
            }

            pub fn env(&mut self, env: String) -> &mut Self {
                self.env.push(env);
                self
            }

            pub fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = std::option::Option::Some(current_dir);
                self
            }
        }
    });
    assert_eq!(expected.stringify(), output.stringify());
}

fn impl_builder_ctor<'a>(strukt: &'a BuilderStruct) -> Result<ItemImpl> {
    let target_name = strukt.target_name;
    let builder_name = &strukt.builder_name;
    let output = parse_quote! {
        impl #target_name {
            pub fn builder() -> #builder_name {
                <#builder_name>::default()
            }
        }
    };
    Ok(output)
}

#[cfg(test)]
#[test]
fn test_impl_builder_ctor() {
    let input = create_input();
    let builder_struct: BuilderStruct = (&input).try_into().unwrap();

    let output = impl_builder_ctor(&builder_struct);

    let expected: Result<ItemImpl> = Ok(parse_quote! {
        impl Command {
            pub fn builder() -> CommandBuilder {
                <CommandBuilder>::default()
            }
        }
    });
    assert_eq!(expected.stringify(), output.stringify());
}

fn impl_target_ctor<'a>(strukt: &'a BuilderStruct) -> Result<ItemImpl> {
    let target_name = strukt.target_name;
    let builder_name = &strukt.builder_name;

    fn gen_field_value(field: &BuilderField) -> FieldValue {
        match field {
            BuilderField::Required { name, .. } => {
                let err_msg = format!("{} must be set", name);
                parse_quote! {
                    #name: self.#name.as_ref().cloned().ok_or_else(|| #err_msg)?
                }
            }
            BuilderField::Optional { name, .. } => {
                parse_quote! {
                    #name: self.#name.as_ref().cloned()
                }
            }
            BuilderField::Repeatable { name, .. } => {
                parse_quote! {
                    #name: self.#name.clone()
                }
            }
        }
    }

    let field = strukt.fields.iter().map(gen_field_value).collect_vec();

    let output = parse_quote! {
        impl #builder_name {
            pub fn build(&self) -> std::result::Result<#target_name, std::boxed::Box<dyn std::error::Error>> {
                let target = #target_name {
                    #(#field,)*
                };
                Ok(target)
            }
        }
    };
    Ok(output)
}

#[cfg(test)]
#[test]
fn test_impl_target_ctor() {
    let input = create_input();
    let builder_struct: BuilderStruct = (&input).try_into().unwrap();

    let output = impl_target_ctor(&builder_struct);

    let expected: Result<ItemImpl> = Ok(parse_quote! {
        impl CommandBuilder {
            pub fn build(&self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
                let target = Command {
                    executable: self.executable.as_ref().cloned().ok_or_else(|| "executable must be set")?,
                    args: self.args.clone(),
                    env: self.env.clone(),
                    current_dir: self.current_dir.as_ref().cloned(),
                };
                Ok(target)
            }
        }
    });
    assert_eq!(expected.stringify(), output.stringify());
}

#[derive(Debug)]
struct BuilderStruct<'a> {
    pub target_name: &'a Ident,
    pub builder_name: Ident,
    pub fields: Vec<BuilderField<'a>>,
}

impl<'a, 'b> convert::TryFrom<&'a DeriveInput> for BuilderStruct<'b>
where
    'a: 'b,
{
    type Error = error::Error;

    fn try_from(value: &'a DeriveInput) -> Result<Self> {
        let target = &value.ident;
        if let Data::Struct(data) = &value.data {
            let fields = BuilderField::from_fields(&data.fields)?;
            let strukt = BuilderStruct {
                target_name: target,
                builder_name: format_ident!("{}Builder", target),
                fields,
            };
            Ok(strukt)
        } else {
            Err(syn::Error::new_spanned(&value, "expected struct").into())
        }
    }
}

mod field {
    use super::*;
    use proc_macro2::Span;
    use std::convert;
    use syn::{Attribute, Error, Fields, Lit, Meta, NestedMeta};
    use BuilderField::*;

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum BuilderField<'a> {
        Required {
            name: Ident,
            ty: &'a Type,
        },
        Optional {
            name: Ident,
            ty: &'a Type,
        },
        Repeatable {
            name: Ident,
            ty: &'a Type,
            setter: Ident,
        },
    }

    impl BuilderField<'_> {
        pub fn name(&self) -> &Ident {
            match self {
                Required { name, .. } => name,
                Optional { name, .. } => name,
                Repeatable { name, .. } => name,
            }
        }

        pub fn ty(&self) -> &Type {
            match self {
                Required { ty, .. } => ty,
                Optional { ty, .. } => ty,
                Repeatable { ty, .. } => ty,
            }
        }

        pub fn is_repeatable(&self) -> bool {
            matches!(self, Repeatable { .. })
        }
    }

    fn builder_attr(field: &Field) -> Option<&Attribute> {
        field.attrs.iter().find(|&attr| {
            let path = attr.path.segments.first();
            path.map(|path| &path.ident) == Some(&parse_quote! { builder })
        })
    }

    fn is_option(ty: &Type) -> Option<&Type> {
        match ty {
            Type::Path(ty) => {
                let path = ty.path.segments.first();
                let path = path.unwrap();
                if path.ident != "Option" {
                    return None;
                }
                let args = match &path.arguments {
                    PathArguments::AngleBracketed(args) => &args.args,
                    _ => return None,
                };
                let arg = match args.first() {
                    Some(GenericArgument::Type(ty)) => ty,
                    _ => return None,
                };
                Some(arg)
            }
            _ => None,
        }
    }

    fn is_repeatable(ty: &Type) -> Option<&Type> {
        match ty {
            Type::Path(ty) => {
                let path = ty.path.segments.first();
                let path = path.unwrap();
                if path.ident != "Vec" {
                    return None;
                }
                let args = match &path.arguments {
                    PathArguments::AngleBracketed(args) => &args.args,
                    _ => return None,
                };
                let arg = match args.first() {
                    Some(GenericArgument::Type(ty)) => ty,
                    _ => return None,
                };
                Some(arg)
            }
            _ => None,
        }
    }

    impl<'f, 'a> convert::TryFrom<&'f syn::Field> for BuilderField<'a>
    where
        'f: 'a,
    {
        type Error = error::Error;

        fn try_from(field: &'f Field) -> Result<Self> {
            let name = field.ident.clone().expect("expected named field");

            fn repeatable_setter_name(meta: &Meta) -> Result<Ident> {
                let name = match meta {
                    Meta::List(list) => match list.nested.first() {
                        Some(NestedMeta::Meta(Meta::NameValue(name_value))) => {
                            if name_value.path.is_ident("each") {
                                match &name_value.lit {
                                    Lit::Str(value) => Some(value.value()),
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                    _ => None,
                };
                name.map(|name| Ident::new(&name, Span::call_site()))
                    .ok_or_else(|| {
                        Error::new_spanned(meta, r#"expected `builder(each = "...")`"#).into()
                    })
            }

            let output = if let Some(attr) = builder_attr(&field) {
                let meta = attr.parse_meta()?;

                let setter = repeatable_setter_name(&meta)?;

                let ty = is_repeatable(&field.ty);
                let ty = ty.ok_or_else(|| Error::new_spanned(ty, "expected `Vec<_>`"))?;

                Repeatable { name, ty, setter }
            } else {
                match is_option(&field.ty) {
                    Some(inner_type) => Optional {
                        name,
                        ty: inner_type,
                    },
                    None => Required {
                        name,
                        ty: &field.ty,
                    },
                }
            };

            Ok(output)
        }
    }

    impl BuilderField<'_> {
        pub fn from_fields<'a, 'b>(fields: &'a Fields) -> Result<Vec<BuilderField<'b>>>
        where
            'a: 'b,
        {
            let fields: Vec<BuilderField> = fields
                .into_iter()
                .map(|x| x.try_into())
                .collect::<Result<_>>()?;
            Ok(fields)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::convert::TryInto;
        use syn::{parse_quote, FieldsNamed};

        #[test]
        fn test_from_required_field() {
            let input: FieldsNamed = parse_quote! {
                {
                    name: String,
                }
            };
            let field = input.named.first().unwrap();

            let output: BuilderField = field.try_into().unwrap();

            assert_eq!(
                BuilderField::Required {
                    name: parse_quote! { name },
                    ty: &parse_quote! { String },
                },
                output
            );
        }

        #[test]
        fn test_from_optional_field() {
            let input: FieldsNamed = parse_quote! {
                {
                    name: Option<String>,
                }
            };
            let field = input.named.first().unwrap();

            let output: BuilderField = field.try_into().unwrap();

            assert_eq!(
                BuilderField::Optional {
                    name: parse_quote! { name },
                    ty: &parse_quote! { String },
                },
                output
            );
        }

        #[test]
        fn test_from_repeatable_field() {
            let input: FieldsNamed = parse_quote! {
                {
                    #[builder(each = "arg")]
                    args: Vec<String>,
                }
            };

            let field = input.named.first().unwrap();
            let output: BuilderField = field.try_into().unwrap();

            assert_eq!(
                BuilderField::Repeatable {
                    name: parse_quote! { args },
                    ty: &parse_quote! { String },
                    setter: parse_quote! { arg }
                },
                output
            );
        }
    }
}

mod error {
    use super::*;

    #[cfg_attr(all(test, debug_assertions), derive(Debug))]
    pub struct Error(syn::Error);

    impl From<syn::Error> for Error {
        fn from(value: syn::Error) -> Self {
            Self(value)
        }
    }

    impl ops::Deref for Error {
        type Target = syn::Error;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[cfg(all(test, debug_assertions))]
    impl PartialEq for Error {
        fn eq(&self, other: &Self) -> bool {
            self.0.to_string() == other.0.to_string()
        }
    }

    #[cfg(all(test, debug_assertions))]
    impl Eq for Error {}
}

#[cfg(test)]
use stringify::*;

#[cfg(test)]
mod stringify {
    use super::*;
    use quote::ToTokens;

    fn stringify_tokens(source: &impl ToTokens) -> String {
        let mut tokens = TokenStream::new();
        source.to_tokens(&mut tokens);
        tokens.to_string()
    }

    pub trait Stringify {
        fn stringify(&self) -> String;
    }

    impl<T> Stringify for T
    where
        T: ToTokens,
    {
        fn stringify(&self) -> String {
            stringify_tokens(self)
        }
    }

    pub trait StringifyResult {
        fn stringify(self) -> Result<String>;
    }

    impl<T> StringifyResult for Result<T>
    where
        T: Stringify,
    {
        fn stringify(self) -> Result<String> {
            self.map(|x| x.stringify())
        }
    }
}
