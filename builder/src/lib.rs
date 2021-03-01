use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, *};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive2(input)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn derive2(input: DeriveInput) -> Result<TokenStream> {
    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => fields,
            Fields::Unnamed(_fields) => {
                return Err(Error::new(
                    input.span(),
                    "Builder can not be derived at tuples",
                ))
            }
            Fields::Unit => {
                return Err(Error::new(
                    input.span(),
                    "Builder is unnecessary for unit struct",
                ))
            }
        },
        _ => {
            return Err(Error::new(
                input.span(),
                "Builder can only be derived at struct",
            ))
        }
    };
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let builder_fields = gen_builder_fields(fields)?;
    let builder_setters = gen_builder_setters(fields)?;
    let build_fields = gen_build_fields(fields)?;
    Ok(quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name::default()
            }
        }
        #[derive(Default)]
        pub struct #builder_name {
            #builder_fields
        }
        impl #builder_name {
            #builder_setters
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name { #build_fields })
            }
        }
    })
}

fn gen_builder_fields(fields: &FieldsNamed) -> Result<TokenStream> {
    let mut recurse = vec![];
    for f in fields.named.iter() {
        let name = &f.ident;
        let attr = BuilderAttrs::parse_from(&f.attrs)?;
        let expanded = if attr.each.is_some() {
            let ty = &f.ty;
            assert!(
                type_as_vec(ty).is_some(),
                "field with 'each' attribute should has type 'Vec'"
            );
            quote_spanned! { f.span() =>
                #name: #ty,
            }
        } else {
            let ty = type_as_option(&f.ty).unwrap_or(&f.ty);
            quote_spanned! { f.span() =>
                #name: std::option::Option<#ty>,
            }
        };
        recurse.push(expanded);
    }
    Ok(quote! { #(#recurse)* })
}

fn gen_builder_setters(fields: &FieldsNamed) -> Result<TokenStream> {
    let mut recurse = vec![];
    for f in fields.named.iter() {
        let name = &f.ident;
        let attr = BuilderAttrs::parse_from(&f.attrs)?;
        let expanded = if let Some(each) = attr.each {
            let each = format_ident!("{}", each);
            let ty_in_vec = type_as_vec(&f.ty).unwrap();
            quote_spanned! { f.span() =>
                fn #each(&mut self, #each: #ty_in_vec) -> &mut Self {
                    self.#name.push(#each);
                    self
                }
            }
        } else {
            let ty = type_as_option(&f.ty).unwrap_or(&f.ty);
            quote_spanned! { f.span() =>
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        };
        recurse.push(expanded);
    }
    Ok(quote! { #(#recurse)* })
}

fn gen_build_fields(fields: &FieldsNamed) -> Result<TokenStream> {
    let mut recurse = vec![];
    for f in fields.named.iter() {
        let name = &f.ident;
        let attr = BuilderAttrs::parse_from(&f.attrs)?;
        let expanded = if attr.each.is_some() || type_as_option(&f.ty).is_some() {
            quote_spanned! { f.span() =>
                #name: std::mem::take(&mut self.#name),
            }
        } else {
            quote_spanned! { f.span() =>
                #name: self.#name.take().ok_or(concat!("uninitialized field: ", stringify!(#name)))?,
            }
        };
        recurse.push(expanded);
    }
    Ok(quote! { #(#recurse)* })
}

/// If the type is literally as `Option<T>`, then return `Some(T)`, otherwise `None`.
fn type_as_option(ty: &Type) -> Option<&Type> {
    match_type(ty, "Option")
}

/// If the type is literally as `Vec<T>`, then return `Some(T)`, otherwise `None`.
fn type_as_vec(ty: &Type) -> Option<&Type> {
    match_type(ty, "Vec")
}

/// If the type is literally as `ty_name<T>`, then return `Some(T)`, otherwise `None`.
fn match_type<'a>(ty: &'a Type, ty_name: &str) -> Option<&'a Type> {
    match ty {
        Type::Path(TypePath { qself: None, path }) => match path.segments.first() {
            Some(ps) if ps.ident == ty_name => match &ps.arguments {
                PathArguments::AngleBracketed(args) => match args.args.first() {
                    Some(GenericArgument::Type(ty)) => Some(ty),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            _ => None,
        },
        _ => None,
    }
}

/// Attributes of field for builder.
#[derive(Default, FromMeta)]
#[darling(default)]
struct BuilderAttrs {
    each: Option<String>,
}

impl BuilderAttrs {
    fn parse_from(attrs: &[Attribute]) -> Result<Self> {
        let attr = attrs.iter().find(|attr| {
            if let Some(ps) = attr.path.segments.first() {
                ps.ident == "builder"
            } else {
                false
            }
        });
        let attr = match attr {
            Some(x) => x,
            None => return Ok(Self::default()),
        };
        let meta = attr.parse_meta().unwrap();
        let ret = Self::from_meta(&meta)
            .map_err(|_| Error::new(attr.span(), "expected `builder(each = \"...\")`"))?;
        Ok(ret)
    }
}
