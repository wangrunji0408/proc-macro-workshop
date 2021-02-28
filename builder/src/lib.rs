use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, *};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => fields,
            Fields::Unnamed(_fields) => panic!("Builder can not be derived at tuples"),
            Fields::Unit => panic!("Builder is unnecessary for unit struct"),
        },
        _ => panic!("Builder can only be derived at struct"),
    };
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let builder_fields = gen_builder_fields(fields);
    let builder_setters = gen_builder_setters(fields);
    let build_fields = gen_build_fields(fields);
    let expanded = quote! {
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
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name { #build_fields })
            }
        }
    };
    expanded.into()
}

fn gen_builder_fields(fields: &FieldsNamed) -> TokenStream {
    let recurse = fields.named.iter().map(|f| {
        let name = &f.ident;
        let attr = BuilderAttrs::parse_from(&f.attrs);
        if attr.each.is_some() {
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
                #name: Option<#ty>,
            }
        }
    });
    quote! { #(#recurse)* }
}

fn gen_builder_setters(fields: &FieldsNamed) -> TokenStream {
    let recurse = fields.named.iter().map(|f| {
        let name = &f.ident;
        let attr = BuilderAttrs::parse_from(&f.attrs);
        if let Some(each) = attr.each {
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
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });
    quote! { #(#recurse)* }
}

fn gen_build_fields(fields: &FieldsNamed) -> TokenStream {
    let recurse = fields.named.iter().map(|f| {
        let name = &f.ident;
        let attr = BuilderAttrs::parse_from(&f.attrs);
        if attr.each.is_some() || type_as_option(&f.ty).is_some() {
            quote_spanned! { f.span() =>
                #name: std::mem::take(&mut self.#name),
            }
        } else {
            quote_spanned! { f.span() =>
                #name: self.#name.take().ok_or(concat!("uninitialized field: ", stringify!(#name)))?,
            }
        }
    });
    quote! { #(#recurse)* }
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
struct BuilderAttrs {
    each: Option<String>,
}

impl BuilderAttrs {
    fn parse_from(attrs: &[Attribute]) -> Self {
        let mut ret = BuilderAttrs { each: None };
        let attr = attrs.iter().find(|attr| {
            if let Some(ps) = attr.path.segments.first() {
                ps.ident == "builder"
            } else {
                false
            }
        });
        let attr = match attr {
            Some(x) => x,
            None => return ret,
        };
        let list = match attr.parse_meta().unwrap() {
            Meta::List(list) => list,
            _ => panic!("invalid attribute format"),
        };
        for meta in list.nested {
            match meta {
                NestedMeta::Meta(meta) => match meta {
                    Meta::NameValue(nv) => {
                        let ps = nv.path.segments.first().unwrap();
                        if ps.ident == "each" {
                            match nv.lit {
                                Lit::Str(s) => ret.each = Some(s.value()),
                                _ => {
                                    panic!("the type of 'each' attribute should be string literal")
                                }
                            }
                        }
                    }
                    _ => panic!("invalid attribute format"),
                },
                _ => panic!("invalid attribute format"),
            }
        }
        ret
    }
}
