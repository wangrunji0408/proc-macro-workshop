use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields, GenericArgument, PathArguments,
    Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let builder_fields = gen_builder_fields(&input.data);
    let builder_setters = gen_builder_setters(&input.data);
    let build_fields = gen_build_fields(&input.data);
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

fn gen_builder_fields(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = type_as_option(&f.ty).unwrap_or(&f.ty);
                    quote_spanned! { f.span() =>
                        #name: Option<#ty>,
                    }
                });
                quote! { #(#recurse)* }
            }
            Fields::Unnamed(_fields) => panic!("Builder can not be derived at tuples"),
            Fields::Unit => quote! {},
        },
        _ => panic!("Builder can only be derived at struct"),
    }
}

fn gen_builder_setters(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = type_as_option(&f.ty).unwrap_or(&f.ty);
                    quote_spanned! { f.span() =>
                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }
                });
                quote! { #(#recurse)* }
            }
            Fields::Unnamed(_fields) => panic!("Builder can not be derived at tuples"),
            Fields::Unit => quote! {},
        },
        _ => panic!("Builder can only be derived at struct"),
    }
}

fn gen_build_fields(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    if type_as_option(&f.ty).is_some() {
                        quote_spanned! { f.span() =>
                            #name: self.#name.take(),
                        }
                    } else {
                        quote_spanned! { f.span() =>
                            #name: self.#name.take().ok_or(concat!("uninitialized field: ", stringify!(#name)))?,
                        }
                    }
                });
                quote! { #(#recurse)* }
            }
            Fields::Unnamed(_fields) => panic!("Builder can not be derived at tuples"),
            Fields::Unit => quote! {},
        },
        _ => panic!("Builder can only be derived at struct"),
    }
}

/// If the type is literally as `Option<T>`, then return `Some(T)`, otherwise `None`.
fn type_as_option(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(TypePath { qself: None, path }) => match path.segments.first() {
            Some(ps) if ps.ident == "Option" => match &ps.arguments {
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
