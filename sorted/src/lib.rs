use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, *};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Item);
    sorted2(args.into(), &input)
        .unwrap_or_else(|e| {
            let ce = e.into_compile_error();
            quote! { #input #ce }
        })
        .into()
}

fn sorted2(args: TokenStream, input: &Item) -> Result<TokenStream> {
    let _ = args;

    match input {
        Item::Enum(item) => {
            for i in 1..item.variants.len() {
                let last_ident = &item.variants[i - 1].ident;
                let ident = &item.variants[i].ident;
                if ident <= last_ident {
                    let before_idx = (0..i)
                        .rfind(|&j| item.variants[j].ident < *ident)
                        .unwrap_or(0);
                    let before_ident = &item.variants[before_idx].ident;
                    return Err(Error::new(
                        item.variants[i].span(),
                        format!("{} should sort before {}", ident, before_ident),
                    ));
                }
            }
        }
        _ => return Err(Error::new(args.span(), "expected enum or match expression")),
    }

    Ok(quote! { #input })
}
