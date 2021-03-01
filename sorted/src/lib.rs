use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, *};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let input = parse_macro_input!(input as Item);
    sorted2(args.into(), input)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn sorted2(args: TokenStream, input: Item) -> Result<TokenStream> {
    let _ = args;

    match &input {
        Item::Enum(_item) => {}
        _ => return Err(Error::new(args.span(), "expected enum or match expression")),
    }

    Ok(quote! { #input })
}
