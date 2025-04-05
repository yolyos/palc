use proc_macro::TokenStream;
use syn::DeriveInput;

#[cfg(test)]
mod tests;

mod common;
mod derive_parser;

#[proc_macro_derive(Parser, attributes(arg, command))]
pub fn derive_parser(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_parser::expand(input).into()
}
