use proc_macro::TokenStream;
use syn::DeriveInput;

#[cfg(test)]
mod tests;

mod common;
mod derive_parser;
mod derive_subcommand;
mod derive_value_enum;

#[proc_macro_derive(Parser, attributes(arg, command))]
pub fn derive_parser(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_parser::expand(input).into()
}

#[proc_macro_derive(Subcommand, attributes(arg, command))]
pub fn derive_subcommand(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_subcommand::expand(input).into()
}

#[proc_macro_derive(ValueEnum)]
pub fn derive_value_enum(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_value_enum::expand(input).into()
}
