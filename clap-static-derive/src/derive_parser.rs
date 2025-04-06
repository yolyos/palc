use darling::FromDeriveInput;
use darling::util::Override;
use heck::AsKebabCase;
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::punctuated::Punctuated;
use syn::{DeriveInput, Error, Generics, Ident, Token};

use crate::common::{
    Arg, ArgField, ArgOrCommand, ArgTyKind, TY_OPTION, strip_ty_ctor, wrap_anon_item,
};

#[derive(FromDeriveInput)]
#[darling(supports(struct_named))]
pub struct ParserItemDef {
    pub ident: Ident,
    pub generics: Generics,
    pub data: darling::ast::Data<(), ArgField>,
}

pub fn expand(input: DeriveInput) -> TokenStream {
    match ParserItemDef::from_derive_input(&input) {
        Err(err) => err.write_errors(),
        Ok(def) => match expand_parser_impl(def) {
            Ok(tts) => wrap_anon_item(tts),
            Err(err) => err.into_compile_error(),
        },
    }
}

pub struct ParserImpl {
    state: ParserStateDefImpl,
}

pub fn expand_parser_impl(def: ParserItemDef) -> syn::Result<ParserImpl> {
    let fields = def.data.take_struct().expect("checked by darling").fields;

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let state_name = format_ident!("{}State", def.ident);
    let struct_name = def.ident.to_token_stream();
    let state = expand_state_def_impl(state_name, struct_name, &fields)?;
    Ok(ParserImpl { state })
}

impl ToTokens for ParserImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let state = &self.state;
        let struct_name = &state.output_ty;
        let state_name = &state.state_name;

        tokens.extend(quote! {
            #[automatically_derived]
            impl rt::Parser for #struct_name {}

            #[automatically_derived]
            impl rt::__internal::ArgsInternal for #struct_name {
                type __State = #state_name;
            }

            #state
        });
    }
}

pub struct ParserStateDefImpl {
    pub state_name: Ident,
    pub output_ty: TokenStream,
    pub output_ctor: Option<TokenStream>,
    fields: Vec<Ident>,
    field_tys: Vec<TokenStream>,
    field_finishes: Vec<TokenStream>,
    match_named_arms: TokenStream,
    handle_unnamed: TokenStream,
    subcommand_ty: TokenStream,
    subcommand_finish: Option<TokenStream>,
}

pub fn expand_state_def_impl(
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &[ArgField],
) -> syn::Result<ParserStateDefImpl> {
    let mut state_fields = Vec::new();
    let mut state_field_tys = Vec::new();
    let mut field_finishes = Vec::new();

    let mut subcommand_ty = quote!(rt::__internal::Infallible);
    let mut subcommand_finish = None;

    let mut match_named_arms = TokenStream::new();
    let mut handle_unnamed = TokenStream::new();

    for field in input_fields {
        let field_name = field.ident.clone().expect("checked by darling");
        let field_name_str = field_name.to_string();
        let field_name_lit = syn::LitStr::new(&field_name_str, field_name.span());
        let field_ty = &field.ty;

        // This is unused for subcommands.
        let (state_field_ty, parser, finish) = match field.arg_ty_kind() {
            ArgTyKind::Bool => {
                if let ArgOrCommand::None = &field.attrs {
                    return Err(syn::Error::new(
                        field_name.span(),
                        "positional arguments cannot be `bool`",
                    ));
                }
                (
                    quote! { bool },
                    TokenStream::new(),
                    quote! { self.#field_name },
                )
            }
            ArgTyKind::Option(subty) => (
                quote! { #field_ty },
                quote! { <#subty as rt::FromValue>::try_from_value },
                quote! { self.#field_name },
            ),
            ArgTyKind::Convert => (
                quote!(Option<#field_ty>),
                quote! { <#field_ty as rt::FromValue>::try_from_value },
                quote! { rt::__internal::require_arg(self.#field_name, #field_name_lit)? },
            ),
        };

        match &field.attrs {
            // Unnamed (positional) arguments.
            ArgOrCommand::None => {
                handle_unnamed.extend(quote! {
                    if let rt::__internal::None = self.#field_name {
                        self.#field_name = rt::__internal::Some(#parser(rt::__internal::take_arg(__arg))?);
                        return rt::__internal::Ok(());
                    }
                });
                state_fields.push(field_name);
                state_field_tys.push(state_field_ty);
                field_finishes.push(finish);
            }
            // Named arguments.
            ArgOrCommand::Arg(Arg { long, short }) => {
                let mut pats = <Punctuated<String, Token![|]>>::new();
                if let Some(name) = long {
                    let pat = match name {
                        Override::Inherit => format!("--{}", AsKebabCase(&field_name_str)),
                        Override::Explicit(s) => format!("--{s}"),
                    };
                    pats.push(pat);
                }
                if let Some(ch) = short {
                    let ch = ch
                        .clone()
                        .unwrap_or_else(|| field_name_str.chars().next().expect("must have name"));
                    pats.push(ch.to_string());
                }
                assert!(!pats.is_empty(), "validated by darling");

                let handler = match field.arg_ty_kind() {
                    ArgTyKind::Bool => quote! {{
                        self.#field_name = true;
                        rt::__internal::Ok(rt::__internal::FeedNamedOk::Arg0)
                    }},
                    ArgTyKind::Option(_) | ArgTyKind::Convert => quote! {
                        rt::__internal::Ok(rt::__internal::FeedNamedOk::Arg1(|__s, __v| {
                            __s.#field_name = Some(#parser(__v)?);
                            rt::__internal::Ok(())
                        }))
                    },
                };

                state_fields.push(field_name);
                state_field_tys.push(state_field_ty);
                match_named_arms.extend(quote! { #pats => #handler, });
                field_finishes.push(finish);
            }
            // Sub-struct injection.
            ArgOrCommand::Command(cmd) => {
                assert!(cmd.subcommand, "validated");
                if subcommand_finish.is_some() {
                    return Err(Error::new(
                        field_name.span(),
                        "duplicated `#[command(subcommand)]`",
                    ));
                }
                if let Some(arg_ty) = strip_ty_ctor(field_ty, TY_OPTION) {
                    subcommand_ty = quote! { #arg_ty };
                    subcommand_finish = Some(quote! { #field_name: __subcmd });
                } else {
                    subcommand_ty = quote! { #field_ty };
                    subcommand_finish =
                        Some(quote! { #field_name: rt::__internal::require_subcmd(__subcmd)? });
                }
            }
        }
    }

    Ok(ParserStateDefImpl {
        state_name,
        output_ty,
        output_ctor: None,
        fields: state_fields,
        field_tys: state_field_tys,
        field_finishes,
        match_named_arms,
        handle_unnamed,
        subcommand_ty,
        subcommand_finish,
    })
}

impl ToTokens for ParserStateDefImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            state_name: name,
            output_ty,
            output_ctor,
            fields,
            field_tys,
            field_finishes,
            match_named_arms,
            handle_unnamed,
            subcommand_ty,
            subcommand_finish,
        } = self;
        let output_ctor = output_ctor.as_ref().unwrap_or(&self.output_ty);

        tokens.extend(quote! {
            #[derive(Default)]
            struct #name {
                #(#fields : #field_tys,)*
            }

            #[automatically_derived]
            impl rt::__internal::ParserState for #name {
                type Output = #output_ty;
                type Subcommand = #subcommand_ty;

                fn feed_named(&mut self, __name: &rt::__internal::str) -> rt::__internal::FeedNamed<Self> {
                    match __name {
                        #match_named_arms
                        _ => rt::__internal::Err(rt::__internal::None)
                    }
                }

                fn feed_unnamed(&mut self, __arg: &mut rt::__internal::OsString) -> rt::__internal::FeedUnnamed {
                    #handle_unnamed
                    rt::__internal::Err(rt::__internal::None)
                }

                fn finish(self, __subcmd: rt::__internal::Option<Self::Subcommand>)
                    -> rt::__internal::Result<Self::Output, rt::Error>
                {
                    rt::__internal::Ok(#output_ctor {
                        #(#fields : #field_finishes,)*
                        #subcommand_finish
                    })
                }
            }
        });
    }
}
