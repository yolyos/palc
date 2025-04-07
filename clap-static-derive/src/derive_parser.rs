use darling::FromDeriveInput;
use darling::util::Override;
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
            impl __rt::Parser for #struct_name {}

            #[automatically_derived]
            impl __rt::ArgsInternal for #struct_name {
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
    fields: Vec<FieldInfo>,
    match_named_arms: TokenStream,
    handle_unnamed: TokenStream,
    subcommand: Option<SubcommandInfo>,
}

struct FieldInfo {
    name: Ident,
    state_ty: TokenStream,
    display_name: String,
    needs_unwrap: bool,
}

struct SubcommandInfo {
    field: Ident,
    ty: TokenStream,
    needs_unwrap: bool,
}

pub fn expand_state_def_impl(
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &[ArgField],
) -> syn::Result<ParserStateDefImpl> {
    let mut out = ParserStateDefImpl {
        state_name,
        output_ty,
        output_ctor: None,
        fields: Vec::with_capacity(input_fields.len()),
        match_named_arms: TokenStream::new(),
        handle_unnamed: TokenStream::new(),
        subcommand: None,
    };

    for field in input_fields {
        let field_name = field.ident.clone().expect("checked by darling");
        let field_name_str = field_name.to_string();
        let field_ty = &field.ty;

        // This is unused for subcommands.
        let (state_ty, parser, needs_unwrap) = match field.arg_ty_kind() {
            ArgTyKind::Bool => {
                if let ArgOrCommand::None = &field.attrs {
                    return Err(syn::Error::new(
                        field_name.span(),
                        "positional arguments cannot be `bool`",
                    ));
                }
                (quote! { bool }, TokenStream::new(), false)
            }
            ArgTyKind::Option(subty) => (
                quote! { #field_ty },
                quote! { <#subty as __rt::FromValue>::try_from_value },
                false,
            ),
            ArgTyKind::Convert => (
                quote! { __rt::Option<#field_ty> },
                quote! { <#field_ty as __rt::FromValue>::try_from_value },
                true,
            ),
        };

        let mut display_name = String::new();

        match &field.attrs {
            // Unnamed (positional) arguments.
            ArgOrCommand::None => {
                display_name = heck::AsShoutySnakeCase(field_name_str).to_string();
                out.handle_unnamed.extend(quote! {
                    if self.#field_name.is_none() {
                        self.#field_name = __rt::Some(#parser(__rt::take_arg(__arg))?);
                        return __rt::Ok(());
                    }
                });
            }
            // Named arguments.
            ArgOrCommand::Arg(Arg { long, short }) => {
                let mut pats = <Punctuated<String, Token![|]>>::new();

                if let Some(ch) = short {
                    let ch = ch
                        .clone()
                        .unwrap_or_else(|| field_name_str.chars().next().expect("must have name"));
                    display_name = format!("-{ch}");
                    pats.push(ch.to_string());
                }
                if let Some(name) = long {
                    let long_name = match name {
                        Override::Inherit => format!("--{}", heck::AsKebabCase(&field_name_str)),
                        Override::Explicit(s) => format!("--{s}"),
                    };
                    // Prefer long names for display.
                    display_name = long_name.clone();
                    pats.push(long_name);
                }
                assert!(!pats.is_empty(), "validated by darling");

                let handler = match field.arg_ty_kind() {
                    ArgTyKind::Bool => quote! {{
                        self.#field_name = true;
                        __rt::Ok(__rt::FeedNamedOk::Arg0)
                    }},
                    ArgTyKind::Option(_) | ArgTyKind::Convert => quote! {
                        __rt::Ok(__rt::FeedNamedOk::Arg1(|__s, __v| {
                            __s.#field_name = Some(#parser(__v)?);
                            __rt::Ok(())
                        }))
                    },
                };

                out.match_named_arms.extend(quote! { #pats => #handler, });
            }
            // Sub-struct injection.
            ArgOrCommand::Command(cmd) => {
                assert!(cmd.subcommand, "validated");
                if out.subcommand.is_some() {
                    return Err(Error::new(
                        field_name.span(),
                        "duplicated `#[command(subcommand)]`",
                    ));
                }
                out.subcommand = Some(if let Some(arg_ty) = strip_ty_ctor(field_ty, TY_OPTION) {
                    SubcommandInfo {
                        field: field_name.clone(),
                        ty: quote! { #arg_ty },
                        needs_unwrap: false,
                    }
                } else {
                    SubcommandInfo {
                        field: field_name.clone(),
                        ty: quote! { #field_ty },
                        needs_unwrap: true,
                    }
                });
                continue;
            }
        }

        out.fields.push(FieldInfo {
            name: field_name,
            state_ty,
            display_name,
            needs_unwrap,
        });
    }

    Ok(out)
}

impl ToTokens for ParserStateDefImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            state_name,
            output_ty,
            output_ctor,
            fields,
            match_named_arms,
            handle_unnamed,
            subcommand,
        } = self;
        let output_ctor = output_ctor.as_ref().unwrap_or(&self.output_ty);

        let mut check_missing =
            fields
                .iter()
                .filter_map(|f| {
                    let name = &f.name;
                    let display_name = &f.display_name;
                    f.needs_unwrap.then(|| quote! {
                        if self.#name.is_none() {
                            return __rt::Err(__rt::Error::MissingRequiredArgument(__rt::str::to_owned(#display_name)));
                        }
                    })
                })
                .collect::<TokenStream>();

        let field_names = fields.iter().map(|f| &f.name).collect::<Vec<_>>();
        let field_tys = fields.iter().map(|f| &f.state_ty);
        let field_finishes = fields.iter().map(|f| {
            let name = &f.name;
            if f.needs_unwrap {
                quote! { self.#name.unwrap() }
            } else {
                quote! { self.#name }
            }
        });

        let (subcommand_ty, subcommand_finish) = match subcommand {
            None => (quote! { __rt::Infallible }, TokenStream::new()),
            Some(SubcommandInfo {
                field,
                ty,
                needs_unwrap,
            }) => {
                if *needs_unwrap {
                    check_missing.extend(quote! {
                        if __subcmd.is_none() {
                            return __rt::Err(__rt::Error::MissingRequiredSubcommand);
                        }
                    });
                    (ty.clone(), quote! { #field : __subcmd.unwrap() })
                } else {
                    (ty.clone(), quote! { #field : __subcmd })
                }
            }
        };

        tokens.extend(quote! {
            #[derive(Default)]
            struct #state_name {
                #(#field_names : #field_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;
                type Subcommand = #subcommand_ty;

                fn feed_named(&mut self, __name: &__rt::str) -> __rt::FeedNamed<Self> {
                    match __name {
                        #match_named_arms
                        _ => __rt::Err(__rt::None)
                    }
                }

                fn feed_unnamed(&mut self, __arg: &mut __rt::OsString) -> __rt::FeedUnnamed {
                    #handle_unnamed
                    __rt::Err(__rt::None)
                }

                fn finish(self, __subcmd: __rt::Option<Self::Subcommand>)
                    -> __rt::Result<Self::Output, __rt::Error>
                {
                    #check_missing
                    __rt::Ok(#output_ctor {
                        #(#field_names : #field_finishes,)*
                        #subcommand_finish
                    })
                }
            }
        });
    }
}
