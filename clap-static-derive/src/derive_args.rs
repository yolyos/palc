use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{DeriveInput, Error, Ident, Visibility};

use crate::common::{
    ArgOrCommand, ArgTyKind, ArgsCommandMeta, Doc, ErrorCollector, Override, TY_OPTION,
    TopCommandMeta, parse_args_attrs, strip_ty_ctor, wrap_anon_item,
};

pub fn expand(input: DeriveInput, is_parser: bool) -> TokenStream {
    let mut tts = match expand_args_impl(&input, is_parser) {
        Ok(out) => return wrap_anon_item(out),
        Err(err) => err.into_compile_error(),
    };

    // Error fallback impl.
    let name = &input.ident;
    tts.extend(wrap_anon_item(quote! {
        #[automatically_derived]
        impl __rt::Parser for #name {}

        #[automatically_derived]
        impl __rt::Args for #name {}

        #[automatically_derived]
        impl __rt::ArgsInternal for #name {
            type __State = __rt::FallbackState<#name>;
        }
    }));
    tts
}

/// For `derive({Args,Parser})`.
pub struct ArgsImpl<'i> {
    is_parser: bool,
    state: ParserStateDefImpl<'i>,
}

fn expand_args_impl(def: &DeriveInput, is_parser: bool) -> syn::Result<ArgsImpl<'_>> {
    let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(fields),
        ..
    }) = &def.data
    else {
        return Err(syn::Error::new(
            Span::call_site(),
            "derive(Args) and derive(Parser) can only be used on named structs",
        ));
    };

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(Error::new(
            def.ident.span(),
            "TODO: generics are not supported yet",
        ));
    }

    let mut errs = ErrorCollector::default();
    let cmd_meta = errs.collect(TopCommandMeta::parse_attrs(&def.attrs));

    let state_name = format_ident!("{}State", def.ident);
    let struct_name = def.ident.to_token_stream();
    let state = errs.collect(expand_state_def_impl(
        &def.vis,
        cmd_meta,
        state_name,
        struct_name,
        fields,
    ));

    errs.finish()?;
    Ok(ArgsImpl {
        is_parser,
        state: state.unwrap(),
    })
}

impl ToTokens for ArgsImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { is_parser, state } = self;
        let struct_name = &state.output_ty;
        let state_name = &state.state_name;

        if *is_parser {
            tokens.extend(quote! {
                #[automatically_derived]
                impl __rt::Parser for #struct_name {}
            });
        }

        tokens.extend(quote! {
            #[automatically_derived]
            impl __rt::Args for #struct_name {}

            #[automatically_derived]
            impl __rt::ArgsInternal for #struct_name {
                type __State = #state_name;
            }

            #state
        });
    }
}

pub struct ParserStateDefImpl<'i> {
    pub vis: &'i Visibility,
    pub state_name: Ident,
    pub output_ty: TokenStream,
    pub output_ctor: Option<TokenStream>,

    named_fields: Vec<FieldInfo<'i>>,
    unnamed_fields: Vec<FieldInfo<'i>>,
    catchall_field: Option<CatchallFieldInfo<'i>>,
    last_field: Option<FieldInfo<'i>>,
    flatten_fields: Vec<FlattenFieldInfo<'i>>,
    cmd_attrs: Option<TopCommandMeta>,
}

struct FieldInfo<'i> {
    ident: &'i Ident,
    doc: Doc,
    kind: FieldKind,
    /// The type used for parser inference, with `Option`/`Vec` stripped.
    effective_ty: &'i syn::Type,
    /// Matching names for named arguments. Empty for unnamed arguments.
    arg_names: Vec<String>,
    /// Display string for the name, eg. `--config-file`, `-c`, or `CONFIG_FILE` for unnamed.
    name_display: String,
    /// Display string for the value, eg. `CONFIG_FILE`.
    value_display: String,
    require_eq: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldKind {
    BoolSetTrue,
    Option,
    UnwrapOption,

    Vec,
    OptionVec,
}

enum CatchallFieldInfo<'i> {
    Subcommand {
        ident: &'i Ident,
        effective_ty: &'i syn::Type,
        optional: bool,
    },
    VecLike {
        greedy: bool,
        field: FieldInfo<'i>,
    },
}

impl<'i> CatchallFieldInfo<'i> {
    fn ident(&self) -> &'i Ident {
        match self {
            Self::Subcommand { ident, .. } => ident,
            Self::VecLike { field, .. } => field.ident,
        }
    }
}

struct FlattenFieldInfo<'i> {
    ident: &'i Ident,
    effective_ty: &'i syn::Type,
}

fn value_info(ty: &syn::Type) -> TokenStream {
    quote_spanned! {ty.span()=> __rt::arg_value_info!(#ty) }
}

fn value_parsed(ty: &syn::Type) -> TokenStream {
    let value_info = value_info(ty);
    quote_spanned! {ty.span()=>
        __rt::ArgValueInfo::parse(#value_info, __rt::take_arg(__arg))?
    }
}

pub fn expand_state_def_impl<'i>(
    vis: &'i Visibility,
    cmd_attrs: Option<TopCommandMeta>,
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &'i syn::FieldsNamed,
) -> syn::Result<ParserStateDefImpl<'i>> {
    let field_attrs = parse_args_attrs(input_fields)?;
    let input_fields = &input_fields.named;

    let mut errs = ErrorCollector::default();

    let mut out = ParserStateDefImpl {
        vis,
        state_name,
        output_ty,
        output_ctor: None,
        named_fields: Vec::with_capacity(input_fields.len()),
        unnamed_fields: Vec::with_capacity(input_fields.len()),
        flatten_fields: Vec::with_capacity(input_fields.len()),
        catchall_field: None,
        last_field: None,
        cmd_attrs,
    };

    for (field, attrs) in input_fields.iter().zip(field_attrs) {
        let ident = field.ident.as_ref().expect("named struct");
        let ident_str = ident.to_string();
        let value_display = heck::AsShoutySnekCase(&ident_str).to_string();

        let arg = match attrs {
            ArgOrCommand::Arg(arg) => arg,
            ArgOrCommand::Command(ArgsCommandMeta::Subcommand) => {
                let (optional, effective_ty) = match strip_ty_ctor(&field.ty, TY_OPTION) {
                    Some(subty) => (true, subty),
                    None => (false, &field.ty),
                };
                let info = CatchallFieldInfo::Subcommand {
                    ident,
                    effective_ty,
                    optional,
                };
                if let Some(prev) = out.catchall_field.replace(info) {
                    errs.push(Error::new(
                        ident.span(),
                        "duplicated variable-length arguments",
                    ));
                    errs.push(Error::new(prev.ident().span(), "previously defined here"));
                }
                continue;
            }
            ArgOrCommand::Command(ArgsCommandMeta::Flatten) => {
                out.flatten_fields.push(FlattenFieldInfo {
                    ident,
                    effective_ty: &field.ty,
                });
                continue;
            }
        };

        let (kind, effective_ty) = match ArgTyKind::of(&field.ty) {
            ArgTyKind::Bool => (FieldKind::BoolSetTrue, &field.ty),
            ArgTyKind::Vec(subty) => (FieldKind::Vec, subty),
            ArgTyKind::OptionVec(subty) => (FieldKind::OptionVec, subty),
            ArgTyKind::Option(subty) => (FieldKind::Option, subty),
            ArgTyKind::Convert => (FieldKind::UnwrapOption, &field.ty),
        };

        let mut arg_names = Vec::new();
        if arg.is_named() {
            // Named arguments.

            let mut name_display = String::new();

            if arg.last || arg.trailing_var_arg {
                errs.push(Error::new(
                    field.ty.span(),
                    "arg(last, trailing_var_arg) must be used on positional arguments",
                ));
                continue;
            }

            if let Some(ch) = &arg.short {
                let ch = match ch {
                    Override::Explicit(c) => *c,
                    Override::Inherit => ident_str.chars().next().expect("must have name"),
                };
                name_display = format!("-{ch}");
                arg_names.push(ch.to_string());
            }
            for &short in arg.short_alias.iter() {
                arg_names.push(short.to_string());
            }
            if let Some(name) = &arg.long {
                let long_name = match name {
                    Override::Inherit => {
                        format!("--{}", heck::AsKebabCase(&ident_str))
                    }
                    Override::Explicit(s) => format!("--{s}"),
                };
                // Prefer long names for display.
                name_display = long_name.clone();
                arg_names.push(long_name);
            }
            for long in arg.alias.iter() {
                arg_names.push(format!("--{long}"));
            }
            assert!(!arg_names.is_empty());

            out.named_fields.push(FieldInfo {
                ident,
                doc: arg.doc,
                kind,
                effective_ty,
                arg_names,
                name_display,
                value_display,
                require_eq: arg.require_equals,
            });
        } else {
            // Unamed arguments.

            let value_display = arg
                .value_name
                .clone()
                .unwrap_or_else(|| heck::AsShoutySnakeCase(ident_str).to_string());
            let is_vec_like = matches!(kind, FieldKind::Vec | FieldKind::OptionVec);
            let info = FieldInfo {
                ident,
                doc: arg.doc,
                kind,
                effective_ty,
                arg_names: Vec::new(),
                name_display: value_display.clone(),
                value_display,
                require_eq: false,
            };

            if arg.last {
                // Last argument(s).
                if let Some(prev) = out.last_field.replace(info) {
                    errs.push(Error::new(ident.span(), "duplicated arg(last)"));
                    errs.push(Error::new(prev.ident.span(), "previously defined here"));
                    continue;
                }
            } else if is_vec_like {
                // Variable length unnamed argument.
                let info = CatchallFieldInfo::VecLike {
                    field: info,
                    greedy: arg.trailing_var_arg,
                };
                if let Some(prev) = out.catchall_field.replace(info) {
                    errs.push(Error::new(
                        ident.span(),
                        "duplicated variable-length arguments",
                    ));
                    errs.push(Error::new(prev.ident().span(), "previously defined here"));
                    continue;
                }
            } else {
                // Single unnamed argument.
                out.unnamed_fields.push(info);
            }
        }
    }

    Ok(out)
}

impl ToTokens for ParserStateDefImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            vis,
            state_name,
            output_ty,
            output_ctor,
            named_fields,
            unnamed_fields,
            catchall_field,
            last_field,
            flatten_fields,
            ..
        } = self;
        let output_ctor = output_ctor.as_ref().unwrap_or(&self.output_ty);

        // State initialization and finalization.
        //
        // T / Option<T>            => Option<T>
        // Vec<T> / Option<Vec<T>>  => Option<Vec<T>>
        // FlattenTy                => <FlattenTy as ArgsInternal>::__State
        // SubcommandTy             => Option<SubcommandTy>
        let mut field_names = Vec::new();
        let mut field_tys = Vec::new();
        let mut field_inits = Vec::new();
        let mut field_finishes = Vec::new();
        let mut check_missings = TokenStream::new();
        for f in named_fields
            .iter()
            .chain(unnamed_fields)
            .chain(last_field.as_ref())
        {
            let ident = f.ident;
            let effective_ty = f.effective_ty;
            let name_display = &f.name_display;
            field_names.push(ident);
            match f.kind {
                FieldKind::BoolSetTrue | FieldKind::Option | FieldKind::UnwrapOption => {
                    field_tys.push(quote! { __rt::Option<#effective_ty> });
                    field_inits.push(quote! { __rt::None });
                    if f.kind == FieldKind::UnwrapOption {
                        check_missings.extend(quote! {
                            if self.#ident.is_none() {
                                return __rt::missing_required_arg(#name_display)
                            }
                        });
                    }
                }
                FieldKind::Vec | FieldKind::OptionVec => {
                    field_tys.push(quote! { __rt::Option<__rt::Vec<#effective_ty>> });
                    field_inits.push(quote! { __rt::None });
                }
            };
            let tail = match f.kind {
                FieldKind::UnwrapOption => quote! { .unwrap() },
                FieldKind::BoolSetTrue | FieldKind::Vec => quote! { .unwrap_or_default() },
                FieldKind::Option | FieldKind::OptionVec => quote!(),
            };
            field_finishes.push(quote! { self.#ident #tail });
        }
        match catchall_field {
            None => {}
            Some(CatchallFieldInfo::Subcommand {
                ident,
                effective_ty,
                optional,
            }) => {
                field_names.push(ident);
                field_tys.push(quote! { __rt::Option<#effective_ty> });
                field_inits.push(quote! { __rt::None });
                let tail = if *optional {
                    quote!()
                } else {
                    check_missings.extend(quote! {
                        if self.#ident.is_none() {
                            return __rt::missing_required_subcmd()
                        }
                    });
                    quote! { .unwrap() }
                };
                field_finishes.push(quote! { self.#ident #tail });
            }
            Some(CatchallFieldInfo::VecLike { field, .. }) => {
                let ident = field.ident;
                let effective_ty = field.effective_ty;
                field_names.push(field.ident);
                field_tys.push(quote! { __rt::Option<__rt::Vec<#effective_ty>> });
                field_inits.push(quote! { __rt::None });
                let tail = (field.kind == FieldKind::Vec).then(|| quote! { .unwrap_or_default() });
                field_finishes.push(quote! { self.#ident #tail });
            }
        }
        for f in flatten_fields {
            let ident = f.ident;
            let effective_ty = f.effective_ty;
            field_names.push(f.ident);
            field_tys.push(quote! { <#effective_ty as __rt::ArgsInternal>::__State });
            field_inits.push(quote! { __rt::ParserState::init() });
            field_finishes.push(quote! { __rt::ParserState::finish(self.#ident)? });
        }

        // Named argument dispatcher.

        let mut feed_named_arms = named_fields
            .iter()
            .map(|f| {
                let ident = f.ident;
                let arg_names = &f.arg_names;
                let require_eq = f.require_eq;
                let value_info = value_info(f.effective_ty);
                let span = f.effective_ty.span();
                let action = match f.kind {
                    FieldKind::BoolSetTrue => {
                        quote! {{ self.#ident = Some(true); __rt::place_for_flag() }}
                    }
                    FieldKind::Option | FieldKind::UnwrapOption => quote_spanned! {span=>
                        __rt::place_for_set_value::<_, _, #require_eq>(&mut self.#ident, #value_info)
                    },
                    FieldKind::Vec | FieldKind::OptionVec => quote_spanned! {span=>
                        __rt::place_for_vec::<_, _, #require_eq>(self.#ident.get_or_insert_default(), #value_info)
                    },
                };
                quote! { #(#arg_names)|* => #action, }
            })
            .collect::<TokenStream>();
        let flatten_names = flatten_fields.iter().map(|f| f.ident);
        let feed_named_else = quote! {
            #(__rt::ParserStateDyn::feed_named(&mut self.#flatten_names, __name)?;)*
        };
        let feed_named_func = if feed_named_arms.is_empty() && feed_named_else.is_empty() {
            TokenStream::new()
        } else {
            feed_named_arms = if feed_named_arms.is_empty() {
                quote! { #feed_named_else __rt::FeedNamed::Continue(()) }
            } else {
                quote! {
                    __rt::FeedNamed::Break(match __name {
                        #feed_named_arms
                        _ => { #feed_named_else return __rt::FeedNamed::Continue(()) }
                    })
                }
            };
            quote! {
                fn feed_named(&mut self, __name: &__rt::str) -> __rt::FeedNamed<'_> {
                    #feed_named_arms
                }
            }
        };

        // Unnamed argument dispatcher.

        let feed_unnamed_func = if unnamed_fields.is_empty()
            && catchall_field.is_none()
            && last_field.is_none()
        {
            quote!()
        } else {
            let asserts = flatten_fields.iter().map(|f| {
                let ty = f.effective_ty;
                quote_spanned! {ty.span()=>
                    const {
                        __rt::assert!(
                            <<#ty as __rt::ArgsInternal>::__State as __rt::ParserState>::TOTAL_UNNAMED_ARG_CNT == 0,
                            "TODO: cannot arg(flatten) positional arguments yet",
                        );
                    }
                }
            }).collect::<TokenStream>();

            let mut arms = TokenStream::new();
            for (idx, f) in unnamed_fields.iter().enumerate() {
                let name = f.ident;
                let parsed = value_parsed(f.effective_ty);
                arms.extend(quote! {
                    #idx => self.#name = __rt::Some(#parsed),
                });
            }

            let catchall = if let Some(f) = catchall_field {
                match f {
                    CatchallFieldInfo::Subcommand { ident, .. } => {
                        quote! { __rt::place_for_subcommand(&mut self.#ident) }
                    }
                    CatchallFieldInfo::VecLike { greedy, field } => {
                        let ident = field.ident;
                        if *greedy {
                            let value_info = value_info(field.effective_ty);
                            quote! { __rt::place_for_trailing_var_arg(&mut self.#ident, #value_info) }
                        } else {
                            let parsed = value_parsed(field.effective_ty);
                            quote! {{ self.#ident.get_or_insert_default().push(#parsed); __rt::Ok(__rt::None) }}
                        }
                    }
                }
            } else {
                quote! { __rt::Err(__rt::None) }
            };

            let non_last = if arms.is_empty() {
                catchall
            } else {
                quote! {
                    match __idx {
                        #arms
                        _ => return #catchall
                    }
                    __rt::Ok(__rt::None)
                }
            };

            let handle_last = if let Some(f) = last_field {
                let ident = f.ident;
                let value_info = value_info(f.effective_ty);
                quote! {
                    if __is_last {
                        return __rt::place_for_trailing_var_arg(&mut self.#ident, #value_info);
                    }
                }
            } else {
                quote!()
            };

            quote! {
                fn feed_unnamed(
                    &mut self,
                    __arg: &mut __rt::OsString,
                    __idx: __rt::usize,
                    __is_last: __rt::bool,
                ) -> __rt::FeedUnnamed {
                    #asserts
                    #handle_last
                    #non_last
                }
            }
        };

        let self_unnamed_arg_cnt = unnamed_fields.len();
        let flatten_tys = flatten_fields.iter().map(|f| f.effective_ty);

        let args_info_lit = ArgsInfoLiteral(self);

        tokens.extend(quote! {
            // FIXME: Visibility is still broken with `command(flatten)`.
            // Inherited visibility is needed to avoid "private type in public interface".
            // It is always invisible from user site because we are in an anonymous scope.
            #vis struct #state_name {
                #(#field_names : #field_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;
                const ARGS_INFO: __rt::ArgsInfo = #args_info_lit;
                const TOTAL_UNNAMED_ARG_CNT: __rt::usize =
                    #self_unnamed_arg_cnt #(+ <<#flatten_tys as __rt::ArgsInternal>::__State as __rt::ParserState>::TOTAL_UNNAMED_ARG_CNT)*;

                fn init() -> Self {
                    Self {
                        #(#field_names : #field_inits,)*
                    }
                }

                fn finish(self) -> __rt::Result<Self::Output> {
                    #check_missings
                    __rt::Ok(#output_ctor {
                        #(#field_names : #field_finishes,)*
                    })
                }
            }

            #[automatically_derived]
            impl __rt::ParserStateDyn for #state_name {
                #feed_named_func
                #feed_unnamed_func
            }
        });
    }
}

/// Generates the reflection constant with type `ArgsInfo`.
struct ArgsInfoLiteral<'a>(&'a ParserStateDefImpl<'a>);

impl ToTokens for ArgsInfoLiteral<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ParserStateDefImpl {
            named_fields,
            unnamed_fields,
            catchall_field,
            last_field,
            flatten_fields,
            cmd_attrs,
            ..
        } = self.0;

        let named_args = named_fields.iter().map(
            |FieldInfo {
                 arg_names,
                 value_display,
                 require_eq,
                 kind,
                 doc,
                 ..
             }| {
                let long_names = arg_names.iter().filter(|s| s.starts_with("--"));
                let short_names = arg_names
                    .iter()
                    .filter(|s| !s.starts_with("--"))
                    .map(|s| format!("-{s}"));
                let values = match kind {
                    FieldKind::BoolSetTrue => quote! { [] },
                    FieldKind::Option
                    | FieldKind::UnwrapOption
                    | FieldKind::Vec
                    | FieldKind::OptionVec => quote! { [#value_display] },
                };
                quote! {
                    __rt::refl::NamedArgInfo::__new(
                        #doc,
                        &[#(#long_names),*],
                        &[#(#short_names),*],
                        &#values,
                        #require_eq,
                    )
                }
            },
        );
        let unnamed_args = unnamed_fields.iter().map(
            |FieldInfo {
                 doc, value_display, ..
             }| {
                quote! { __rt::refl::UnnamedArgInfo::__new(#doc, #value_display) }
            },
        );
        let mut trailing_var_arg = quote! { __rt::None };
        let mut subcmd = trailing_var_arg.clone();
        match catchall_field {
            None => {}
            Some(CatchallFieldInfo::Subcommand {
                effective_ty,
                optional,
                ..
            }) => {
                subcmd = quote! {
                    Some((#optional, <#effective_ty as __rt::CommandInternal>::COMMAND_INFO))
                };
            }
            Some(CatchallFieldInfo::VecLike { greedy, field }) => {
                let doc = &field.doc;
                let value_display = &field.value_display;
                trailing_var_arg = quote! {
                    Some((#greedy, __rt::refl::UnnamedArgInfo::__new(#doc, #value_display)))
                }
            }
        };
        let last_arg = match last_field {
            Some(FieldInfo {
                doc, value_display, ..
            }) => {
                quote! { __rt::Some(__rt::refl::UnnamedArgInfo::__new(#doc, #value_display)) }
            }
            None => quote! { __rt::None },
        };
        let flatten_args =
            flatten_fields
                .iter()
                .map(|FlattenFieldInfo { effective_ty, .. }| {
                    quote! { &<<#effective_ty as __rt::ArgsInternal>::__State as __rt::ParserState>::ARGS_INFO }
                });

        let app_info = AppInfoLiteral(cmd_attrs.as_ref());

        tokens.extend(quote! {
            __rt::refl::ArgsInfo::__new(
                &[#(#named_args),*],
                &[#(#unnamed_args),*],
                #trailing_var_arg,
                #subcmd,
                #last_arg,
                &[#(#flatten_args),*],
                #app_info,
            )
        });
    }
}

/// Generates the reflection constant with type `Option<&'static AppInfo>`.
struct AppInfoLiteral<'i>(Option<&'i TopCommandMeta>);

impl ToTokens for AppInfoLiteral<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Some(TopCommandMeta {
            doc,
            name,
            version,
            author,
            about,
            long_about,
        }) = self.0
        else {
            tokens.extend(quote! { __rt::None });
            return;
        };

        let name = match name {
            Some(s) => quote! { #s },
            None => quote! { __rt::env!("CARGO_PKG_NAME") },
        };
        let version = match version {
            Some(Override::Explicit(s)) => quote! { #s },
            Some(Override::Inherit) => quote! { __rt::env!("CARGO_PKG_VERSION") },
            None => quote! { "" },
        };
        let author = match author {
            Some(Override::Explicit(s)) => quote! { #s },
            Some(Override::Inherit) => quote! { __rt::env!("CARGO_PKG_AUTHORS") },
            None => quote! { "" },
        };
        let about = match about {
            Some(Override::Explicit(s)) => quote! { #s },
            Some(Override::Inherit) => doc.summary().to_token_stream(),
            None => quote! { "" },
        };
        let long_about = match long_about {
            Some(Override::Explicit(s)) => quote! { #s },
            Some(Override::Inherit) => doc.to_token_stream(),
            None => quote! { "" },
        };

        tokens.extend(quote! {
            // Force promote in case of any arguments referencing statics.
            __rt::Some(&const {
                __rt::refl::AppInfo::__new(
                    #name,
                    #version,
                    #author,
                    #about,
                    #long_about,
                )
            })
        });
    }
}
