use std::ops;

use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::meta::ParseNestedMeta;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{Attribute, GenericArgument, LitBool, LitChar, LitStr, PathArguments, Type};
use syn::{Token, bracketed, token};

pub const TY_BOOL: &str = "bool";
pub const TY_OPTION: &str = "Option";
pub const TY_VEC: &str = "Vec";

#[derive(Default)]
pub(crate) struct ErrorCollector {
    err: Option<syn::Error>,
    defused: bool,
}

impl ErrorCollector {
    pub fn collect<T>(&mut self, ret: syn::Result<T>) -> Option<T> {
        match ret {
            Ok(v) => Some(v),
            Err(e) => {
                self.push(e);
                None
            }
        }
    }

    pub fn push(&mut self, e: syn::Error) {
        match &mut self.err {
            Some(prev) => prev.combine(e),
            p @ None => *p = Some(e),
        }
    }

    pub fn finish(mut self) -> syn::Result<()> {
        self.defused = true;
        match self.err.take() {
            None => Ok(()),
            Some(e) => Err(e),
        }
    }

    pub fn finish_then<T>(self, v: T) -> syn::Result<T> {
        self.finish().map(|_| v)
    }
}

impl Drop for ErrorCollector {
    fn drop(&mut self) {
        if !self.defused && !std::thread::panicking() {
            assert!(self.err.is_none(), "error not finished");
        }
    }
}

/// `TyCtor<ArgTy>` => `ArgTy`. `ty_ctor` must be a single-identifier path.
pub fn strip_ty_ctor<'i>(mut ty: &'i Type, ty_ctor: &str) -> Option<&'i Type> {
    let ty = loop {
        match ty {
            Type::Group(inner) => ty = &inner.elem,
            Type::Paren(inner) => ty = &inner.elem,
            Type::Path(inner) => break inner,
            _ => return None,
        }
    };
    if ty.path.leading_colon.is_none() && ty.path.segments.len() == 1 {
        let seg = &ty.path.segments[0];
        if seg.ident == ty_ctor {
            if let PathArguments::AngleBracketed(args) = &seg.arguments {
                if args.args.len() == 1 {
                    if let GenericArgument::Type(arg_ty) = &args.args[0] {
                        return Some(arg_ty);
                    }
                }
            }
        }
    }
    None
}

pub fn wrap_anon_item(tts: impl ToTokens) -> TokenStream {
    quote! {
        const _: () = {
            use ::clap_static::__private as __rt;
            #tts
        };
    }
}

pub enum ArgTyKind<'a> {
    Bool,
    Option(&'a Type),
    Convert,
    Vec(&'a Type),
    OptionVec(&'a Type),
}

impl ArgTyKind<'_> {
    pub fn of(ty: &syn::Type) -> ArgTyKind<'_> {
        if matches!(ty, Type::Path(p) if p.qself.is_none() && p.path.is_ident(TY_BOOL)) {
            return ArgTyKind::Bool;
        }
        if let Some(subty) = strip_ty_ctor(ty, TY_OPTION) {
            if let Some(subty) = strip_ty_ctor(subty, TY_VEC) {
                return ArgTyKind::OptionVec(subty);
            } else {
                return ArgTyKind::Option(subty);
            }
        }
        if let Some(subty) = strip_ty_ctor(ty, TY_VEC) {
            return ArgTyKind::Vec(subty);
        }
        ArgTyKind::Convert
    }
}

pub enum ArgOrCommand {
    Arg(ArgMeta),
    Command(ArgsCommandMeta),
}

/// Parse `arg(..)` and `command(..)` in `derive(Args)`-like struct or variants.
pub fn parse_args_attrs(fields: &syn::FieldsNamed) -> syn::Result<Vec<ArgOrCommand>> {
    let mut errs = ErrorCollector::default();
    let attrs = fields
        .named
        .iter()
        .filter_map(|f| errs.collect(ArgOrCommand::parse_attrs(&f.attrs)))
        .collect();
    errs.finish_then(attrs)
}

impl ArgOrCommand {
    fn parse_attrs(attrs: &[Attribute]) -> syn::Result<ArgOrCommand> {
        let mut errs = ErrorCollector::default();
        let mut doc = Doc::default();
        let mut arg = None::<ArgMeta>;
        let mut command = None;
        for attr in attrs {
            let path = attr.path();
            doc.extend_from_attr(attr)?;
            if path.is_ident("arg") {
                let arg = arg.get_or_insert_default();
                errs.collect(attr.parse_nested_meta(|meta| arg.parse_update(meta)));
            } else if path.is_ident("command") {
                if let Some(c) = errs.collect(attr.parse_args::<ArgsCommandMeta>()) {
                    if command.is_some() {
                        errs.push(syn::Error::new(path.span(), "duplicated command(..)"));
                    }
                    command = Some((path.span(), c));
                }
            }
        }
        doc.post_process();
        let ret = if let Some((span, c)) = command {
            if arg.is_some() {
                errs.push(syn::Error::new(span, "command(..) conflicts with arg(..)"));
            }
            Self::Command(c)
        } else {
            let mut arg = arg.unwrap_or_default();
            arg.doc = doc;
            Self::Arg(arg)
        };
        errs.finish_then(ret)
    }
}

#[derive(Default)]
pub struct ArgMeta {
    pub doc: Doc,

    // Names.
    pub long: Option<Override<String>>,
    pub short: Option<Override<char>>,
    pub alias: OneOrArray<String>,
    pub short_alias: OneOrArray<char>,
    pub value_name: Option<String>,
    // TODO: {,visible_}{,short_}alias{,es}, value_names

    // Named argument behaviors.
    pub require_equals: bool,
    // TODO: global

    // Unnamed argument behaviors.
    pub trailing_var_arg: bool,
    pub last: bool,
    // TODO: raw

    // Value behaviors.
    // TODO: num_args, value_delimiter, default_value,
    // index, action, value_terminator, default_missing_value*, env
    // Not needed: required

    // Help & completion.
    pub help: Option<String>,
    pub long_help: Option<String>,
    // TODO: add, hide*, next_line_help, help_heading, display_order

    // Validation.
    // TODO: exclusive, requires, default_value_if{,s}, required_unless_present*, required_if*,
    // conflicts_with*, overrides_with*
}

impl ArgMeta {
    pub fn is_named(&self) -> bool {
        self.long.is_some() || self.short.is_some()
    }

    fn parse_update(&mut self, meta: ParseNestedMeta<'_>) -> syn::Result<()> {
        let path = &meta.path;
        let span = path.span();
        macro_rules! ensure {
            ($cond:expr, $msg:expr) => {
                if !$cond {
                    return Err(syn::Error::new(span, $msg));
                }
            };
        }
        macro_rules! check_dup {
            ($name:ident) => {
                ensure!(
                    self.$name.is_none(),
                    concat!("duplicated arg(", stringify!($name), ")")
                );
            };
        }
        macro_rules! check_true {
            () => {
                let v = meta.value()?.parse::<LitBool>()?;
                if !v.value {
                    return Err(syn::Error::new(v.span, "must be true"));
                }
            };
        }

        if path.is_ident("long") {
            check_dup!(long);
            self.long = Some(if meta.input.peek(Token![=]) {
                let v = meta.value()?.parse::<LitStr>()?.value();
                Override::Explicit(v)
            } else {
                Override::Inherit
            });
        } else if path.is_ident("short") {
            check_dup!(short);
            self.short = Some(if meta.input.peek(Token![=]) {
                let v = meta.value()?.parse::<LitChar>()?.value();
                Override::Explicit(v)
            } else {
                Override::Inherit
            });
        } else if path.is_ident("alias") || path.is_ident("aliases") {
            self.alias.extend(
                meta.value()?
                    .parse::<OneOrArray<LitStr>>()?
                    .into_iter()
                    .map(|s| s.value()),
            );
        } else if path.is_ident("short_alias") || path.is_ident("short_aliases") {
            self.short_alias.extend(
                meta.value()?
                    .parse::<OneOrArray<LitChar>>()?
                    .into_iter()
                    .map(|s| s.value()),
            );
        } else if path.is_ident("value_name") {
            check_dup!(value_name);
            self.value_name = Some(meta.value()?.parse::<LitStr>()?.value());
        } else if path.is_ident("require_equals") {
            check_true!();
            self.require_equals = true;
        } else if path.is_ident("trailing_var_arg") {
            check_true!();
            self.trailing_var_arg = true;
        } else if path.is_ident("last") {
            check_true!();
            self.last = true;
        } else if path.is_ident("help") {
            check_dup!(help);
            self.help = Some(meta.value()?.parse::<LitStr>()?.value());
        } else if path.is_ident("long_help") {
            check_dup!(long_help);
            self.help = Some(meta.value()?.parse::<LitStr>()?.value());
        } else {
            if cfg!(feature = "__test-allow-unknown-fields") {
                if meta.input.peek(Token![=]) {
                    meta.value()?.parse::<syn::Expr>()?;
                }
                return Ok(());
            }
            ensure!(false, "unknown attribute");
        }
        Ok(())
    }
}

pub enum ArgsCommandMeta {
    Subcommand,
    Flatten,
}

impl Parse for ArgsCommandMeta {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<syn::Ident>()?;
        Ok(if ident == "subcommand" {
            Self::Subcommand
        } else if ident == "flatten" {
            Self::Flatten
        } else {
            return Err(syn::Error::new(
                ident.span(),
                "must be either 'subcommand' or 'flatten'",
            ));
        })
    }
}

pub struct OneOrArray<T>(pub Vec<T>);

impl<T> Default for OneOrArray<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> ops::Deref for OneOrArray<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> IntoIterator for OneOrArray<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> Extend<T> for OneOrArray<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl<T: Parse> Parse for OneOrArray<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(if input.peek(token::Bracket) {
            let inner;
            bracketed!(inner in input);
            inner
                .parse_terminated(T::parse, Token![,])?
                .into_iter()
                .collect()
        } else {
            vec![input.parse::<T>()?]
        }))
    }
}

pub enum Override<T> {
    Inherit,
    Explicit(T),
}

impl<T: Parse> Parse for Override<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Token![=]) {
            Self::Explicit(input.parse()?)
        } else {
            Self::Inherit
        })
    }
}

/// Collect doc-comments into a single string.
///
/// Paragraph (consecutive doc-comments without blank lines) are joined with space. In the result,
/// the first line is the summary, and each of rest lines corresponds to a paragraph.
///
/// See `src/refl.rs` for runtime usage that depends on this.
#[derive(Default)]
pub struct Doc(String);

impl Doc {
    fn post_process(&mut self) {
        let len = self.0.trim_ascii_end().len();
        self.0.truncate(len);
    }

    fn extend_from_attr(&mut self, attr: &Attribute) -> syn::Result<()> {
        if attr.path().is_ident("doc") {
            if let syn::Meta::NameValue(m) = &attr.meta {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &m.value
                {
                    let s = s.value();
                    let s = s.trim_ascii();
                    if s.is_empty() {
                        if !self.0.ends_with("\n") {
                            self.0.push('\n');
                        }
                    } else {
                        if !self.0.is_empty() && !self.0.ends_with("\n") {
                            self.0.push(' ');
                        }
                        self.0.push_str(s);
                    }
                } else {
                    return Err(syn::Error::new(
                        m.value.span(),
                        "only literal doc comment is supported yet",
                    ));
                }
            }
        }
        Ok(())
    }
}

impl ToTokens for Doc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}
