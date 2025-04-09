use std::ops;

use darling::util::Override;
use darling::{Error, FromField, FromMeta, FromVariant, Result};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::spanned::Spanned;
use syn::{Attribute, Expr, GenericArgument, Ident, PathArguments, Type};

pub const TY_BOOL: &str = "bool";
pub const TY_OPTION: &str = "Option";
pub const TY_VEC: &str = "Vec";

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

#[derive(FromVariant)]
pub struct CommandVariant {
    pub ident: Ident,
    pub fields: darling::ast::Fields<ArgField>,
}

pub fn wrap_anon_item(tts: impl ToTokens) -> TokenStream {
    quote! {
        const _: () = {
            use ::clap_static::__private as __rt;
            #tts
        };
    }
}

#[derive(FromField)]
#[darling(forward_attrs(arg, command))]
pub struct ArgField {
    pub ident: Option<Ident>,
    pub ty: Type,

    #[darling(with = parse_arg_or_command)]
    pub attrs: ArgOrCommand,
}

pub enum ArgTyKind<'a> {
    Bool,
    Option(&'a Type),
    Convert,
    Vec(&'a Type),
    OptionVec(&'a Type),
}

impl ArgField {
    pub fn arg_ty_kind(&self) -> ArgTyKind<'_> {
        let ty = &self.ty;
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

fn parse_arg_or_command(attrs: Vec<Attribute>) -> Result<ArgOrCommand> {
    let mut arg = None;
    let mut command = None;
    for attr in attrs {
        let path = attr.path();
        if path.is_ident("arg") {
            if arg.is_some() {
                return Err(Error::duplicate_field("arg").with_span(&path.span()));
            }
            arg = Some(ArgMeta::from_meta(&attr.meta)?);
        } else if path.is_ident("command") {
            if command.is_some() {
                return Err(Error::duplicate_field("command").with_span(&path.span()));
            }
            command = Some((path.span(), CommandMeta::from_meta(&attr.meta)?));
        }
    }
    if let (Some(_), Some((span, _))) = (&arg, &command) {
        return Err(Error::custom("#[arg(..)] conflicts with #[command(..)]").with_span(span));
    }

    if let Some((_, cmd)) = &command {
        if cmd.subcommand {
            return Ok(ArgOrCommand::Subcommand);
        }
        return Ok(ArgOrCommand::Flatten);
    }

    Ok(ArgOrCommand::Arg(arg.unwrap_or_default()))
}

pub enum ArgOrCommand {
    Arg(ArgMeta),
    Subcommand,
    Flatten,
}

#[derive(FromMeta, Default)]
#[cfg_attr(feature = "__test-allow-unknown-fields", darling(allow_unknown_fields))]
#[darling(default, and_then = ArgMeta::validate)]
pub struct ArgMeta {
    // Names.
    pub long: Option<Override<String>>,
    pub short: Option<Override<char>>,
    pub alias: OneOrArray<String>,
    pub short_alias: OneOrArray<char>,
    pub value_name: Option<String>,
    // TODO: {,visible_}{,short_}alias{,es}, value_names

    // Argument behaviors.
    pub require_equals: bool,
    // TODO: global, trailing_var_args, last, raw

    // Value behaviors.
    // TODO: num_args, value_delimiter, default_value,
    // index, action, value_terminator, default_missing_value*, env
    // Not needed: required

    // Help & completion.
    pub help: Option<String>,
    pub long_help: Option<String>,
    pub display_order: Option<usize>,
    pub help_heading: Option<String>,
    pub next_line_help: bool,
    pub hide: bool,
    pub hide_possible_values: bool,
    pub hide_default_value: bool,
    pub hide_env: bool,
    pub hide_env_values: bool,
    pub hide_short_help: bool,
    pub hide_long_help: bool,
    // TODO: add

    // Validation.
    // TODO: exclusive, requires, default_value_if{,s}, required_unless_present*, required_if*,
    // conflicts_with*, overrides_with*
}

impl ArgMeta {
    pub fn is_named(&self) -> bool {
        self.long.is_some() || self.short.is_some()
    }

    fn validate(self) -> Result<Self> {
        if !self.is_named() && self.require_equals {
            return Err(Error::custom(
                "arg(require_equals) is only useful for named arguments",
            ));
        }
        Ok(self)
    }
}

#[derive(FromMeta)]
#[darling(and_then = CommandMeta::validate)]
pub struct CommandMeta {
    #[darling(default)]
    pub subcommand: bool,
    #[darling(default)]
    pub flatten: bool,
}

impl CommandMeta {
    fn validate(self) -> Result<Self> {
        if !self.subcommand && !self.flatten {
            return Err(Error::custom("empty `#[command(..)]`"));
        }
        if self.subcommand && self.flatten {
            return Err(Error::custom(
                "`#[command(subcommand)]` conflicts with `#[command(flatten)]`",
            ));
        }
        Ok(self)
    }
}

#[derive(Default)]
pub struct OneOrArray<T>(pub Vec<T>);

impl<T> ops::Deref for OneOrArray<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: FromMeta> FromMeta for OneOrArray<T> {
    fn from_value(value: &syn::Lit) -> Result<Self> {
        T::from_value(value).map(|elem| Self(vec![elem]))
    }

    fn from_expr(expr: &Expr) -> Result<Self> {
        match *expr {
            Expr::Array(ref arr) => {
                let mut errs = darling::Error::accumulator();
                let mut ret = Vec::with_capacity(arr.elems.len());
                for elem in arr.elems.iter() {
                    ret.extend(errs.handle(T::from_expr(elem)));
                }
                errs.finish_with(Self(ret))
            }
            Expr::Lit(ref lit) => Self::from_value(&lit.lit),
            Expr::Group(ref group) => Self::from_expr(&group.expr),
            _ => Err(Error::unexpected_expr_type(expr)),
        }
        .map_err(|e| e.with_span(expr))
    }
}
