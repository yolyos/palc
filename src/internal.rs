pub use std::borrow::Cow;
pub use std::convert::Infallible;
pub use std::ffi::{OsStr, OsString};
pub use std::unreachable;
pub use {Default, Err, Iterator, None, Ok, Option, Result, Some, char, str, usize};

use super::Error;

pub fn err_require_arg<T>(v: Option<T>, name: &'static str) -> Result<T, Error> {
    match v {
        Some(v) => Ok(v),
        None => Err(Error::MissingRequiredArgument(name)),
    }
}

pub fn err_extra_positional(arg: OsString) -> Result<(), Error> {
    Err(Error::UnknownArgument(arg))
}

pub fn err_require_subcmd<T>(subcmd: Option<T>) -> Result<T, Error> {
    match subcmd {
        Some(subcmd) => Ok(subcmd),
        None => Err(Error::MissingRequiredSubcommand),
    }
}

pub enum Action<B> {
    Flag(fn(&mut B) -> Result<(), Error>),
    KeyValue(fn(&mut B, Cow<'_, OsStr>) -> Result<(), Error>),
}

pub trait ParserInternal {
    type __Builder: ParserBuilder<Output = Self>;
}

pub trait ParserBuilder: Sized + Default + 'static {
    type Output;
    type Subcommand: crate::Subcommand;

    const SHORT_ARGS: &'static [(char, Action<Self>)];
    const LONG_ARGS: &'static [(&'static str, Action<Self>)];

    fn feed_positional(&mut self, arg: OsString) -> Result<(), Error>;
    fn finish(self, subcmd: Option<Self::Subcommand>) -> Result<Self::Output, Error>;
}

pub trait SubcommandInternal: Sized {
    const __COMMANDS: &'static [&'static str];

    fn __try_parse_subcommand_from(
        cmd_idx: usize,
        iter: &mut dyn Iterator<Item = OsString>,
    ) -> Result<Self, Error>;
}

pub fn try_parse_from_builder<B: ParserBuilder>(
    iter: &mut dyn Iterator<Item = OsString>,
) -> Result<B::Output, Error> {
    let long_args = B::LONG_ARGS;
    let short_args = B::SHORT_ARGS;

    let mut builder = B::default();
    while let Some(arg) = iter.next() {
        let argb = arg.as_encoded_bytes();
        if argb == b"--" {
            todo!()
        } else if argb == b"-" {
            todo!()
        } else if argb.starts_with(b"--") {
            let arg = &arg
                .to_str()
                .ok_or_else(|| Error::InvalidUtf8(arg.to_os_string()))?[2..];
            let (name, value) = match arg.split_once("=") {
                Some((name, value)) => (name, Some(value)),
                None => (arg, None),
            };
            let idx = long_args
                .binary_search_by_key(&name, |(k, _)| k)
                .map_err(|_| Error::UnknownArgument(name.into()))?;
            match (&long_args[idx].1, value) {
                (Action::Flag(_), Some(v)) => {
                    return Err(Error::UnexpectedValue(name.into(), v.into()));
                }
                (Action::Flag(f), None) => f(&mut builder)?,
                (Action::KeyValue(f), Some(value)) => {
                    f(&mut builder, Cow::Borrowed(OsStr::new(value)))?;
                }
                (Action::KeyValue(f), None) => {
                    let value = iter.next().ok_or(Error::MissingValue(name.into()))?;
                    f(&mut builder, Cow::Owned(value))?;
                }
            }
        } else if argb.starts_with(b"-") {
            let arg = &arg
                .to_str()
                .ok_or_else(|| Error::InvalidUtf8(arg.to_os_string()))?[1..];
            let mut chars = arg.chars();
            for name in chars.by_ref() {
                let idx = short_args
                    .binary_search_by_key(&name, |(k, _)| *k)
                    .map_err(|_| Error::UnknownArgument(name.to_string().into()))?;
                match &short_args[idx].1 {
                    Action::Flag(f) => f(&mut builder)?,
                    Action::KeyValue(f) => {
                        let rest = chars.as_str();
                        let value = if !rest.is_empty() {
                            Cow::Borrowed(OsStr::new(rest.strip_prefix('=').unwrap_or(rest)))
                        } else {
                            Cow::Owned(iter.next().ok_or(Error::MissingValue(name.into()))?)
                        };
                        f(&mut builder, value)?;
                        break;
                    }
                }
            }
        } else if let Err(err) = builder.feed_positional(arg.clone()) {
            let arg = match err {
                Error::UnknownArgument(arg) => arg,
                err => return Err(err),
            };
            if let Ok(idx) = B::Subcommand::__COMMANDS
                .binary_search_by_key(&arg.as_encoded_bytes(), |k| k.as_bytes())
            {
                let subcmd = B::Subcommand::__try_parse_subcommand_from(idx, iter)?;
                return builder.finish(Some(subcmd));
            } else {
                return Err(Error::UnknownArgument(arg));
            }
        }
    }
    builder.finish(None)
}
