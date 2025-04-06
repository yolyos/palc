pub use std::borrow::Cow;
pub use std::convert::Infallible;
pub use std::ffi::{OsStr, OsString};
pub use std::unreachable;
pub use {Default, Err, Iterator, None, Ok, Option, Result, Some, char, str, usize};

use super::Error;

pub fn require_arg<T>(v: Option<T>, name: &'static str) -> Result<T, Error> {
    match v {
        Some(v) => Ok(v),
        None => Err(Error::MissingRequiredArgument(name)),
    }
}

pub fn extra_positional<T>(arg: OsString) -> Result<T, Error> {
    Err(Error::UnknownArgument(arg))
}

pub fn require_subcmd<T>(subcmd: Option<T>) -> Result<T, Error> {
    match subcmd {
        Some(subcmd) => Ok(subcmd),
        None => Err(Error::MissingRequiredSubcommand),
    }
}

pub fn take_arg(s: &mut OsString) -> Cow<'static, OsStr> {
    Cow::Owned(std::mem::take(s))
}

/// This is actually `enum { Ok(_), Unknown, Err(Error) }`,
/// but use `Result` for `?` support.
/// WAIT: <https://github.com/rust-lang/rust/issues/84277>
pub type FeedNamed<S> = Result<FeedNamedOk<S>, Option<Error>>;
pub enum FeedNamedOk<S> {
    Arg0,
    Arg1(fn(&mut S, Cow<'_, OsStr>) -> Result<(), Error>),
}

/// This is actually `enum { Ok, Unknown, Err(Error) }`. Same as above.
pub type FeedUnnamed = Result<(), Option<Error>>;

pub trait ArgsInternal: Sized + 'static {
    type __State: ParserState<Output = Self>;
}

pub trait ParserState: Sized + Default + 'static {
    type Output;
    type Subcommand: crate::Subcommand;

    fn feed_named(&mut self, name: &str) -> FeedNamed<Self>;

    fn feed_unnamed(&mut self, arg: &mut OsString) -> FeedUnnamed;

    fn finish(self, subcmd: Option<Self::Subcommand>) -> Result<Self::Output, Error>;
}

pub trait CommandInternal: Sized {
    fn try_parse_with_name(
        name: OsString,
        iter: &mut dyn Iterator<Item = OsString>,
    ) -> Result<Self, Error>;
}

pub fn try_parse_with_state<S: ParserState>(
    iter: &mut dyn Iterator<Item = OsString>,
) -> Result<S::Output, Error> {
    let mut state = S::default();
    while let Some(mut arg) = iter.next() {
        let argb = arg.as_encoded_bytes();
        if argb == b"--" {
            todo!()
        } else if argb == b"-" {
            todo!()
        } else if argb.starts_with(b"--") {
            let arg = arg
                .to_str()
                .ok_or_else(|| Error::InvalidUtf8(arg.to_os_string()))?;
            let (name, value) = match arg.split_once("=") {
                Some((name, value)) => (name, Some(value)),
                None => (arg, None),
            };
            match state.feed_named(name) {
                Err(err) => return Err(err.unwrap_or_else(|| Error::UnknownArgument(arg.into()))),
                Ok(FeedNamedOk::Arg0) => {
                    if let Some(v) = value {
                        return Err(Error::UnexpectedValue(name.into(), v.into()));
                    }
                }
                Ok(FeedNamedOk::Arg1(updater)) => {
                    let value = match value {
                        Some(v) => Cow::Borrowed(OsStr::new(v)),
                        None => Cow::Owned(iter.next().ok_or(Error::MissingValue(name.into()))?),
                    };
                    updater(&mut state, value)?
                }
            }
        } else if argb.starts_with(b"-") {
            let mut arg = &arg
                .to_str()
                .ok_or_else(|| Error::InvalidUtf8(arg.to_os_string()))?[1..];

            // Iterate over each `char`, but chunk it as `&str` streams.
            while !arg.is_empty() {
                let mut ch_iter = arg.char_indices();
                ch_iter.next();
                let name = &arg[..arg.len() - ch_iter.as_str().len()];
                arg = ch_iter.as_str();

                match state.feed_named(name) {
                    Ok(FeedNamedOk::Arg0) => {}
                    Err(None) => return Err(Error::UnknownArgument(arg.into())),
                    Err(Some(err)) => return Err(err),
                    Ok(FeedNamedOk::Arg1(updater)) => {
                        let value = if arg.is_empty() {
                            Cow::Owned(
                                iter.next()
                                    .ok_or_else(|| Error::MissingValue(name.into()))?,
                            )
                        } else {
                            let arg = arg.strip_prefix("=").unwrap_or(arg);
                            Cow::Borrowed(OsStr::new(arg))
                        };
                        updater(&mut state, value)?;
                        break;
                    }
                }
            }
        } else {
            match state.feed_unnamed(&mut arg) {
                Ok(()) => {}
                Err(Some(err)) => return Err(err),
                Err(None) => {
                    let subcmd = S::Subcommand::try_parse_with_name(arg, iter)?;
                    return state.finish(Some(subcmd));
                }
            }
        }
    }
    state.finish(None)
}
