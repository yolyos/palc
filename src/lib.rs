#![forbid(unsafe_code)]
use std::convert::Infallible;
use std::ffi::OsString;
use std::path::PathBuf;

use crate::internal::{ArgsInternal, CommandInternal};

use error::ErrorKind;
use internal::{ArgsIter, GlobalAncestors};

mod error;
mod internal;
pub mod refl;
mod values;

#[cfg(feature = "help")]
mod help;

#[cfg(feature = "derive")]
pub use clap_static_derive::{Args, Parser, Subcommand, ValueEnum};

pub use crate::error::Error;
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Not public API. Only for proc-macro internal use.
// To scan all usages:
// ```sh
// rg --only-matching --no-filename '\b__rt::\w+' | LC_COLLATE=C sort --unique
// ```
#[doc(hidden)]
pub mod __private {
    pub use std::borrow::Cow;
    pub use std::convert::Infallible;
    pub use std::ffi::{OsStr, OsString};
    pub use std::{assert, concat, env, unimplemented, unreachable};
    pub use {Default, Err, Fn, Iterator, None, Ok, Option, Some, Vec, bool, char, str, usize};

    // Used by `arg_value_info!`
    pub use crate::arg_value_info;
    pub use crate::values::{ArgValueInfo, InferValueParser, ValueEnum};
    pub use std::marker::PhantomData;

    use crate::ErrorKind;
    pub use crate::internal::*;
    pub use crate::refl::{self, ArgsInfo, CommandInfo};
    pub use crate::{Args, Parser, Result, Subcommand};

    /// The fallback state type for graceful failing from proc-macro.
    pub struct FallbackState<T>(Infallible, PhantomData<T>);

    impl<T: 'static> ParserState for FallbackState<T> {
        type Output = T;
        type Subcommand = Infallible;
        const ARGS_INFO: ArgsInfo = ArgsInfo::empty();
        const TOTAL_UNNAMED_ARG_CNT: usize = 0;
        fn init() -> Self {
            unimplemented!()
        }
        fn finish(self) -> Result<Self::Output> {
            match self {}
        }
        fn subcommand_getter() -> impl Fn(&mut Self) -> &mut Option<Self::Subcommand> {
            |_| unreachable!()
        }
    }
    impl<T: 'static> ParserStateDyn for FallbackState<T> {}

    pub fn unknown_subcommand<T>(arg: &str) -> Result<T> {
        Err(ErrorKind::UnknownSubcommand(arg.into()).into())
    }

    pub fn missing_required_arg<T>(arg: &'static str) -> Result<T> {
        Err(ErrorKind::MissingRequiredArgument.with_arg(arg))
    }

    pub fn missing_required_subcmd<T>() -> Result<T> {
        Err(ErrorKind::MissingRequiredSubcommand.into())
    }

    // TODO: Detail errors.
    pub fn fail_constraint<T>(arg: &'static str) -> Result<T> {
        Err(ErrorKind::Constraint.with_arg(arg))
    }

    #[inline]
    pub fn take_arg(s: &mut OsString) -> Cow<'static, OsStr> {
        Cow::Owned(std::mem::take(s))
    }
}

/// Top-level command interface.
pub trait Parser: Sized + 'static + CommandInternal {
    fn parse() -> Self {
        match Self::try_parse_from(std::env::args_os()) {
            Ok(v) => v,
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
    }

    fn try_parse_from<I, T>(iter: I) -> Result<Self>
    where
        I: IntoIterator<Item = T>,
        T: Into<OsString> + Clone,
    {
        let mut iter = iter.into_iter().map(|s| s.into());
        try_parse_from_command(&mut iter)
    }
}

fn try_parse_from_command<C: CommandInternal>(
    iter: &mut dyn Iterator<Item = OsString>,
) -> Result<C> {
    let arg0 = PathBuf::from(iter.next().ok_or(ErrorKind::MissingArg0)?);
    // A non-UTF8 program name does not matter in help. Multi-call commands will fail anyway.
    let program_name = arg0.file_name().unwrap_or(arg0.as_ref()).to_string_lossy();
    let mut args = ArgsIter::new(iter);
    CommandInternal::try_parse_with_name(&program_name, &mut args, &mut ())
        .map_err(|err| err.in_subcommand::<C>(program_name.into_owned()))
}

/// A group of arguments for composing larger inferface.
pub trait Args: Sized + 'static + ArgsInternal {}

/// A subcommand enum.
pub trait Subcommand: Sized + 'static + CommandInternal {}

impl CommandInternal for Infallible {
    const COMMAND_INFO: refl::CommandInfo = refl::CommandInfo::__new(&[]);

    fn try_parse_with_name(
        name: &str,
        _args: &mut ArgsIter<'_>,
        _global: GlobalAncestors<'_>,
    ) -> Result<Self> {
        Err(ErrorKind::UnknownSubcommand(name.into()).into())
    }
}

impl Subcommand for Infallible {}
