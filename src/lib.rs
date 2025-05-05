#![forbid(unsafe_code)]
use std::ffi::OsString;
use std::path::PathBuf;

use error::ErrorKind;
use runtime::{ArgsIter, CommandInternal, ParserState};

mod error;
mod refl;
mod runtime;
mod values;

#[cfg(feature = "help")]
mod help;

#[cfg(feature = "derive")]
pub use palc_derive::{Args, Parser, Subcommand, ValueEnum};

pub use crate::error::Error;
use crate::runtime::Sealed;
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
    pub use std::marker::PhantomData;
    pub use std::str::from_utf8;
    pub use {Default, Err, Fn, Iterator, None, Ok, Option, Some, Vec, bool, char, str, usize};

    pub use crate::runtime::*;

    // Macros.
    pub use crate::{__const_concat, arg_value_info};
    pub use std::{assert, concat, env, unimplemented, unreachable};

    // Used by `__arg_value_info!`
    pub use crate::values::{ArgValueInfo, InferValueParser, ValueEnum};

    pub use crate::refl::{RawArgsInfo, RawCommandInfo};
    pub use crate::{Args, Parser, Result, Subcommand};
}

/// Top-level command interface.
///
/// Users should only get an implementation via [`derive(Parser)`](macro@Parser).
pub trait Parser: Sized + CommandInternal + Sealed + 'static {
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

    #[cfg(feature = "help")]
    fn render_long_help(argv0: impl Into<String>) -> String {
        help::render_help_for::<Self>(argv0)
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
///
/// Users should only get an implementation via [`derive(Args)`](macro@Args).
pub trait Args: Sized + Sealed + 'static {
    /// Not public API. Only for proc-macro internal use.
    #[doc(hidden)]
    type __State: ParserState<Output = Self>;
}

/// A subcommand enum.
///
/// Users should only get an implementation via [`derive(Subcommand)`](macro@Subcommand).
pub trait Subcommand: Sized + CommandInternal + Sealed + 'static {}
