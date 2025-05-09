use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use crate::{ErrorKind, Result};

mod sealed {
    pub trait Sealed {}
}

#[diagnostic::on_unimplemented(
    message = "`{Self}` cannot be parsed into a palc argument value",
    label = "Unparsable type",
    note = "for enum types, try `derive(palc::ValueEnum)` on it",
    note = "or you can implement either `From<OsString>`, `From<String>` or `FromStr` for it \
    to make it parseable"
)]
pub trait ArgValueInfo<T>: 'static + Sized + sealed::Sealed {
    fn parse(v: Cow<'_, OsStr>) -> Result<T>;
}

pub trait ValueEnum: Sized {
    fn parse_value(s: &str) -> Option<Self>;
}

#[macro_export]
#[doc(hidden)]
macro_rules! arg_value_info {
    ($ty:ty) => {
        $crate::__private::InferValueParser::<$ty, &&&&()>($crate::__private::PhantomData).get()
    };
}

pub struct InferValueParser<T, Fuel>(pub PhantomData<(T, Fuel)>);

impl<T, Fuel> Deref for InferValueParser<T, &Fuel> {
    type Target = InferValueParser<T, Fuel>;
    fn deref(&self) -> &Self::Target {
        &InferValueParser(PhantomData)
    }
}

impl<T: ValueEnum> InferValueParser<T, &&&&()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: ValueEnum> ArgValueInfo<T> for Info {
            fn parse(v: Cow<'_, OsStr>) -> Result<T> {
                // TODO: better diagnostics?
                v.to_str()
                    .ok_or(ErrorKind::InvalidUtf8)
                    .and_then(|s| T::parse_value(s).ok_or(ErrorKind::InvalidValue))
                    .map_err(|err| err.with_input(v.into_owned()))
            }
        }
        Info
    }
}

impl<T: From<OsString>> InferValueParser<T, &&&()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: From<OsString>> ArgValueInfo<T> for Info {
            fn parse(v: Cow<'_, OsStr>) -> Result<T> {
                Ok(v.into_owned().into())
            }
        }
        Info
    }
}

// TODO: TryFrom.
impl<T: From<String>> InferValueParser<T, &&()> {
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T: From<String>> ArgValueInfo<T> for Info {
            fn parse(v: Cow<'_, OsStr>) -> Result<T> {
                Ok(v.into_owned()
                    .into_string()
                    .map_err(|e| ErrorKind::InvalidUtf8.with_input(e))?
                    .into())
            }
        }
        Info
    }
}

impl<T> InferValueParser<T, &()>
where
    // This implies either `T::Err` impls `std::error::Error` or it is string-like.
    // Note that `&str: !std::error::Error`.
    T: FromStr<Err: Into<Box<dyn std::error::Error + Send + Sync + 'static>>>,
{
    pub fn get(&self) -> impl ArgValueInfo<T> {
        struct Info;
        impl sealed::Sealed for Info {}
        impl<T> ArgValueInfo<T> for Info
        where
            T: FromStr<Err: Into<Box<dyn std::error::Error + Send + Sync + 'static>>>,
        {
            fn parse(v: Cow<'_, OsStr>) -> Result<T> {
                let s = v
                    .to_str()
                    .ok_or_else(|| ErrorKind::InvalidUtf8.with_input(v.clone().into()))?;
                let t = s.parse::<T>().map_err(|err| {
                    ErrorKind::InvalidValue.with_input(s.into()).with_source(err.into())
                })?;
                Ok(t)
            }
        }
        Info
    }
}

// For error reporting.
// Since `ArgValueInfo` is sealed and all implementations are private, this user type is guaranteed
// to cause an unimplemented error on `ArgValueInfo`.
impl<T> InferValueParser<T, ()> {
    pub fn get(&self) -> T {
        unreachable!()
    }
}

#[test]
fn native_impls() {
    use std::path::PathBuf;

    fn has_parser<T>(_: impl ArgValueInfo<T>) {}

    has_parser::<OsString>(arg_value_info!(OsString));
    has_parser::<PathBuf>(arg_value_info!(PathBuf));
    has_parser::<String>(arg_value_info!(String));
    has_parser::<usize>(arg_value_info!(usize));

    let _ = || {
        let () = arg_value_info!(());
    };
}
