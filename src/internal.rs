use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::num::NonZero;

use os_str_bytes::OsStrBytesExt;

use crate::error::ErrorKind;
use crate::{Args, Result, Subcommand};

use super::Error;

/// A named argument with its place attached as `&mut self`.
pub trait ArgPlace {
    fn num_values(&self) -> NumValues;

    fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error>;
}

/// The expected number of values for a named argument to take.
pub enum NumValues {
    Zero,
    One { require_equals: bool },
}

pub fn place_for_flag<'a>() -> &'a mut dyn ArgPlace {
    struct FlagPlace;
    impl ArgPlace for FlagPlace {
        fn num_values(&self) -> NumValues {
            NumValues::Zero
        }
        fn feed(&mut self, _: Cow<'_, OsStr>) -> Result<(), Error> {
            unreachable!()
        }
    }

    // This does no allocation but only initialize it as a dangling reference.
    // From: <https://github.com/rust-lang/rust/issues/103821#issuecomment-1304004618>
    Box::leak(Box::new(FlagPlace))
}

pub fn place_for_vec<T, F, const REQUIRE_EQ: bool>(
    place: &mut Vec<T>,
    _f: F,
) -> &'_ mut dyn ArgPlace
where
    F: Fn(Cow<'_, OsStr>) -> Result<T> + 'static,
{
    #[repr(C)]
    struct Place<T, F, const REQUIRE_EQ: bool>(Vec<T>, F);

    impl<T, F, const REQUIRE_EQ: bool> ArgPlace for Place<T, F, REQUIRE_EQ>
    where
        F: Fn(Cow<'_, OsStr>) -> Result<T>,
    {
        fn num_values(&self) -> NumValues {
            NumValues::One {
                require_equals: REQUIRE_EQ,
            }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            self.0.push((self.1)(value)?);
            Ok(())
        }
    }

    assert_eq!(size_of::<F>(), 0);
    assert_eq!(align_of::<F>(), 1);
    unsafe { &mut *(place as *mut Vec<T> as *mut Place<T, F, REQUIRE_EQ>) }
}

pub fn place_for_set_value<T, F, const REQUIRE_EQ: bool>(
    place: &mut Option<T>,
    _f: F,
) -> &'_ mut dyn ArgPlace
where
    F: Fn(Cow<'_, OsStr>) -> Result<T> + 'static,
{
    #[repr(C)]
    struct Place<T, F, const REQUIRE_EQ: bool>(Option<T>, F);

    impl<T, F, const REQUIRE_EQ: bool> ArgPlace for Place<T, F, REQUIRE_EQ>
    where
        F: Fn(Cow<'_, OsStr>) -> Result<T>,
    {
        fn num_values(&self) -> NumValues {
            NumValues::One {
                require_equals: REQUIRE_EQ,
            }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            self.0 = Some((self.1)(value)?);
            Ok(())
        }
    }

    assert_eq!(size_of::<F>(), 0);
    assert_eq!(align_of::<F>(), 1);
    unsafe { &mut *(place as *mut Option<T> as *mut Place<T, F, REQUIRE_EQ>) }
}

/// A greedy unnamed argument with its place attached as `&mut self`.
///
/// It always consumes all the rest arguments.
pub trait GreedyArgsPlace {
    // TODO: Maybe avoid splitting out the first argument?
    fn feed_greedy(&mut self, arg: OsString, _args: &mut ArgsIter<'_>) -> Result<()>;
}

pub fn place_for_trailing_var_arg<T, F>(place: &mut Vec<T>, _f: F) -> FeedUnnamed<'_>
where
    F: Fn(Cow<'_, OsStr>) -> Result<T> + 'static,
{
    #[repr(C)]
    struct Place<T, F>(Vec<T>, F);

    impl<T, F> GreedyArgsPlace for Place<T, F>
    where
        F: Fn(Cow<'_, OsStr>) -> Result<T>,
    {
        fn feed_greedy(&mut self, arg: OsString, args: &mut ArgsIter<'_>) -> Result<()> {
            self.0.push((self.1)(Cow::Owned(arg))?);
            if let Some(high) = args.iter.size_hint().1 {
                self.0.reserve(high);
            }
            for s in &mut *args.iter {
                self.0.push((self.1)(Cow::Owned(s))?);
            }
            Ok(())
        }
    }

    assert_eq!(size_of::<F>(), 0);
    assert_eq!(align_of::<F>(), 1);
    let place = unsafe { &mut *(place as *mut Vec<T> as *mut Place<T, F>) };
    Ok(Some(place))
}

pub fn place_for_subcommand<C: Subcommand>(place: &mut Option<C>) -> FeedUnnamed<'_> {
    #[repr(transparent)]
    struct Place<C>(Option<C>);

    impl<C: CommandInternal> GreedyArgsPlace for Place<C> {
        fn feed_greedy(&mut self, name: OsString, args: &mut ArgsIter<'_>) -> Result<()> {
            let name = name
                .to_str()
                .ok_or_else(|| ErrorKind::InvalidUtf8(name.clone()))?;
            self.0 = Some(C::try_parse_with_name(name, args)?);
            Ok(())
        }
    }

    let place = unsafe { &mut *(place as *mut Option<C> as *mut Place<C>) };
    Ok(Some(place))
}

pub type FeedNamed<'s> = Option<&'s mut dyn ArgPlace>;

/// This should be an enum, but be this for `?` support, which is unstable to impl.
pub type FeedUnnamed<'s> = Result<Option<&'s mut dyn GreedyArgsPlace>, Option<Error>>;

pub trait ArgsInternal: Sized + 'static {
    type __State: ParserState<Output = Self>;
}

pub trait ParserState: ParserStateDyn {
    type Output;

    fn init() -> Self;
    fn finish(self) -> Result<Self::Output>;
}

pub trait ParserStateDyn: 'static {
    fn feed_named(&mut self, _name: &str) -> FeedNamed<'_> {
        None
    }

    fn feed_unnamed(&mut self, _arg: &mut OsString) -> FeedUnnamed {
        Err(None)
    }
}

pub trait CommandInternal: Sized {
    fn try_parse_with_name(name: &str, args: &mut ArgsIter<'_>) -> Result<Self>;
}

/// A common program-name-agnostic command.
impl<A: Args> CommandInternal for A {
    fn try_parse_with_name(_name: &str, args: &mut ArgsIter<'_>) -> Result<Self> {
        try_parse_args(args)
    }
}

pub fn try_parse_args<A: Args>(args: &mut ArgsIter<'_>) -> Result<A> {
    let mut state = A::__State::init();
    try_parse_with_state(&mut state, args)?;
    state.finish()
}

pub fn try_parse_with_state(state: &mut dyn ParserStateDyn, args: &mut ArgsIter<'_>) -> Result<()> {
    while let Some(arg) = args.cache_next_arg()? {
        match arg {
            Arg::DashDash => todo!(),
            Arg::Named(name) => match state.feed_named(name) {
                Some(place) => match place.num_values() {
                    NumValues::Zero => args.check_no_value()?,
                    NumValues::One {
                        require_equals: false,
                    } => {
                        place.feed(args.take_value()?)?;
                    }
                    NumValues::One {
                        require_equals: true,
                    } => {
                        place.feed(Cow::Borrowed(args.take_value_after_eq()?))?;
                    }
                },
                None => return Err(ErrorKind::UnknownNamedArgument.with_arg(name)),
            },
            Arg::Unnamed(mut arg) => match state.feed_unnamed(&mut arg) {
                Ok(None) => {}
                Ok(Some(place)) => return place.feed_greedy(arg, args),
                Err(Some(err)) => return Err(err),
                Err(None) => return Err(ErrorKind::UnexpectedUnnamedArgument(arg).into()),
            },
        }
    }
    Ok(())
}

pub struct ArgsIter<'a> {
    iter: &'a mut dyn Iterator<Item = OsString>,
    cur_input_arg: OsString,
    state: ArgsState,
}

enum ArgsState {
    Unnamed,
    Long { eq_pos: Option<NonZero<usize>> },
    Short { next_pos: usize },
}

enum Arg<'a> {
    DashDash,
    Named(&'a str),
    Unnamed(OsString),
}

impl<'a> ArgsIter<'a> {
    pub(crate) fn new(iter: &'a mut dyn Iterator<Item = OsString>) -> Self {
        Self {
            iter,
            cur_input_arg: OsString::new(),
            state: ArgsState::Unnamed,
        }
    }

    fn check_no_value(&mut self) -> Result<()> {
        // This only fail for long arguments with joined value: `--long=[..]`.
        if let ArgsState::Long { eq_pos: Some(pos) } = self.state {
            Err(ErrorKind::UnexpectedValue(
                self.cur_input_arg.index(pos.get() + 1..).to_os_string(),
            )
            .with_arg(self.cur_input_arg.index(..pos.get())))
        } else {
            Ok(())
        }
    }

    /// Retrieve the value for the current named argument after a mandatory "=".
    fn take_value_after_eq(&mut self) -> Result<&'_ OsStr> {
        match self.state {
            ArgsState::Long { eq_pos: Some(pos) } => Ok(self.cur_input_arg.index(pos.get() + 1..)),
            ArgsState::Short { next_pos }
                if self.cur_input_arg.as_encoded_bytes().get(next_pos) == Some(&b'=') =>
            {
                // Don't traverse the rest.
                self.state = ArgsState::Unnamed;
                Ok(self.cur_input_arg.index(next_pos + 1..))
            }
            // FIXME: Report error argument name.
            _ => Err(ErrorKind::MissingEq.into()),
        }
    }

    /// Retrieve the value for the current named argument, either after "=" or fetch from the next
    /// input argument.
    fn take_value(&mut self) -> Result<Cow<'_, OsStr>> {
        match self.state {
            ArgsState::Long { eq_pos: Some(pos) } => {
                Ok(Cow::Borrowed(self.cur_input_arg.index(pos.get() + 1..)))
            }
            ArgsState::Short { next_pos } => {
                let pos = if self.cur_input_arg.as_encoded_bytes().get(next_pos) == Some(&b'=') {
                    next_pos + 1
                } else {
                    next_pos
                };
                Ok(Cow::Borrowed(self.cur_input_arg.index(pos..)))
            }
            ArgsState::Long { eq_pos: None } => {
                let Some(arg) = self.iter.next() else {
                    let name = std::mem::take(&mut self.cur_input_arg);
                    return Err(ErrorKind::MissingValue.with_arg(name));
                };
                Ok(Cow::Owned(arg))
            }
            ArgsState::Unnamed => unreachable!(),
        }
    }

    /// Cache the next logical argument (short or long).
    fn cache_next_arg(&mut self) -> Result<Option<Arg<'_>>> {
        // Iterate the next short argument if we are inside a group of it.
        match self.state {
            ArgsState::Short { next_pos } if next_pos != self.cur_input_arg.len() => {
                let rest = &self.cur_input_arg.as_encoded_bytes()[next_pos..];
                // FIXME: More efficient way to get the next UTF-8 char?
                // UTF-8 char has encoded length 1..=4
                for len in 1..4 {
                    match std::str::from_utf8(&rest[..len]) {
                        Ok(s) => {
                            // Invariant: `prev_end..prev_end+len` is checked to be valid UTF-8.
                            self.state = ArgsState::Short {
                                next_pos: next_pos + len,
                            };
                            return Ok(Some(Arg::Named(s)));
                        }
                        // Incomplete encoding.
                        Err(e) if e.error_len().is_none() => {}
                        Err(_) => break,
                    }
                }
                return Err(ErrorKind::InvalidUtf8(
                    self.cur_input_arg.index(next_pos..).to_os_string(),
                )
                .into());
            }
            _ => {}
        }

        // Otherwise, fetch the next input argument.
        let Some(s) = self.iter.next() else {
            return Ok(None);
        };
        self.cur_input_arg = s;
        let argb = self.cur_input_arg.as_encoded_bytes();
        self.state = ArgsState::Unnamed;

        Ok(Some(if argb == b"-" {
            Arg::Unnamed(std::mem::take(&mut self.cur_input_arg))
        } else if argb == b"--" {
            Arg::DashDash
        } else if argb.starts_with(b"--") {
            let end = if let Some(pos) = argb.iter().position(|&b| b == b'=') {
                self.state = ArgsState::Long {
                    eq_pos: NonZero::new(pos),
                };
                pos
            } else {
                self.state = ArgsState::Long { eq_pos: None };
                argb.len()
            };
            let s = self.cur_input_arg.index(..end);
            Arg::Named(
                s.to_str()
                    .ok_or_else(|| ErrorKind::InvalidUtf8(s.to_os_string()))?,
            )
        } else if argb.starts_with(b"-") {
            self.state = ArgsState::Short { next_pos: 1 };
            return self.cache_next_arg();
        } else {
            Arg::Unnamed(std::mem::take(&mut self.cur_input_arg))
        }))
    }
}
