use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::marker::PhantomData;
use std::num::NonZero;
use std::ops::ControlFlow;

use os_str_bytes::OsStrBytesExt;
use ref_cast::RefCast;

use crate::error::ErrorKind;
use crate::refl::ArgsInfo;
use crate::values::ArgValueInfo;
use crate::{Args, Result, Subcommand};

use super::Error;

/// A named argument with its place attached as `&mut self`.
pub trait ArgPlace {
    fn num_values(&self) -> NumValues;

    fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error>;
}

/// The expected number of values for a named argument to take.
#[derive(Debug, Clone, Copy)]
pub enum NumValues {
    Zero,
    One { require_equals: bool },
}

#[inline(always)]
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
    let b = Box::leak(Box::new(FlagPlace));
    debug_assert_eq!(std::ptr::from_ref(b), std::ptr::dangling());
    b
}

pub fn place_for_vec<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool>(
    place: &mut Vec<T>,
    _: A,
) -> &'_ mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A, const REQUIRE_EQ: bool>(Vec<T>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool> ArgPlace for Place<T, A, REQUIRE_EQ> {
        fn num_values(&self) -> NumValues {
            NumValues::One {
                require_equals: REQUIRE_EQ,
            }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            self.0.push(A::parser()(value)?);
            Ok(())
        }
    }

    Place::<T, A, REQUIRE_EQ>::ref_cast_mut(place)
}

pub fn place_for_set_value<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool>(
    place: &mut Option<T>,
    _: A,
) -> &'_ mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A, const REQUIRE_EQ: bool>(Option<T>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>, const REQUIRE_EQ: bool> ArgPlace for Place<T, A, REQUIRE_EQ> {
        fn num_values(&self) -> NumValues {
            NumValues::One {
                require_equals: REQUIRE_EQ,
            }
        }

        fn feed(&mut self, value: Cow<'_, OsStr>) -> Result<(), Error> {
            self.0 = Some(A::parser()(value)?);
            Ok(())
        }
    }

    Place::<T, A, REQUIRE_EQ>::ref_cast_mut(place)
}

/// A greedy unnamed argument with its place attached as `&mut self`.
///
/// It always consumes all the rest arguments.
pub trait GreedyArgsPlace {
    // TODO: Maybe avoid splitting out the first argument?
    fn feed_greedy(&mut self, arg: OsString, _args: &mut ArgsIter<'_>) -> Result<()>;
}

pub fn place_for_trailing_var_arg<T, A: ArgValueInfo<T>>(
    place: &mut Vec<T>,
    _: A,
) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A>(Vec<T>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>> GreedyArgsPlace for Place<T, A> {
        fn feed_greedy(&mut self, mut arg: OsString, args: &mut ArgsIter<'_>) -> Result<()> {
            if let Some(high) = args.iter.size_hint().1 {
                self.0.reserve(1 + high);
            }
            loop {
                self.0.push(A::parser()(Cow::Owned(arg))?);
                arg = match args.iter.next() {
                    Some(arg) => arg,
                    None => return Ok(()),
                };
            }
        }
    }

    Ok(Some(Place::<T, A>::ref_cast_mut(place)))
}

pub fn place_for_subcommand<C: Subcommand>(place: &mut Option<C>) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
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

    Ok(Some(Place::<C>::ref_cast_mut(place)))
}

/// Break on a resolved place. Continue on unknown names.
/// So we can `?` in generated code of `command(flatten)`.
pub type FeedNamed<'s> = ControlFlow<&'s mut dyn ArgPlace>;

/// This should be an enum, but be this for `?` support, which is unstable to impl.
pub type FeedUnnamed<'s> = Result<Option<&'s mut dyn GreedyArgsPlace>, Option<Error>>;

pub trait ArgsInternal: Sized + 'static {
    type __State: ParserState<Output = Self>;
}

pub trait ParserState: ParserStateDyn {
    type Output;
    const ARGS_INFO: ArgsInfo;

    fn init() -> Self;
    fn finish(self) -> Result<Self::Output>;
}

pub trait ParserStateDyn: 'static {
    fn feed_named(&mut self, _name: &str) -> FeedNamed<'_> {
        ControlFlow::Continue(())
    }

    fn feed_unnamed(&mut self, _arg: &mut OsString, _is_last: bool) -> FeedUnnamed {
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
            Arg::DashDash => {
                drop(arg);
                for mut arg in &mut args.iter {
                    match state.feed_unnamed(&mut arg, true) {
                        Ok(None) => {}
                        Ok(Some(place)) => return place.feed_greedy(arg, args),
                        Err(Some(err)) => return Err(err),
                        Err(None) => return Err(ErrorKind::UnexpectedUnnamedArgument(arg).into()),
                    }
                }
            }
            Arg::Named(name) => match state.feed_named(name) {
                ControlFlow::Break(place) => match place.num_values() {
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
                ControlFlow::Continue(()) => {
                    return Err(ErrorKind::UnknownNamedArgument.with_arg(name));
                }
            },
            Arg::Unnamed(mut arg) => match state.feed_unnamed(&mut arg, false) {
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

#[derive(Debug)]
enum ArgsState {
    Unnamed,
    Long { eq_pos: Option<NonZero<usize>> },
    Short { next_pos: usize },
}

#[derive(Debug)]
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
            ArgsState::Short { next_pos } if next_pos < self.cur_input_arg.len() => {
                let pos = if self.cur_input_arg.as_encoded_bytes()[next_pos] == b'=' {
                    next_pos + 1
                } else {
                    next_pos
                };
                // Don't traverse the rest.
                self.state = ArgsState::Unnamed;
                Ok(Cow::Borrowed(self.cur_input_arg.index(pos..)))
            }
            _ => {
                let Some(arg) = self.iter.next() else {
                    let name = std::mem::take(&mut self.cur_input_arg);
                    return Err(ErrorKind::MissingValue.with_arg(name));
                };
                Ok(Cow::Owned(arg))
            }
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
