#![cfg_attr(
    not(feature = "help"),
    expect(unused, reason = "some getters are only used by help")
)]
const MAX_ITER_DEPTH: usize = 4;

/// Description of a self-contained applet.
///
/// Only available via `derive(Parser, Subcommand)`, not `derive(Args)`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AppInfo {
    name: &'static str,
    version: Option<&'static str>,
    author: Option<&'static str>,
    about: Option<&'static str>,
    long_about: Option<&'static str>,
}

impl AppInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(
        name: &'static str,
        version: &'static str,
        author: &'static str,
        about: &'static str,
        long_about: &'static str,
    ) -> Self {
        // Workaround: `bool::then_some` is not a const fn.
        macro_rules! opt {
            ($e:expr) => {
                if $e.is_empty() { None } else { Some($e) }
            };
        }
        Self {
            name,
            version: opt!(version),
            author: opt!(author),
            about: opt!(about),
            long_about: opt!(long_about),
        }
    }
}

impl AppInfo {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn version(&self) -> Option<&'static str> {
        self.version
    }

    pub fn author(&self) -> Option<&'static str> {
        self.author
    }

    pub fn about(&self) -> Doc {
        Doc {
            summary: self.about.unwrap_or(""),
            full: self.long_about.unwrap_or(""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArgsInfo {
    direct_named_args: &'static [NamedArgInfo],
    direct_unnamed_args: &'static [UnnamedArgInfo],
    last_arg: Option<UnnamedArgInfo>,
    // (greedy, arg)
    trailing_var_arg: Option<(bool, UnnamedArgInfo)>,
    // (optional, subcommand)
    subcommand: Option<(bool, CommandInfo)>,
    flatten_args: &'static [&'static ArgsInfo],
    total_named_arg_cnt: usize,
    total_unnamed_arg_cnt: usize,

    app: Option<&'static AppInfo>,
}

impl ArgsInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(
        direct_named_args: &'static [NamedArgInfo],
        direct_unnamed_args: &'static [UnnamedArgInfo],
        mut trailing_var_arg: Option<(bool, UnnamedArgInfo)>,
        mut subcommand: Option<(bool, CommandInfo)>,
        mut last_arg: Option<UnnamedArgInfo>,
        flatten_args: &'static [&'static ArgsInfo],
        app: Option<&'static AppInfo>,
    ) -> Self {
        let mut total_named_arg_cnt = direct_named_args.len();
        let mut total_unnamed_arg_cnt = direct_unnamed_args.len();
        let mut i = 0;
        while i < flatten_args.len() {
            let flatten = flatten_args[i];
            total_named_arg_cnt += flatten.total_named_arg_cnt;
            total_unnamed_arg_cnt += flatten.total_unnamed_arg_cnt;
            if let Some(last) = flatten.last_arg {
                assert!(
                    last_arg.replace(last).is_none(),
                    "duplicated arg(last) from command(flatten) Args",
                );
            }
            if let Some(vaarg) = flatten.trailing_var_arg {
                assert!(
                    trailing_var_arg.replace(vaarg).is_none(),
                    "duplicated variable-length positional arguments from command(flatten) Args",
                );
            }
            if let Some(subcmd) = flatten.subcommand {
                assert!(
                    subcommand.replace(subcmd).is_none(),
                    "duplicated subcommand from command(flatten) Args",
                );
            }
            i += 1;
        }
        assert!(
            trailing_var_arg.is_some() as u8 + subcommand.is_some() as u8 <= 1,
            "variable-length positional arguments conflicts with subcommands",
        );

        Self {
            direct_named_args,
            direct_unnamed_args,
            trailing_var_arg,
            subcommand,
            last_arg,
            flatten_args,
            total_named_arg_cnt,
            total_unnamed_arg_cnt,
            app,
        }
    }

    // NB. Used by proc-macro.
    pub const fn empty() -> Self {
        Self {
            direct_named_args: &[],
            direct_unnamed_args: &[],
            trailing_var_arg: None,
            subcommand: None,
            last_arg: None,
            flatten_args: &[],
            total_named_arg_cnt: 0,
            total_unnamed_arg_cnt: 0,
            app: None,
        }
    }

    pub fn named_args(&self) -> impl Iterator<Item = &NamedArgInfo> {
        let mut stack = [None; MAX_ITER_DEPTH];
        let mut top = 0usize;
        stack[0] = Some((self, 0usize));
        std::iter::from_fn(move || {
            loop {
                let (info, i) = stack[top].as_mut()?;
                if *i < info.direct_named_args.len() {
                    *i += 1;
                    return Some(&info.direct_named_args[*i - 1]);
                }
                if let Some(deep) = info.flatten_args.get(*i - info.direct_named_args.len()) {
                    if top == MAX_ITER_DEPTH {
                        panic!("arg(flatten) too deep");
                    }
                    *i += 1;
                    top += 1;
                    stack[top] = Some((deep, 0));
                } else if top == 0 {
                    return None;
                } else {
                    top -= 1;
                }
            }
        })
    }

    pub fn unnamed_args(&self) -> impl Iterator<Item = &UnnamedArgInfo> {
        let mut stack = [None; MAX_ITER_DEPTH];
        let mut top = 0usize;
        stack[0] = Some((self, 0usize));
        std::iter::from_fn(move || {
            loop {
                let (info, i) = stack[top].as_mut()?;
                if *i < info.direct_unnamed_args.len() {
                    *i += 1;
                    return Some(&info.direct_unnamed_args[*i - 1]);
                }
                if let Some(deep) = info.flatten_args.get(*i - info.direct_unnamed_args.len()) {
                    if top == MAX_ITER_DEPTH {
                        panic!("arg(flatten) too deep");
                    }
                    *i += 1;
                    top += 1;
                    stack[top] = Some((deep, 0));
                } else if top == 0 {
                    return None;
                } else {
                    top -= 1;
                }
            }
        })
    }

    pub(crate) fn trailing_var_arg(&self) -> Option<(bool, UnnamedArgInfo)> {
        self.trailing_var_arg
    }

    pub(crate) fn subcommand(&self) -> Option<(bool, CommandInfo)> {
        self.subcommand
    }

    pub fn app(&self) -> Option<&AppInfo> {
        self.app
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NamedArgInfo {
    long_names: &'static [&'static str],
    short_names: &'static [&'static str],
    value_display: &'static [&'static str],
    require_eq: bool,
    doc: &'static str,
}

impl NamedArgInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(
        doc: &'static str,
        long_names: &'static [&'static str],
        short_names: &'static [&'static str],
        value_display: &'static [&'static str],
        require_eq: bool,
    ) -> Self {
        Self {
            long_names,
            short_names,
            value_display,
            require_eq,
            doc,
        }
    }

    pub fn long_names(&self) -> &[&str] {
        self.long_names
    }

    pub fn short_names(&self) -> &[&str] {
        self.short_names
    }

    pub fn value_display(&self) -> &[&str] {
        self.value_display
    }

    pub fn requires_eq(&self) -> bool {
        self.require_eq
    }

    pub fn doc(&self) -> Option<Doc> {
        Doc::from_raw(self.doc)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnnamedArgInfo {
    value_display: &'static str,
    doc: &'static str,
}

impl UnnamedArgInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(doc: &'static str, value_display: &'static str) -> Self {
        Self { value_display, doc }
    }

    pub fn value_display(&self) -> &str {
        self.value_display
    }

    pub fn doc(&self) -> Option<Doc> {
        Doc::from_raw(self.doc)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CommandInfo {
    commands: &'static [(&'static str, &'static ArgsInfo)],
    catchall: Option<&'static ArgsInfo>,
}

impl CommandInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(commands: &'static [(&'static str, &'static ArgsInfo)]) -> Self {
        Self {
            commands,
            catchall: None,
        }
    }

    pub(crate) const fn new_catchall(catchall: &'static ArgsInfo) -> Self {
        Self {
            commands: &[],
            catchall: Some(catchall),
        }
    }

    pub(crate) fn catchall(&self) -> Option<&ArgsInfo> {
        self.catchall
    }

    pub(crate) fn commands(&self) -> impl ExactSizeIterator<Item = (&str, &ArgsInfo)> {
        self.commands.iter().copied()
    }
}

// See `Doc` in `clap-static-derive/src/common.rs` for proc-macro pre-processing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Doc {
    summary: &'static str,
    full: &'static str,
}

impl Doc {
    fn from_raw(doc: &'static str) -> Option<Self> {
        if doc.is_empty() {
            return None;
        }
        let (summary, _) = doc.split_once("\n").unwrap_or((doc, ""));
        Some(Doc { summary, full: doc })
    }

    pub fn summary(&self) -> &str {
        self.summary
    }

    pub fn all_paragraphs(&self) -> impl Iterator<Item = &str> {
        self.full.lines()
    }
}
