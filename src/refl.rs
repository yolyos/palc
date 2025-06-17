#![expect(dead_code, reason = "TODO: Decide whether to public these APIs")]
// TODO: Better Debug impl for structs in this mod.

/// Description of a collection of arguments.
///
/// NB. This struct is constructed by proc-macro.
#[doc(hidden)]
#[derive(Debug)]
pub struct RawArgsInfo {
    /// Subcommand argument, if any.
    pub __subcommand: Option<&'static RawCommandInfo>,

    /// Zero or more '\0'-terminated raw `ArgInfo`.
    ///
    /// Each raw `ArgInfo` consists of following strings in order:
    /// - `required as u8`.
    /// - Example-like description, eg. `--key <VALUE>`, `-c, --color=<COLOR>`.
    /// - '\n'.
    /// - Help text.
    ///
    /// If either short or long names is non-empty, this is a named argument,
    /// otherwise it is an unnamed one.
    pub __raw_args: &'static str,

    /// The first byte is '1' if there is an optional subcommand, otherwise '0'.
    ///
    /// If command doc is enabled, there are additionally NUL-separated elements
    /// in following order:
    /// - `name`
    /// - `version`
    /// - `author`
    /// - `about`
    /// - `long_about`
    /// - `long_help`
    /// - `after_long_help`
    pub __raw_meta: &'static str,
}

impl RawArgsInfo {
    // NB. Used by proc-macro.
    pub const fn empty() -> Self {
        Self { __subcommand: None, __raw_args: "", __raw_meta: "0" }
    }
}

/// Description of a collection of arguments.
#[derive(Debug, Clone, Copy)]
pub struct ArgsInfo {
    raw_args: &'static str,
    subcommand: Option<&'static RawCommandInfo>,
    is_subcommand_optional: bool,
    raw_doc: &'static str,
}

impl ArgsInfo {
    fn from_raw(raw: &'static RawArgsInfo) -> Self {
        // See `RawArgsInfo`.
        let (fst, raw_doc) = raw.__raw_meta.split_at(1);
        Self {
            raw_args: raw.__raw_args,
            subcommand: raw.__subcommand,
            is_subcommand_optional: fst == "1",
            raw_doc,
        }
    }

    pub fn args(&self) -> impl Iterator<Item = ArgInfo> {
        // See `RawArgsInfo`.
        split_terminator(self.raw_args, b'\0').map(ArgInfo::from_raw)
    }

    pub fn named_args(&self) -> impl Iterator<Item = NamedArgInfo> {
        self.args().filter_map(ArgInfo::to_named)
    }

    pub fn unnamed_args(&self) -> impl Iterator<Item = UnnamedArgInfo> {
        self.args().filter_map(ArgInfo::to_unnamed)
    }

    pub fn subcommand(&self) -> Option<SubcommandInfo> {
        self.subcommand.map(SubcommandInfo::from_raw)
    }

    pub fn is_subcommand_optional(&self) -> bool {
        self.is_subcommand_optional
    }

    #[inline(never)]
    pub fn doc(&self) -> Option<CommandDoc> {
        // See `RawArgsInfo`.
        let [name, version, author, about, long_about, after_help, after_long_help] =
            split_sep_many(self.raw_doc, b'\0')?;
        Some(CommandDoc { name, version, author, about, long_about, after_help, after_long_help })
    }
}

/// Description of an arguments.
#[derive(Debug, Clone, Copy)]
pub enum ArgInfo {
    Named(NamedArgInfo),
    Unnamed(UnnamedArgInfo),
}

impl ArgInfo {
    // See `RawArgsInfo`.
    #[inline(never)]
    fn from_raw(raw: &'static str) -> Self {
        (|| {
            let [first, long_help] = split_sep_many(raw, b'\n')?;
            let (required_str, description) = first.split_at(1);
            let required = required_str == "1";
            Some(if description.starts_with('-') {
                Self::Named(NamedArgInfo { description, required, long_help })
            } else {
                Self::Unnamed(UnnamedArgInfo { description, required, long_help })
            })
        })()
        .unwrap()
    }

    pub fn to_named(self) -> Option<NamedArgInfo> {
        if let Self::Named(v) = self { Some(v) } else { None }
    }

    pub fn to_unnamed(self) -> Option<UnnamedArgInfo> {
        if let Self::Unnamed(v) = self { Some(v) } else { None }
    }
}

/// Description of a named argument.
#[derive(Debug, Clone, Copy)]
pub struct NamedArgInfo {
    required: bool,
    description: &'static str,
    long_help: &'static str,
}

impl NamedArgInfo {
    pub fn description(&self) -> &'static str {
        self.description
    }

    pub fn required(&self) -> bool {
        self.required
    }

    pub fn long_help(&self) -> Option<&'static str> {
        opt(self.long_help)
    }
}

/// Description of an unnamed (positional) argument.
#[derive(Debug, Clone, Copy)]
pub struct UnnamedArgInfo {
    description: &'static str,
    required: bool,
    long_help: &'static str,
}

impl UnnamedArgInfo {
    pub fn description(&self) -> &'static str {
        self.description
    }

    pub fn required(&self) -> bool {
        self.required
    }

    pub fn long_help(&self) -> Option<&'static str> {
        opt(self.long_help)
    }
}

/// NB. This struct is constructed by proc-macro.
///
/// - For regular `derive(Subcommand)` enums, `__raw_names` has the same number
///   of names as `__subcommands`.
/// - For `derive(Parser)` structs, `__raw_names` is empty and `__subcommands`
///   has exactly one element. Note that in this case, this struct cannot be
///   used as a subcommand of other `derive(Args)` structs.
#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub struct RawCommandInfo {
    /// '\t'-terminated subcommand names.
    pub __raw_names: &'static str,
    /// Argument of each subcommand.
    pub __subcommands: &'static [RawArgsInfo],
}

impl RawCommandInfo {
    // NB. Used by proc-macro.
    pub const fn empty() -> Self {
        Self { __raw_names: "", __subcommands: &[] }
    }

    // pub(crate) fn find_subcommand(&self)
}

/// Description of a command applet.
#[derive(Debug, Clone, Copy)]
pub enum CommandInfo {
    /// A top-level program-name-agnostic `Parser` struct.
    RootArgs(ArgsInfo),
    Subcommand(SubcommandInfo),
}

impl CommandInfo {
    pub(crate) fn from_raw(raw: &'static RawCommandInfo) -> Self {
        if raw.__raw_names.is_empty() {
            Self::RootArgs(ArgsInfo::from_raw(&raw.__subcommands[0]))
        } else {
            Self::Subcommand(SubcommandInfo::from_raw(raw))
        }
    }
}

/// Description of a subcommand enum.
#[derive(Debug, Clone, Copy)]
pub struct SubcommandInfo(&'static RawCommandInfo);

impl SubcommandInfo {
    pub(crate) fn from_raw(raw: &'static RawCommandInfo) -> Self {
        Self(raw)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static str, ArgsInfo)> {
        std::iter::zip(
            split_terminator(self.0.__raw_names, b'\t'),
            self.0.__subcommands.iter().map(ArgsInfo::from_raw),
        )
    }

    pub fn get(&self, subcmd: &str) -> Option<ArgsInfo> {
        Some(self.iter().find(|(name, _)| *name == subcmd)?.1)
    }
}

/// Help and documentation of a command applet.
#[derive(Debug, Clone, Copy)]
pub struct CommandDoc {
    name: &'static str,
    version: &'static str,
    author: &'static str,
    about: &'static str,
    long_about: &'static str,
    after_help: &'static str,
    after_long_help: &'static str,
}

impl CommandDoc {
    pub fn name(&self) -> &str {
        self.name
    }

    pub fn version(&self) -> Option<&str> {
        opt(self.version)
    }

    pub fn author(&self) -> Option<&str> {
        opt(self.author)
    }

    pub fn about(&self) -> Option<&str> {
        opt(self.about)
    }

    pub fn long_about(&self) -> Option<&str> {
        opt(self.long_about)
    }

    pub fn after_help(&self) -> Option<&str> {
        opt(self.after_help)
    }

    pub fn after_long_help(&self) -> Option<&str> {
        opt(self.after_long_help)
    }
}

fn opt(s: &str) -> Option<&str> {
    if s.is_empty() { None } else { Some(s) }
}

#[inline(never)]
fn split_once(s: &str, b: u8) -> Option<(&str, &str)> {
    assert!(b.is_ascii());
    s.split_once(b as char)
}

fn split_sep_many<const N: usize>(mut s: &str, b: u8) -> Option<[&str; N]> {
    assert!(b.is_ascii());
    let mut arr = [""; N];
    let (last, init) = arr.split_last_mut().unwrap();
    for p in init {
        (*p, s) = split_once(s, b)?;
    }
    *last = s;
    Some(arr)
}

fn split_terminator(mut s: &str, b: u8) -> impl Iterator<Item = &str> {
    assert!(b.is_ascii());
    std::iter::from_fn(move || {
        let (fst, rest) = split_once(s, b)?;
        s = rest;
        Some(fst)
    })
}
