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
        }
    }
}

impl ArgsInfo {
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NamedArgInfo {
    long_names: &'static [&'static str],
    short_names: &'static [&'static str],
    value_display: &'static str,
    require_eq: bool,
}

impl NamedArgInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(
        long_names: &'static [&'static str],
        short_names: &'static [&'static str],
        value_display: &'static str,
        require_eq: bool,
    ) -> Self {
        Self {
            long_names,
            short_names,
            value_display,
            require_eq,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnnamedArgInfo {
    value_display: &'static str,
}

impl UnnamedArgInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(value_display: &'static str) -> Self {
        Self { value_display }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CommandInfo {
    commands: &'static [(Option<&'static str>, &'static ArgsInfo)],
}

impl CommandInfo {
    // NB. Used by proc-macro.
    #[doc(hidden)]
    pub const fn __new(commands: &'static [(Option<&'static str>, &'static ArgsInfo)]) -> Self {
        Self { commands }
    }
}
