use crate::{
    refl::{CommandInfo, RawCommandInfo},
    runtime::CommandInternal,
};

/// List of (arg_info, subcommand) context, in reverse order.
/// The last element (outermost command) is the top-level command.
pub(crate) type SubcommandPath = Vec<SubcommandPathFrag>;
pub(crate) type SubcommandPathFrag = (&'static RawCommandInfo, String);

#[inline(never)]
fn push_str(out: &mut String, s: &str) {
    out.push_str(s);
}

#[cold]
pub(crate) fn render_help_into(out: &mut String, rev_path: &[SubcommandPathFrag]) {
    macro_rules! w {
        ($($e:expr),*) => {{
            $(push_str(out, $e);)*
        }};
    }

    let path = rev_path
        .iter()
        .rev()
        .filter_map(|(raw, cmd)| match CommandInfo::from_raw(raw) {
            CommandInfo::RootArgs(args) => Some((cmd.as_str(), args)),
            CommandInfo::Subcommand(subcmds) => Some((cmd, subcmds.get(cmd)?)),
        })
        .collect::<Vec<_>>();

    // There must be at least a top-level `Parser` info, or we would fail fast by `MissingArg0`.
    assert!(!path.is_empty());
    let info = path.last().unwrap().1;

    // About this (sub)command.
    let cmd_doc = info.doc().expect("doc is enabled");
    if let Some(about) = cmd_doc.long_about() {
        w!(about, "\n");
    }
    w!("\n");

    // Usage of current subcommand path.

    w!("Usage:");
    // Argv0 is included.
    for (cmd, _) in path.iter() {
        w!(" ", cmd);
    }

    let mut has_named @ mut has_opt_named @ mut has_unnamed = false;
    for arg in info.named_args() {
        has_named = true;
        if arg.required() {
            let mut desc = arg.description();
            if matches!(desc.as_bytes(), [b'-', short, _, _, ..] if *short != b'-') {
                // `-s, --long <VAL>` => `--long <VAL>`
                desc = &desc[4..];
            }
            w!(" ", desc);
        } else {
            has_opt_named = true;
        }
    }
    if has_opt_named {
        w!(" [OPTIONS]");
    }
    for arg in info.unnamed_args() {
        has_unnamed = true;
        w!(" ", arg.description());
    }
    let subcmd = info.subcommand();
    if subcmd.is_some() {
        w!(if info.is_subcommand_optional() { " [COMMAND]" } else { " <COMMAND>" });
    }
    w!("\n");

    // List of commands.

    if let Some(subcmd) = &subcmd {
        w!("\nCommands:\n");
        for (cmd, _) in subcmd.iter() {
            // TODO: Description.
            w!("    ", cmd, "\n");
        }
    }

    // List of unnamed arguments.

    if has_unnamed {
        w!("\nArguments:\n");
        let mut first = true;
        for arg in info.unnamed_args() {
            if first {
                first = false;
            } else {
                w!("\n");
            }
            w!("  ", arg.description());
            if let Some(help) = arg.long_help() {
                for (j, s) in help.split_terminator('\n').enumerate() {
                    if j > 0 {
                        w!("\n");
                    }
                    w!("          ", s, "\n");
                }
            }
        }
    }

    // List of named arguments.

    if has_named {
        w!("\nOptions:\n");

        for arg in info.named_args() {
            let padding = if arg.description().starts_with("--") { "      " } else { "  " };
            w!(padding, arg.description(), "\n");
            if let Some(help) = arg.long_help() {
                for (j, s) in help.split_terminator('\n').enumerate() {
                    if j != 0 {
                        w!("\n");
                    }
                    w!("          ", s, "\n");
                }
            }
            w!("\n");
        }
    }

    if let Some(after) = cmd_doc.after_long_help() {
        w!(after);
    }
}

pub(crate) fn render_help_for<C: CommandInternal>(argv0: impl Into<String>) -> String {
    let mut out = String::new();
    render_help_into(&mut out, &[(C::RAW_COMMAND_INFO, argv0.into())]);
    out
}
