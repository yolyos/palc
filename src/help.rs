use std::fmt::Write;

use crate::refl::CommandInfo;

pub(crate) type SubcommandPath = Vec<(String, CommandInfo)>;

pub(crate) fn generate(path: &SubcommandPath, out: &mut String) -> std::fmt::Result {
    let ((last_cmd, cmd_info), ancestors) = path.split_first().unwrap();
    let Some(info) = cmd_info.catchall().or_else(|| {
        cmd_info
            .commands()
            .find_map(|(c, args)| (c == last_cmd).then_some(args))
    }) else {
        // Invalid last subcommand, help for all subcommands?
        todo!();
    };
    // Inside a valid subcommand.

    // About this (sub)command.
    let app = info.app().unwrap();
    let about = app.about();
    for s in about.all_paragraphs() {
        writeln!(out, "{s}")?;
    }
    writeln!(out)?;

    // Usage of current subcommand path.

    write!(out, "Usage:")?;
    for (subcmd, _) in ancestors.iter().rev() {
        write!(out, " {subcmd}")?;
    }
    write!(out, " {last_cmd}")?;

    let mut has_named @ mut has_unnamed = false;
    // TODO: Hide optional arguments?
    for arg in info.named_args() {
        has_named = true;
        let name = arg
            .long_names()
            .first()
            .or(arg.short_names().first())
            .unwrap();
        write!(out, " {name}")?;
        match arg.value_display() {
            [] => {}
            [v] => {
                let sep = if arg.requires_eq() { "=" } else { " " };
                write!(out, "{sep}<{v}>")?;
            }
            _ => todo!(),
        }
    }
    for arg in info.unnamed_args() {
        has_unnamed = true;
        write!(out, " <{}>", arg.value_display())?;
    }
    if let Some((_, vaarg)) = info.trailing_var_arg() {
        write!(out, " [{}]...", vaarg.value_display())?;
    }
    if let Some((optional, _)) = info.subcommand() {
        out.write_str(if optional { " [COMMAND]" } else { " <COMMAND>" })?;
    }
    writeln!(out)?;

    // List of commands.

    if let Some((_, cmd_info)) = info.subcommand() {
        writeln!(out, "\nCommands:")?;
        for (cmd, _) in cmd_info.commands() {
            writeln!(out, "    {cmd}")?;
        }
    }

    // List of unnamed arguments.

    if has_unnamed {
        writeln!(out, "\nArguments:")?;
        for (i, arg) in info.unnamed_args().enumerate() {
            if i > 0 {
                writeln!(out)?;
            }
            writeln!(out, "  <{}>", arg.value_display())?;
            if let Some(doc) = arg.doc() {
                for (i, s) in doc.all_paragraphs().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    writeln!(out, "          {s}")?;
                }
            }
        }
    }

    // List of named arguments.

    if has_named {
        writeln!(out, "\nOptions:")?;

        for arg in info.named_args() {
            match (arg.short_names().first(), arg.long_names().first()) {
                (None, None) => unreachable!(),
                (None, Some(long)) => write!(out, "      {long}"),
                (Some(short), None) => write!(out, "  {short}"),
                (Some(short), Some(long)) => write!(out, "  {short}, {long}"),
            }?;
            match arg.value_display() {
                [] => {}
                [v] => {
                    let sep = if arg.requires_eq() { "=" } else { " " };
                    write!(out, "{sep}<{v}>")?;
                }
                _ => todo!(),
            }
            writeln!(out)?;
            if let Some(doc) = arg.doc() {
                for (i, s) in doc.all_paragraphs().enumerate() {
                    if i != 0 {
                        writeln!(out)?;
                    }
                    writeln!(out, "          {s}")?;
                }
            }
            writeln!(out)?;
        }
    }

    if let Some(after) = app.after_help() {
        out.write_str(after)?;
    }

    Ok(())
}
