use expect_test::{Expect, expect};
use palc_derive::{Args, Parser, Subcommand};
use std::{ffi::OsString, fmt::Debug};

#[derive(Debug, Parser)]
struct CliEmpty {}

#[track_caller]
fn check<P: palc::Parser + Debug + PartialEq>(
    args: impl IntoIterator<Item = impl Into<OsString> + Clone>,
    expect: &P,
) {
    let got = P::try_parse_from(args).unwrap();
    assert_eq!(got, *expect);
}

#[track_caller]
fn check_err<P: palc::Parser + Debug>(
    args: impl IntoIterator<Item = impl Into<OsString> + Clone>,
    expect: Expect,
) {
    let ret = P::try_parse_from(args).unwrap_err();
    expect.assert_eq(&ret.to_string());
}

#[derive(Debug, Clone, PartialEq, Subcommand)]
enum Sub {
    Sub,
}

#[test]
fn argv0() {
    check_err::<CliEmpty>(None::<&str>, expect!["missing executable argument (argv[0])"]);
}

#[test]
fn short() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short)]
        verbose: bool,
        #[arg(short)]
        debug: bool,
        #[arg(short)]
        file: Option<String>,
    }

    check(["", "-dvf-"], &Cli { verbose: true, debug: true, file: Some("-".into()) });
    check(["", "-f=-", "-v"], &Cli { verbose: true, debug: false, file: Some("-".into()) });
    check(["", "-d", "-f", "-", "-v"], &Cli { verbose: true, debug: true, file: Some("-".into()) });
}

#[test]
fn require_equals() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(long, short, require_equals = true)]
        file: Option<String>,
        #[arg(short)]
        verbose: bool,
    }

    check_err::<Cli>(
        ["", "--file", "-"],
        expect!["equal sign is needed when assigning values for '--file'"],
    );
    check_err::<Cli>(
        ["", "-f", "-"],
        expect!["equal sign is needed when assigning values for '-f'"],
    );
    check_err::<Cli>(
        ["", "-vf", "-"],
        expect!["equal sign is needed when assigning values for '-f'"],
    );
    check_err::<Cli>(["", "-fv"], expect!["equal sign is needed when assigning values for '-f'"]);

    check(["", "-f="], &Cli { verbose: false, file: Some("".into()) });
    check(["", "-f=v"], &Cli { verbose: false, file: Some("v".into()) });
    check(["", "-vf=v"], &Cli { verbose: true, file: Some("v".into()) });
    check(["", "--file="], &Cli { verbose: false, file: Some("".into()) });
    check(["", "--file=v"], &Cli { verbose: false, file: Some("v".into()) });
}

#[test]
fn required() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        // TODO: Reject bool as unnamed arguments. It is almost always an typo.
        #[arg(long)]
        key: String,
        file: String,
        #[command(subcommand)]
        sub: Sub,
    }

    check_err::<Cli>(["", "path"], expect!["argument '--key' is required but not provided"]);
    check_err::<Cli>(
        ["", "--key", "value"],
        expect!["argument 'FILE' is required but not provided"],
    );
    check_err::<Cli>(
        ["", "--key", "value", "path"],
        expect!["subcommand is required but not provided"],
    );
    check_err::<Cli>(["", "path", "sub"], expect!["argument '--key' is required but not provided"]);

    check_err::<Cli>(
        ["", "--key", "value", "sub"],
        expect!["argument 'FILE' is required but not provided"],
    );

    let expect = Cli { key: "value".into(), file: "path".into(), sub: Sub::Sub };
    check(["", "path", "--key", "value", "sub"], &expect);
    check(["", "--key", "value", "path", "sub"], &expect);
}

#[test]
fn optional() {
    #[derive(Debug, Clone, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(long)]
        flag: bool,
        #[arg(long)]
        key: Option<String>,
        file: Option<String>,
        #[command(subcommand)]
        sub: Option<Sub>,
    }

    let default = Cli::default();
    check([""], &default);
    check(["", "--flag"], &Cli { flag: true, ..default.clone() });
    check(["", "--key", "value"], &Cli { key: Some("value".into()), ..default.clone() });
    check(["", "path"], &Cli { file: Some("path".into()), ..default.clone() });

    check(["", "sub"], &Cli { sub: Some(Sub::Sub), ..default.clone() });

    check(
        ["", "--key", "sub", "path"],
        &Cli { key: Some("sub".into()), file: Some("path".into()), ..default.clone() },
    );
    check(
        ["", "path", "sub"],
        &Cli { file: Some("path".into()), sub: Some(Sub::Sub), ..default.clone() },
    );

    check(
        ["", "--flag", "--key", "value", "path", "sub"],
        &Cli {
            flag: true,
            key: Some("value".into()),
            file: Some("path".into()),
            sub: Some(Sub::Sub),
        },
    );
}

#[test]
fn flatten() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(long)]
        verbose: bool,
        #[command(flatten)]
        config: Config,
        #[arg(long)]
        debug: bool,
    }

    #[derive(Debug, PartialEq, Args)]
    struct Config {
        #[arg(long)]
        config: Option<String>,
        #[arg(long)]
        config_file: Option<String>,
    }

    check(
        ["", "--debug", "--verbose", "--config", "a=b"],
        &Cli {
            debug: true,
            verbose: true,
            config: Config { config: Some("a=b".into()), config_file: None },
        },
    );
    check(
        ["", "--config-file", "path", "--debug", "--verbose"],
        &Cli {
            debug: true,
            verbose: true,
            config: Config { config_file: Some("path".into()), config: None },
        },
    );
}

#[test]
fn unknown_args() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short, long)]
        verbose: bool,
        file: String,
    }

    #[derive(Debug, PartialEq, Parser)]
    struct CliWithSub {
        #[arg(long)]
        verbose: bool,
        file: String,
        #[command(subcommand)]
        sub: Sub,
    }

    check_err::<Cli>(["", "--debug"], expect!["unexpected argument '--debug'"]);
    check_err::<Cli>(["", "-d"], expect!["unexpected argument '-d'"]);
    check_err::<Cli>(["", "-vd"], expect!["unexpected argument '-d'"]);
    check_err::<Cli>(["", "-v坏"], expect!["unexpected argument '-坏'"]);

    check_err::<Cli>(["", "path1", "path2"], expect!["unexpected argument 'path2'"]);

    check_err::<CliWithSub>(["", "path1", "path2"], expect!["unrecognized subcommand 'path2'"]);
    check_err::<CliWithSub>(["", "sub", "path"], expect!["unexpected argument 'path'"]);
}

#[cfg(unix)]
#[test]
fn non_utf8() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::{OsStrExt, OsStringExt};
    use std::path::PathBuf;

    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short, long)]
        config: Option<PathBuf>,
        file: Option<PathBuf>,
        #[arg(short)]
        verbose: bool,
    }

    fn concat_os(a: impl AsRef<OsStr>, b: impl AsRef<OsStr>) -> OsString {
        let mut buf = a.as_ref().as_bytes().to_vec();
        buf.extend_from_slice(b.as_ref().as_bytes());
        OsString::from_vec(buf)
    }

    let non_utf8 = || OsString::from_vec(vec![0xFF]);
    assert!(non_utf8().to_str().is_none());
    check(
        ["".into(), non_utf8()],
        &Cli { file: Some(non_utf8().into()), config: None, verbose: false },
    );

    let mut exp = Cli { config: Some(non_utf8().into()), file: None, verbose: false };
    check([OsString::new(), "--config".into(), non_utf8()], &exp);
    check([OsString::new(), concat_os("--config=", non_utf8())], &exp);
    check([OsString::new(), concat_os("-c", non_utf8())], &exp);
    check([OsString::new(), concat_os("-c=", non_utf8())], &exp);

    exp.verbose = true;
    check([OsString::new(), concat_os("-vc", non_utf8())], &exp);
    check([OsString::new(), concat_os("-vc=", non_utf8())], &exp);
}

#[test]
fn global() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short, global = true)]
        verbose: bool,
        #[arg(short, long, global = true)]
        debug: Option<u8>,
        #[command(subcommand)]
        sub: Sub2,
    }

    #[derive(Debug, PartialEq, Subcommand)]
    enum Sub2 {
        Empty,
        Deep {
            #[arg(short)]
            verbose: bool,
        },
    }

    check(["", "empty"], &Cli { verbose: false, debug: None, sub: Sub2::Empty });
    check(["", "-v", "empty"], &Cli { verbose: true, debug: None, sub: Sub2::Empty });
    check(["", "empty", "-v"], &Cli { verbose: true, debug: None, sub: Sub2::Empty });

    // TODO: Is this behavior expected?
    check(
        ["", "-v", "deep"],
        &Cli { verbose: true, debug: None, sub: Sub2::Deep { verbose: false } },
    );
    check(
        ["", "deep", "-v"],
        &Cli { verbose: false, debug: None, sub: Sub2::Deep { verbose: true } },
    );
    check(
        ["", "-v", "deep", "-v"],
        &Cli { verbose: true, debug: None, sub: Sub2::Deep { verbose: true } },
    );

    check(["", "-d2", "empty"], &Cli { verbose: false, debug: Some(2), sub: Sub2::Empty });
    check(["", "-d", "2", "empty"], &Cli { verbose: false, debug: Some(2), sub: Sub2::Empty });
    check(
        ["", "-d2", "deep"],
        &Cli { verbose: false, debug: Some(2), sub: Sub2::Deep { verbose: false } },
    );
    check(
        ["", "-d", "2", "deep"],
        &Cli { verbose: false, debug: Some(2), sub: Sub2::Deep { verbose: false } },
    );

    // FIXME: Duplicated arguments.
    // check_err::<Cli>(["", "-d2", "deep", "-d0"], expect![]);
}
