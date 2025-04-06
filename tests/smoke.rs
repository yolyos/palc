use std::path::PathBuf;

use clap_static::{Parser, Subcommand};

#[derive(Debug, PartialEq, Parser)]
struct MyCli {
    name: Option<String>,

    #[arg(short, long)]
    config: Option<PathBuf>,

    #[arg(short = 'v')]
    debug: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, PartialEq, Subcommand)]
enum Commands {
    Test {
        #[arg(short, long)]
        list: bool,
    },
}

#[test]
fn smoke() {
    let args = MyCli::try_parse_from(["foo", "--config", "foo", "bar", "test", "-l"]).unwrap();
    assert_eq!(
        args,
        MyCli {
            name: Some("bar".into()),
            config: Some("foo".into()),
            debug: false,
            command: Some(Commands::Test { list: true }),
        }
    );
}
