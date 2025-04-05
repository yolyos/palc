use std::path::PathBuf;

use clap_static::Parser;
use clap_static_derive::Parser;

#[derive(Debug, PartialEq, Parser)]
struct MyCli {
    name: Option<String>,

    #[arg(short, long)]
    config: Option<PathBuf>,

    #[arg(short = 'v')]
    debug: bool,
}

#[test]
fn parse() {
    let args = MyCli::try_parse_from(["foo", "--config", "foo", "bar"]).unwrap();
    assert_eq!(
        args,
        MyCli {
            name: Some("bar".into()),
            config: Some("foo".into()),
            debug: false,
        }
    );
}
