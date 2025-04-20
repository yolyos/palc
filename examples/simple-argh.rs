//! See `./util/one.rs`.
use std::path::PathBuf;

use argh::FromArgs;

#[expect(dead_code, reason = "fields are only for testing")]
#[derive(FromArgs)]
pub struct Cli {
    #[argh(switch, short = 'v')]
    verbose: bool,
    #[argh(positional)]
    file: PathBuf,
}

fn main() {
    let cli: Cli = argh::from_env();
    std::hint::black_box(&cli);
}
