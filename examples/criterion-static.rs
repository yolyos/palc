use clap_static::Parser as _;
use clap_static_derive::{Parser, ValueEnum};

#[allow(dead_code)]
#[path = "./util/criterion.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}
