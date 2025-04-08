use clap::{Parser, ValueEnum};

#[path = "./util/criterion.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}
