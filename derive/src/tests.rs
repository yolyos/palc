#![expect(clippy::print_stdout, reason = "Workaround: allowed in tests")]
use syn::parse_quote;

#[test]
fn derive_parser() {
    let input = parse_quote! {
        struct MyCli {
            name: Option<String>,

            #[arg(short, long)]
            config: Option<PathBuf>,

            #[arg(short = 'v')]
            debug: bool,

            #[command(subcommand)]
            command: Option<Command>,
        }
    };

    let output = crate::derive_args::expand(&input, true);
    println!("{output}");
    let text = prettyplease::unparse(&syn::parse2(output).unwrap());
    println!("{text}");
}

#[test]
fn derive_subcommand() {
    let input = parse_quote! {
        enum MyCommands {
            Foo {
                #[arg(short)]
                verbose: bool,
            },
            Bar {
                #[arg(long)]
                name: String,
            },
        }
    };

    let output = crate::derive_subcommand::expand(&input);
    println!("{output}");
    let text = prettyplease::unparse(&syn::parse2(output).unwrap());
    println!("{text}");
}
