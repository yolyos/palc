use syn::parse_quote;

#[test]
fn smoke() {
    let foo = parse_quote! {
        struct MyCli {
            name: Option<String>,

            #[arg(short, long)]
            config: Option<PathBuf>,

            #[arg(short = 'v')]
            debug: bool,
        }
    };

    let output = crate::derive_parser::expand(foo);
    println!("{output}");
    let text = prettyplease::unparse(&syn::parse2(output).unwrap());
    println!("{text}");
}
