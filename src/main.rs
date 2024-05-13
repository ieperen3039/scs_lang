use std::path::PathBuf;

use crate::compiler::FauxCompiler;

pub mod compiler;

mod parsing;
mod symbolization;

#[cfg(test)]
mod tests;
pub mod transforming;
pub mod transpilation;

use clap::Parser;

/// Compiler for the Faux programming language
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CommandlineArguments {
    /// Path to the ebnf definition file to use when parsing
    #[arg(long, value_name = ".ebnf file")]
    definition: Option<PathBuf>,
    /// Path to the file to compile
    #[arg(short, long, value_name = ".faux file")]
    file: Option<PathBuf>,
    /// Optional path to a debug xml output
    #[arg(long)]
    debug_out: Option<PathBuf>,
}

fn main() {
    let args = CommandlineArguments::parse();

    let is_interactive = args.file.is_some();

    let definition = args
        .definition
        .map(|definition| {
            std::fs::read_to_string(definition).expect("Could not read definition input file")
        })
        .unwrap_or_else(|| {
            if is_interactive {
                String::from(include_str!("../doc/faux_script.ebnf"))
            } else {
                String::from(include_str!("../doc/definition.ebnf"))
            }
        });

    let xml_output_file = args.debug_out.map(|debug_out| {
        std::fs::File::create(debug_out).expect("Could not create debug output file")
    });

    let mut compiler =
        FauxCompiler::build(&definition, xml_output_file).expect("Could not build compiler");

    if let Some(script_file) = args.file {
        println!("Parsing {} in script mode", script_file.display());

        let base_directory =
            std::env::current_dir().expect("Could not get this program's current directory");

        let compile_result = compiler.compile(&base_directory.join(script_file));

        match compile_result {
            Ok(program) => {
                let mut out = std::io::stdout();
                let write = transpilation::generator_c::GeneratorC::write(&mut out, program);
                write.unwrap();
            }
            Err(simple_error) => print!("{}", simple_error),
        }
    } else {
        println!(
            "Faux version {} interactive mode (try `help()` for more information)",
            env!("CARGO_PKG_VERSION")
        );

        let mut buffer = String::new();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Error reading input");

        println!("{buffer}");
    }
}
