use std::path::PathBuf;

use crate::compiler::FauxCompiler;

pub mod compiler;

mod parsing;
mod symbolization;

#[cfg(test)]
mod tests;

use clap::Parser;

/// Compiler for the Faux programming language
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CommandlineArguments {
    /// Path to the ebnf definition file to use when parsing
    #[arg(short, long, value_name = ".ebnf file")]
    definition: PathBuf,
    /// Path to the file to compile
    #[arg(short, long, value_name = ".faux file")]
    program: PathBuf,
    /// Optional path to a debug xml output
    #[arg(long)]
    xml: Option<PathBuf>,
}

fn main() {
    let args = CommandlineArguments::parse();

    let definition = {
        let file_contents = std::fs::read_to_string(&args.definition);
        if file_contents.is_err() {
            panic!("File {} can not be opened", args.definition.display())
        } else {
            file_contents.unwrap()
        }
    };

    let base_directory = std::env::current_dir().expect("Could not get this program's current directory");
    let program_file = base_directory.join(args.program);

    let xml_output_file = args.xml.map(std::fs::File::create).map(Result::ok).flatten();

    let mut compiler = FauxCompiler::build(&definition, xml_output_file).unwrap();
    let compile_result = compiler.compile(&program_file);
    match compile_result {
        Ok(program) => print!("{}", program.name),
        Err(simple_error) => print!("{}", simple_error),
    }

}
