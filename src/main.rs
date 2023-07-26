

#[allow(dead_code)]
use crate::commandline_parser::ArgumentParser;
use crate::compiler::FauxCompiler;

pub mod compiler;

mod commandline_parser;
mod parsing;
mod symbolization;

#[cfg(test)]
mod tests;

fn main() {
    let mut arg_parser = ArgumentParser::new();
    
    let base_directory = std::env::current_dir().expect("Could not get this program's current directory");

    let definition = if let Some(file_name) = arg_parser.get_parameter("--definition") {
        let file_contents = std::fs::read_to_string(&file_name);
        if file_contents.is_err() {
            panic!("File {file_name} can not be opened")
        } else {
            file_contents.unwrap()
        }
    } else {
        panic!("missing --definition tag");
    };

    let program_file = arg_parser.get_parameter("--program").expect("missing --program tag");

    let xml_output_file = arg_parser.get_parameter("--xml").map(std::fs::File::create).map(Result::ok).flatten();

    let mut compiler = FauxCompiler::build(&definition, xml_output_file).unwrap();
    let compile_result = compiler.compile(&base_directory.join(program_file));
    match compile_result {
        Ok(program) => print!("{}", program.name),
        Err(simple_error) => print!("{}", simple_error),
    }

}
