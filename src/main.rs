#[allow(dead_code)]
use crate::commandline_parser::ArgumentParser;
use crate::compiler::ScsCompiler;

pub mod compiler;

mod commandline_parser;
mod parsing;

#[cfg(test)]
mod tests;

fn main() {
    let mut arg_parser = ArgumentParser::new();

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

    let program = if let Some(file_name) = arg_parser.get_parameter("--program") {
        let file_contents = std::fs::read_to_string(&file_name);
        if file_contents.is_err() {
            panic!("File {file_name} can not be opened")
        } else {
            file_contents.unwrap().replace("\r\n", "\n")
        }
    } else {
        panic!("missing --program tag");
    };

    let xml_output_file = arg_parser.get_parameter("--xml").map(std::fs::File::create).map(Result::ok).flatten();

    let compiler = ScsCompiler::build(&definition, xml_output_file).unwrap();

    if let Some(result) = compiler.compile(&program) {
        print!("{:?}", result);
    }

}
