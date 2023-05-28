use scs_lexer::create_scs_lexer;

use crate::commandline_parser::ArgumentParser;

mod commandline_parser;
pub mod lexer;

#[cfg(test)]
mod tests;
pub mod scs_lexer;

fn main() {
    let mut arg_parser = ArgumentParser::new();

    if let Some(file_name) = arg_parser.get_parameter("--file") {
        let file_contents = std::fs::read_to_string(&file_name);
        if file_contents.is_err() {
            println!("File {file_name} can not be opened")
        } else {
            
        }
    }
}
