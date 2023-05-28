use crate::commandline_parser::ArgumentParser;

pub mod compiler;

mod commandline_parser;
mod lexer;
mod scs_lexer;
mod parser;

#[cfg(test)]
mod tests;

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
