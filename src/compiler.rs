use std::collections::HashMap;
use std::path::{Path, PathBuf};

use simple_error::SimpleError;

use crate::parsing::{ebnf_parser, parser};
use crate::symbolization::ast::{self, Scope, Program};
use crate::symbolization::{meta_program, symbolizer};

pub struct ScsCompiler {
    parser: parser::Parser,
    symbolizer: symbolizer::Symbolizer,
    file_cache: HashMap<PathBuf, ast::Program>,
    parse_stack: Vec<PathBuf>,
}

impl ScsCompiler {
    pub fn build(definition: &str, xml_out: Option<std::fs::File>) -> Option<ScsCompiler> {
        let grammar = ebnf_parser::parse_ebnf(definition);
        if let Err(err) = grammar {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            return None;
        }

        let parser = parser::Parser::new(grammar.unwrap(), xml_out);
        if let Err(err) = parser {
            println!("Error creating parser: {}", err);
            return None;
        }

        Some(ScsCompiler {
            parser: parser.unwrap(),
            file_cache: HashMap::new(),
            parse_stack: Vec::new(),
        })
    }

    pub fn compile(&mut self, source_file: &Path) -> Option<&ast::Program> {
        let this_path =
            std::env::current_dir().expect("Could not get this program's current directory");

        if self.file_cache.contains_key(source_file) {
            // already compiled
            return self.file_cache.get(source_file);
        }

        if self.parse_stack.iter().any(|file| file == source_file) {
            print!("Recursive include : {:?}", self.parse_stack);
            return None;
        }

        let file_contents = std::fs::read_to_string(source_file)
            .map_err(SimpleError::from)
            .ok()?;
        let program_string = file_contents.replace("\r\n", "\n");

        let parse_result = self.parser.parse_program(&program_string);

        let syntax_tree = match parse_result {
            Ok(node) => node,
            Err(err) => {
                print!(
                    "Error parsing file {}: \n{}",
                    source_file.to_string_lossy(),
                    err.into_iter()
                        .map(|err| err.error_string(&program_string) + "\n---\n\n")
                        .collect::<String>()
                );
                return None;
            }
        };

        let mut files_to_compile =
            meta_program::extract_includes(&syntax_tree, &this_path).into_iter();

        let mut program = Program {
            definitions: Scope::new(&source_file.to_string_lossy()),
            main: None,
        };

        while let Some(include_file) = files_to_compile.next() {
            let include_file_name = include_file.to_string_lossy().to_string();
            let include_program = self.compile(&include_file)?;

            program.definitions.extend(include_program.definitions);
        }



        // start parsing the definitions?

        self.file_cache
            .insert(source_file.to_path_buf(), program);

        todo!()
    }
}
