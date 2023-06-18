use std::collections::HashMap;
use std::path::{Path, PathBuf};

use simple_error::SimpleError;

use crate::parsing::{ebnf_parser, parser};
use crate::symbolization::ast;
use crate::symbolization::syntax_tree_converter;

pub struct ScsCompiler {
    parser: parser::Parser,
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

        let syntax_tree = parse_result
            .map_err(|err| {
                print!(
                    "Error parsing file {}: \n{}",
                    source_file.to_string_lossy(),
                    err.into_iter()
                        .map(|err| err.error_string(&program_string) + "\n---\n\n")
                        .collect::<String>()
                );
                err
            })
            .ok()?;

        let files_to_compile =
            syntax_tree_converter::extract_includes(&syntax_tree, &this_path).into_iter();

        let mut types: HashMap<String, ast::TypeRef> = HashMap::new();
        let mut functions: HashMap<String, ast::FunctionRef> = HashMap::new();

        while let Some(include_file) = files_to_compile.next() {
            let include_file_name = include_file.to_string_lossy().to_string();
            let mut include_program = self.compile(&include_file)?;

            for (k, v) in include_program.types.drain() {
                types.insert(k, v);
            }

            for (k, v) in include_program.functions.drain() {
                functions.insert(k, v);
            }

            functions.insert( include_file_name, include_program.main );
        }

        let program_ast: ast::Program;

        self.file_cache.insert(source_file.to_path_buf(), program_ast);

        todo!()
    }
}
