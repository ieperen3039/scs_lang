use std::{path::{Path, PathBuf}, rc::Rc, collections::HashMap};

use simple_error::SimpleError;

use crate::{parsing::{parser, lexer}, symbolization::{type_collector, meta_program, built_in_types}};

struct Program {
} 

pub struct Transpiler {
    parser: Rc<parser::Parser>,
    lexer: lexer::Lexer,
    parse_stack: Vec<PathBuf>,
    file_cache: HashMap<PathBuf, String>,
    type_collector: type_collector::TypeCollector
}

impl Transpiler {
    pub fn generate(&mut self, source_file: &Path, ) -> Result<String, SimpleError> {
        if self.parse_stack.iter().any(|file| file == source_file) {
            return Err(SimpleError::new(format!("Recursive include : {:?}", self.parse_stack)));
        }

        let file_contents = std::fs::read_to_string(source_file)
            .map_err(SimpleError::from)?;
        let program_string = file_contents.replace("\r\n", "\n");

        let tokens = self.lexer.read_all(&program_string).map_err(|char_idx| {
            SimpleError::new(parser::Failure::LexerError { char_idx, }.error_string(&program_string))
        })?;

        // we clone the parser rc, because parse_program returns a result that borrows from this parser.
        // if we don't clone, the borrow prevents us from recursively parse includes before using the parse_result
        let parser = &self.parser.clone();
        let parse_result = parser.parse_program(&tokens);

        let syntax_tree = match parse_result {
            Ok(node) => node,
            Err(err) => {
                return Err(SimpleError::new(format!("Error parsing file {}: \n{}",
                    source_file.to_string_lossy(),
                    err.into_iter()
                        .map(|err| err.error_string(&program_string) + "\n---\n\n")
                        .collect::<String>()
                )));
            }
        };

        let this_path =
            std::env::current_dir().expect("Could not get this program's current directory");
        let files_to_compile =
            meta_program::extract_includes(&syntax_tree, &this_path);

        let mut included_scope = built_in_types::get_implicit();

        for include_file in files_to_compile {
            if !self.file_cache.contains_key(&include_file) {
                self.parser.parse_program()

                self.file_cache.insert(include_file.clone(), Rc::from(this_program));
            };

            let include_program = self.file_cache.get(&include_file).expect("if-statement above should compile on-demand");
            included_scope.extend(include_program.namespaces.clone());
        }

        return Ok
    }
}