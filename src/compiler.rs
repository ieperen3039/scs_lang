use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use simple_error::SimpleError;

use crate::parsing::lexer::Lexer;
use crate::parsing::{ebnf_parser, parser, lexer};
use crate::symbolization::ast::{self, Program};
use crate::symbolization::type_collector::TypeCollector;
use crate::symbolization::{meta_program, symbolizer, built_in_types};

pub struct FauxCompiler {
    parser: Rc<parser::Parser>,
    lexer: lexer::Lexer,
    parse_stack: Vec<PathBuf>,
    file_cache: HashMap<PathBuf, Rc<ast::Program>>,
    type_collector: TypeCollector
}

impl FauxCompiler {
    pub fn build(definition: &str, xml_out: Option<std::fs::File>) -> Option<FauxCompiler> {
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

        Some(FauxCompiler {
            parser: Rc::from(parser.unwrap()),
            file_cache: HashMap::new(),
            parse_stack: Vec::new(),
            type_collector: TypeCollector::new(),
            lexer: Lexer {},
        })
    }

    pub fn compile(&mut self, source_file: &Path) -> Result<ast::Program, SimpleError> {
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
                let this_program = self.compile(&include_file)?;
                self.file_cache.insert(include_file.clone(), Rc::from(this_program));
            };

            let include_program = self.file_cache.get(&include_file).expect("if-statement above should compile on-demand");
            included_scope.extend(include_program.namespaces.clone());
        }

        // start parsing the definitions?
        let root_scope = symbolizer::parse_symbols(syntax_tree, &included_scope, &mut self.type_collector)?;

        Ok(Program {
            name: source_file.to_string_lossy().to_string(),
            main: None,
            namespaces: root_scope,
            type_definitions: todo!(),
            member_function_definitions: todo!(),
            function_definitions: todo!(),
        })
    }
}
