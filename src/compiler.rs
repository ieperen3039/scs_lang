use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use simple_error::{SimpleError, SimpleResult};

use crate::symbolization::ast::Namespace;
use crate::symbolization::{ast, type_collector::TypeCollector, meta_program, symbolizer};
use crate::parsing::{lexer::{Lexer, self}, ebnf_parser, parser, left_left_parser};
use crate::transformation::grammatificator;

pub struct FauxCompiler {
    parser: Rc<left_left_parser::Parser>,
    lexer: lexer::Lexer,
    parse_stack: Vec<Box<Path>>,
    file_cache: HashMap<Box<Path>, Rc<ast::Program>>,
    type_collector: TypeCollector
}

impl FauxCompiler {
    pub fn build(definition: &str, xml_out: Option<std::fs::File>) -> Option<FauxCompiler> {
        let grammar = match ebnf_parser::parse_ebnf(definition) {
            Ok(ebnf_grammar) => grammatificator::convert_to_grammar(ebnf_grammar),
            Err(err) => {
                println!(
                    "Error parsing EBNF definition: {}",
                    ebnf_parser::error_string(&err, definition)
                );
                return None;
            }
        };

        let parser = left_left_parser::Parser::new(grammar, xml_out);

        Some(FauxCompiler {
            parser: Rc::from(parser),
            file_cache: HashMap::new(),
            parse_stack: Vec::new(),
            type_collector: TypeCollector::new(),
            lexer: Lexer { ignore_whitespace: true },
        })
    }

    pub fn compile(&mut self, source_file: &Path) -> SimpleResult<ast::Program> {
        if self.parse_stack.iter().any(|file| file.as_ref() == source_file) {
            return Err(SimpleError::new(format!("Recursive include : {:?}", self.parse_stack)));
        }
        self.parse_stack.push(Box::from(source_file));

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

        let mut included_scope = Namespace::new("root", None);

        for include_file in files_to_compile {
            if !self.file_cache.contains_key(include_file.as_path()) {
                let this_program = self.compile(&include_file)?;
                self.file_cache.insert(Box::from(include_file.clone()), Rc::from(this_program));
            };

            let include_program = self.file_cache.get(include_file.as_path()).expect("if-statement above should compile on-demand");
            included_scope.extend(include_program.namespaces.clone());
        }

        // we have resolved and compiled all includes.
        // now we can start parsing this file
        let program = symbolizer::parse_symbols(syntax_tree, &included_scope, &mut self.type_collector)?;
        Ok(program)
    }
}
