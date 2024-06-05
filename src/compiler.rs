use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use simple_error::{simple_error, SimpleError, SimpleResult};

use crate::built_in::functions::InternalFunctions;
use crate::built_in::types::InternalTypes;
use crate::built_in::{functions, primitives, types};
use crate::symbolization::ast::Namespace;
use crate::symbolization::{ast, meta_program, symbolizer, type_collector::TypeCollector};
use crate::transformation::grammatificator;
use crate::{
    parsing::{
        ebnf_parser, left_left_parser,
        lexer::{self, Lexer},
        parser,
    },
    symbolization::function_collector::FunctionCollector,
};

pub struct FauxCompiler {
    parser: Rc<left_left_parser::Parser>,
    lexer: lexer::Lexer,
    parse_stack: Vec<Box<Path>>,
    file_cache: HashMap<Box<Path>, Rc<ast::FileAst>>,
    type_collector: TypeCollector,
    function_collector: FunctionCollector,
    built_in_functions: InternalFunctions,
    built_in_types: InternalTypes,
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
            },
        };

        let parser = left_left_parser::Parser::new(grammar, xml_out);

        let mut function_collector = FunctionCollector::new();
        let type_collector = TypeCollector::new();

        let built_in_functions: InternalFunctions =
            functions::build_functions(&mut function_collector);
        let built_in_types: InternalTypes = primitives::build_primitives();

        Some(FauxCompiler {
            parser: Rc::from(parser),
            file_cache: HashMap::new(),
            parse_stack: Vec::new(),
            type_collector,
            function_collector,
            lexer: Lexer::new_faux_lexer(),
            built_in_functions,
            built_in_types,
        })
    }

    // the compiler builds the built-in definitions, but the interpreter also
    // needs to know the function definitions
    pub fn get_built_in_functions(self) -> InternalFunctions {
        self.built_in_functions
    }

    pub fn compile(&mut self, source_file: &Path) -> SimpleResult<ast::FileAst> {
        if self
            .parse_stack
            .iter()
            .any(|file| file.as_ref() == source_file)
        {
            return Err(SimpleError::new(format!(
                "Recursive include : {:?}",
                self.parse_stack
            )));
        }
        self.parse_stack.push(Box::from(source_file));

        let file_contents = std::fs::read_to_string(source_file).map_err(SimpleError::from)?;
        let program_string = file_contents.replace("\r\n", "\n");

        let tokens = self.lexer.read(&program_string).map_err(|char_idx| {
            SimpleError::new(parser::Failure::LexerError { char_idx }.error_string(&program_string))
        })?;

        // we clone the parser rc, because parse_program returns a result that borrows from this parser.
        // if we don't clone, the borrow prevents us from recursively parse includes before using the parse_result
        let parser = &self.parser.clone();
        let parse_result = parser.parse_program(&tokens);

        let syntax_tree = match parse_result {
            Ok(node) => node,
            Err(err) => {
                return Err(SimpleError::new(format!(
                    "Error parsing file {}: \n{}",
                    source_file.to_string_lossy(),
                    err.into_iter()
                        .map(|err| err.error_string(&program_string) + "\n---\n\n")
                        .collect::<String>()
                )));
            },
        };

        let this_path =
            std::env::current_dir().expect("Could not get this program's current directory");
        let files_to_compile = meta_program::extract_includes(&syntax_tree, &this_path);

        let mut namespace = Namespace::new_root();

        if files_to_compile.is_empty() {
            // add all built-in definitions if this has no includes.
            // if this has includes, then the include will provide the built-in definitions
            namespace.extend(functions::get_functions(&self.built_in_functions));
            namespace.extend(types::get_types(&self.built_in_types));
        } else {
            for include_file in files_to_compile {
                if !self.file_cache.contains_key(include_file.as_path()) {
                    let this_program = self.compile(&include_file)?;
                    self.file_cache
                        .insert(Box::from(include_file.clone()), Rc::from(this_program));
                };

                let include_program = self
                    .file_cache
                    .get(include_file.as_path())
                    .expect("if-statement above should compile on-demand");
                namespace.extend(include_program.namespaces.clone());
            }
        }

        // we have resolved and compiled all includes.
        // now we can start parsing this file
        symbolizer::parse_faux_program(
            syntax_tree,
            &namespace,
            &mut self.type_collector,
            &mut self.function_collector,
        )
        .map_err(SimpleError::from)
    }

    pub fn compile_script(definition_text: &str, program_text: &str) -> SimpleResult<ast::FileAst> {
        let grammar = ebnf_parser::parse_ebnf(definition_text)
            .map(grammatificator::convert_to_grammar)
            .map_err(|err| SimpleError::new(ebnf_parser::error_string(&err, definition_text)))?;

        let tokens = Lexer::read_faux(&program_text)
            .map_err(|char_idx| simple_error!("Unrecognized token at char index {}", char_idx))?;
        let parser = left_left_parser::Parser::new(grammar, None);
        let parse_result = parser.parse_program(&tokens);

        let syntax_tree = match parse_result {
            Ok(node) => node,
            Err(err) => {
                return Err(SimpleError::new(format!(
                    "Error parsing script: \n{}",
                    err.into_iter()
                        .map(|err| err.error_string(&program_text) + "\n---\n\n")
                        .collect::<String>()
                )));
            },
        };

        let mut function_collector = FunctionCollector::new();
        let mut namespace = ast::Namespace::new_root();

        let built_in_functions = functions::build_functions(&mut function_collector);
        let built_in_types = primitives::build_primitives();

        namespace.extend(functions::get_functions(&built_in_functions));
        namespace.extend(types::get_types(&built_in_types));

        symbolizer::parse_faux_script(syntax_tree, &namespace, &mut function_collector)
            .map_err(SimpleError::from)
    }
}
