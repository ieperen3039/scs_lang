use crate::parsing::lexer::Lexer;
use crate::parsing::{ebnf_parser, left_left_parser};
use crate::symbolization::ast::Namespace;
use crate::symbolization::function_collector::FunctionCollector;
use crate::symbolization::semantic_result::{SemanticError, SemanticResult};
use crate::symbolization::symbolizer;
use crate::transformation::grammatificator;
use crate::{compiler::FauxCompiler, symbolization::ast};


pub fn parse_with_custom_namespace(
    definition: &str,
    program: &str,
    namespace: Namespace,
) -> SemanticResult<ast::FileAst> {
    let mut function_collector = FunctionCollector::new();
    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);

    if let Err(error) = syntax_tree {
        return Err(SemanticError::SyntaxError {
            error_lines: error.iter().map(|e| e.error_string(program)).collect(),
        });
    }

    symbolizer::parse_faux_script(syntax_tree.unwrap(), &namespace, &mut function_collector)
}

// #[test]
#[allow(dead_code)]
fn transitive_include() {
    let definition = include_str!("../doc/definition.ebnf");

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let result = compiler
        .compile(&std::path::PathBuf::from(
            "../test_data/include_test/include_test_4.faux",
        ))
        .unwrap();

    assert!(result.namespaces.functions.contains_key("include_test_5"));
}

#[test]
fn faux_script_compile() {
    let definition = include_str!("../doc/faux_script.ebnf");
    let program = r#"
        add(1, 3)
            mul(4)
            = alpha;

        add(5, 6)
            mul(r=alpha)
            div(l=1) // calculates 1/x
    "#;

    let parse_result = FauxCompiler::build(&definition, None)
        .unwrap()
        .compile_script(&program);

    match parse_result {
        Err(error) => {
            panic!("{error}");
        },
        Ok(program) => {
            // only the script function should be here
            assert_eq!(program.function_definitions.len(), 1);
            let entry_fn = program.function_definitions.get(&program.entry_function);
            let Some(entry_fn) = entry_fn else {
                panic!("program.entry_function not found")
            };

            assert_eq!(entry_fn.return_type, ast::TypeRef::INT);
        },
    }
}
