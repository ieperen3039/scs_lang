use simple_error::SimpleError;

use crate::{
    built_in,
    parsing::{ebnf_parser, left_left_parser, lexer::Lexer},
    symbolization::semantic_result::SemanticError,
    transformation::grammatificator,
};

use super::{
    ast::{self, TypeRef},
    function_collector::FunctionCollector,
    semantic_result::SemanticResult,
    symbolizer,
    type_collector::TypeCollector,
};

// #[test]
// fn parse_simple_derived_type() {
//     let definition = include_str!("../../doc/definition.ebnf");
//     let program = r#"
//         type FilePath : String
//     "#;
//     let tokens = Lexer::read_faux(program).unwrap();
//     let ast = parser.parse_program(&tokens).unwrap();

//     println!("{:?}", ast);

//     let external_scope = Namespace::new("", None);
//     let mut type_collector = TypeCollector::new();
//     let parsed_program =
//         symbolizer::parse_symbols(ast, &external_scope, &mut type_collector).unwrap();

//     assert_eq!(parsed_program.type_definitions.len(), 2, "{:?}", parsed_program.type_definitions.values());

//     let found_type = parsed_program
//         .type_definitions
//         .values()
//         .find(|t| t.name.as_ref() == "FilePath");
//     assert!(found_type.is_some());

//     let found_type = found_type.unwrap();
//     assert_eq!(found_type.name, Identifier::from("FilePath"));
//     assert_eq!(found_type.full_scope, vec![]);
//     assert_eq!(
//         found_type.type_class,
//         TypeClass::Base {
//             derived: Some(Box::from(TypeRef::Defined(DefinedRef {
//                 id: TYPE_ID_STRING
//             })))
//         }
//     );
// }

#[test]
fn parse_convoluted_statements() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    // it should be noted that floating point arithmatic is (probably) not supported
    let program = r#"
        cat("data.csv")
            = data; // string[]

        data
            tail(from_begin=2)
            split(separator=";", n=3)
            > git.log(pattern="%s;%h")
            = extra_columns; // string[]

        data
            zip(extra_columns)
            write("data.csv);
    "#;

    let mut function_collector = FunctionCollector::new();
    let functions = vec![
        built_in::functions::FunctionProto::new("cat", &mut function_collector)
            .req_par("file", None, &ast::TypeRef::STRING)
            .returns(&ast::TypeRef::Stream(Box::from(TypeRef::STRING.clone()))),
    ];

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);

    if let Err(error) = syntax_tree {
        println!(
            "{}",
            error
                .iter()
                .map(|e| e.error_string(program))
                .fold(String::new(), |a, s| a + "\n" + &s)
        );
        return;
    }

    let program_result = symbolizer::parse_faux_script(
        syntax_tree.unwrap(),
        &ast::Namespace::new("", None),
        &functions,
        &mut function_collector,
    );

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{error}");
    }
}

#[test]
fn parse_function_definition() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    // it should be noted that floating point arithmatic is (probably) not going to be supported
    let program = r#"
        10
            invsqrt()
            less_than(1)
            = return;

        fn invsqrt(int n) : int {
            n
                sqrt() 
                (n_sq) {
                    1 div(n_sq)
                }
        }
    "#;

    let mut function_collector = FunctionCollector::new();

    let functions = vec![
        built_in::functions::FunctionProto::new("sqrt", &mut function_collector)
            .req_par("n", None, &ast::TypeRef::INT)
            .returns(&ast::TypeRef::INT),
        built_in::functions::FunctionProto::new("div", &mut function_collector)
            .req_par("a", None, &ast::TypeRef::INT)
            .req_par("b", None, &ast::TypeRef::INT)
            .returns(&ast::TypeRef::INT),
        built_in::functions::FunctionProto::new("less_than", &mut function_collector)
            .req_par("a", None, &ast::TypeRef::INT)
            .req_par("b", None, &ast::TypeRef::INT)
            .returns(&ast::TypeRef::BOOLEAN),
    ];

    let mut namespace = ast::Namespace::new("", None);
    for fn_def in &functions {
        namespace.add_function(fn_def);
    }
    for type_def in &built_in::primitives::get_primitives() {
        namespace.add_type(type_def);
    }

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);

    if let Err(error) = syntax_tree {
        println!(
            "{}",
            error
                .iter()
                .map(|e| e.error_string(program))
                .fold(String::new(), |a, s| a + "\n" + &s)
        );
        return;
    }

    let program_result = symbolizer::parse_faux_script(
        syntax_tree.unwrap(),
        &namespace,
        &functions,
        &mut function_collector,
    );

    match program_result {
        Err(error) => {
            panic!("Error parsing program: \n{error}");
        }
        Ok(program) => {
            // both the script function and the defined function should be here
            assert_eq!(program.function_definitions.len(), 2);
            let entry_fn = program.function_definitions.get(&program.entry_function);
            let Some(entry_fn) = entry_fn else {
                panic!("program.entry_function not found")
            };

            assert_eq!(entry_fn.return_var.var_type, ast::TypeRef::BOOLEAN);
        }
    }
}
