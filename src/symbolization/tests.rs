use crate::{
    built_in,
    parsing::{ebnf_parser, left_left_parser, lexer::Lexer},
    transformation::grammatificator,
};

use super::{ast, function_collector::FunctionCollector, symbolizer};

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
    let program = r#"
        cat("data.csv")
            = data; // string[]

        data
            tail(from_begin=2)
            > select(separator=";", n=3)
            > git.log(pattern="%s;%h")
            = extra_columns; // string[]

        data
            zip(extra_columns)
            write("data.csv);
    "#;

    let type_string_stream = ast::TypeRef::Stream(Box::new(ast::TypeRef::STRING.clone()));

    let mut function_collector = FunctionCollector::new();

    let fn_cat = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("cat"),
            parameters: vec![builder.req_par("file", None, &ast::TypeRef::STRING)],
            return_type: type_string_stream.clone(),
            is_external: true,
        }
    };
    let fn_tail = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("tail"),
            parameters: vec![builder.req_par("input", None, &type_string_stream)],
            return_type: type_string_stream.clone(),
            is_external: true,
        }
    };
    let fn_select = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("select"),
            parameters: vec![
                builder.req_par("input", None, &ast::TypeRef::STRING),
                builder.req_par("separator", None, &ast::TypeRef::STRING),
                builder.req_par("n", None, &ast::TypeRef::INT),
            ],
            return_type: ast::TypeRef::STRING.clone(),
            is_external: true,
        }
    };
    let fn_zip = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("zip"),
            parameters: vec![
                builder.req_par("a", None, &type_string_stream.clone()),
                builder.req_par("b", None, &type_string_stream.clone()),
            ],
            return_type: ast::TypeRef::UnamedTuple(vec![
                type_string_stream.clone(),
                type_string_stream.clone(),
            ]),
            is_external: true,
        }
    };

    let fn_git_log = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("log"),
            parameters: vec![builder.req_par("pattern", None, &ast::TypeRef::STRING)],
            return_type: ast::TypeRef::STRING.clone(),
            is_external: true,
        }
    };

    let functions = vec![fn_cat, fn_tail, fn_select, fn_zip];

    let mut namespace = ast::Namespace::new("", None);
    for fn_def in &functions {
        namespace.add_function(fn_def.clone());
    }

    {
        let mut git_ns = ast::Namespace::new("git", Some(&namespace));
        git_ns.add_function(fn_git_log);

        namespace.add_sub_scope(git_ns);
    }

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);

    if let Err(error) = syntax_tree {
        panic!(
            "{}",
            error
                .iter()
                .map(|e| e.error_string(program))
                .fold(String::new(), |a, s| a + "\n" + &s)
        );
    }

    let program_result = symbolizer::parse_faux_script(
        syntax_tree.unwrap(),
        &ast::Namespace::new("", None),
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

    let fn_sqrt = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("sqrt"),
            parameters: vec![builder.req_par("n", None, &ast::TypeRef::INT)],
            return_type: ast::TypeRef::INT.clone(),
            is_external: true,
        }
    };
    let fn_div = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("div"),
            parameters: vec![
                builder.req_par("a", None, &ast::TypeRef::INT),
                builder.req_par("b", None, &ast::TypeRef::INT),
            ],
            return_type: ast::TypeRef::INT.clone(),
            is_external: true,
        }
    };
    let fn_lt = {
        let mut builder = built_in::functions::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: function_collector.new_id(),
            name: ast::Identifier::from("less_than"),
            parameters: vec![
                builder.req_par("a", None, &ast::TypeRef::INT),
                builder.req_par("b", None, &ast::TypeRef::INT),
            ],
            return_type: ast::TypeRef::BOOLEAN.clone(),
            is_external: true,
        }
    };

    let functions = vec![fn_sqrt, fn_div, fn_lt];

    let mut namespace = ast::Namespace::new("", None);
    for fn_def in &functions {
        namespace.add_function(fn_def.clone());
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
        panic!(
            "{}",
            error
                .iter()
                .map(|e| e.error_string(program))
                .fold(String::new(), |a, s| a + "\n" + &s)
        );
    }

    let program_result = symbolizer::parse_faux_script(
        syntax_tree.unwrap(),
        &namespace,
        &mut function_collector,
    );

    match program_result {
        Err(error) => {
            panic!("Error parsing program: \n{error}");
        },
        Ok(program) => {
            // both the script function and the defined function should be here
            assert_eq!(program.function_definitions.len(), 2);
            let entry_fn = program.function_definitions.get(&program.entry_function);
            let Some(entry_fn) = entry_fn else {
                panic!("program.entry_function not found")
            };

            assert_eq!(entry_fn.return_var.var_type, ast::TypeRef::BOOLEAN);
        },
    }
}
