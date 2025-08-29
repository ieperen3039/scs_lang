use super::{
    ast::{self, FunctionType},
    function_collector::FunctionCollector,
    symbolizer,
};
use crate::parsing::parser::Failure;
use crate::symbolization::ast::{Identifier, Literal, Namespace, Statement, TypeRef};
use crate::{
    built_in::{self, functions::NativeFunctionBuilder, primitives::build_primitives},
    parsing::{ebnf_parser, left_left_parser, lexer::Lexer},
    transformation::grammatificator,
};
use crate::symbolization::ast::Literal::Boolean;
use crate::symbolization::semantic_result::SemanticResult;

#[test]
fn hello_world() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = "\"Hello world!\"";

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);
    let namespace = Namespace::new_root();
    let mut function_collector = FunctionCollector::new();
    let program_result =
        symbolizer::parse_faux_script(syntax_tree.unwrap(), &namespace, &mut function_collector);

    match program_result {
        Err(err) => {
            panic!("{err}");
        }
        Ok(ast) => {
            let entry_point = ast.function_definitions.get(&ast.entry_function);
            let statements = &entry_point.unwrap().statements;
            assert_eq!(statements.len(), 1);
            assert!(matches!(statements[0].base_element.inner, ast::ValueExpressionInner::Literal(Literal::String(_))));
        }
    }
}

/// fn cat(file: string) -> string[]
/// fn transform_each(in: string[], fn: fn<(string)string>)
/// operator '>' fn_for_each
/// fn convert_case(input: string, to_upper: flag, to_lower: flag, toggle: flag) -> string
/// fn select(input: string[], n: int) -> string
/// fn zip<T>(left: T[], right: T[]) -> (T, T)[]
/// fn sys.log(text: string, err: flag) -> string
fn create_test_namespace() -> Namespace {
    let mut id_gen = NativeFunctionBuilder::new();
    let type_string_stream = TypeRef::Stream(Box::new(TypeRef::STRING.clone()));
    let mut namespace = Namespace::new_root();

    namespace.add_constant_literal(ast::Identifier::from("true"), Boolean(true));
    namespace.add_constant_literal(ast::Identifier::from("false"), Boolean(false));

    namespace.add_function({
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("cat"),
            parameters: vec![
                builder.req_par("file", &TypeRef::STRING)
            ],
            return_type: type_string_stream.clone(),
            start_char: 0,
            generic_parameters: Vec::new(),
        }
    });

    {
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        let fn_for_each = ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("transform_each"),
            parameters: vec![
                builder.req_par("in", &type_string_stream),
                builder.req_par(
                    "fn",
                    &TypeRef::Function(FunctionType {
                        parameters: vec![TypeRef::STRING.clone()],
                        return_type: Box::new(TypeRef::STRING),
                    }),
                ),
            ],
            return_type: type_string_stream.clone(),
            start_char: 0,
            generic_parameters: Vec::new(),
        };
        namespace.add_operator(ast::Identifier::from(">"), &fn_for_each);
        namespace.add_function(fn_for_each);
    }
    namespace.add_function({
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("convert_case"),
            parameters: vec![
                builder.req_par("input", &TypeRef::STRING),
                builder.flag("to_upper"),
                builder.flag("to_lower"),
                builder.flag("toggle"),
            ],
            return_type: TypeRef::STRING.clone(),
            start_char: 0,
            generic_parameters: Vec::new(),
        }
    });
    namespace.add_function({
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("select"),
            parameters: vec![
                builder.req_par("input", &type_string_stream),
                builder.req_par("n", &TypeRef::INT),
            ],
            return_type: TypeRef::STRING.clone(),
            start_char: 0,
            generic_parameters: Vec::new(),
        }
    });
    namespace.add_function({
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        let generic_name = Identifier::from("T");

        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("zip"),
            parameters: vec![
                builder.req_par("left", &TypeRef::Stream(Box::new(TypeRef::GenericName(generic_name.clone())))),
                builder.req_par("right", &TypeRef::Stream(Box::new(TypeRef::GenericName(generic_name.clone())))),
            ],
            return_type: TypeRef::Stream(Box::new(TypeRef::UnnamedTuple(vec![
                TypeRef::GenericName(generic_name.clone()),
                TypeRef::GenericName(generic_name.clone()),
            ]))),
            start_char: 0,
            generic_parameters: vec![generic_name],
        }
    });

    {
        let mut git_ns = Namespace::new("sys", &namespace);
        git_ns.add_function({
            let mut builder = built_in::function_builder::FunctionBuilder::new();
            ast::FunctionDeclaration {
                id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
                name: ast::Identifier::from("log"),
                parameters: vec![
                    builder.req_par("text", &TypeRef::STRING),
                    builder.flag("err"),
                ],
                return_type: TypeRef::STRING.clone(),
                start_char: 0,
                generic_parameters: Vec::new(),
            }
        });
        namespace.add_sub_scope(git_ns);
    }
    namespace
}

fn parse(definition: &str, program: &str, namespace: Namespace) -> SemanticResult<ast::FileAst> {
    let mut function_collector = FunctionCollector::new();
    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);

    if let Err(error) = syntax_tree {
        report_parse_error(program, error);
    }

    let program_result =
        symbolizer::parse_faux_script(syntax_tree.unwrap(), &namespace, &mut function_collector);
    program_result
}

#[test]
fn parse_simple_statement() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            select(5)
            convert_case(to_upper=true)
            sys.log()
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_partial_fn_as_parameter() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            transform_each(convert_case(to_upper=true))
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_explicit_lambda_as_parameter() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            transform_each((s) {
                s
                    convert_case(to_upper=true)
                    sys.log()
            })
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_implicit_lambda_as_parameter() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            transform_each({
                convert_case(to_upper=true)
                sys.log()
            })
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_fn_literal_as_parameter() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            transform_each(sys.log)
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_operator_usage() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            > convert_case(to_upper=true)
            > convert_case(to_upper=true)
            > sys.log()
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_operator_with_explicit_lambda() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            > (str) {
                str
                    convert_case(to_upper=true)
                    sys.log()
            }
            > (str) {
                str
                    convert_case(to_upper=true)
                    sys.log()
            }
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_operator_with_implicit_lambda() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            > {
                convert_case(to_upper=true)
                sys.log()
            }
            > {
                convert_case(to_upper=true)
                sys.log()
            }
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}

#[test]
fn parse_convoluted_statements() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        cat("data.csv")
            = data; // string[]

        data
            head(from_begin=2)
            select(n=3)
            > sys.log()
            = extra_columns; // string[]

        data
            zip(extra_columns)
    "#;

    let program_result = parse(definition, program, create_test_namespace());

    if let Err(error) = program_result {
        panic!("Error parsing program: \n{}", error.error_string(program));
    }
}


fn report_parse_error(program: &str, error: Vec<Failure>) -> ! {
    let error_string = error
        .iter()
        .map(|e| e.error_string(program))
        .fold(String::new(), |a, s| a + "\n" + &s);
    panic!("{}", error_string);
}

#[test]
fn parse_function_definition() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    // it should be noted that floating point arithmatic is (probably) not going to be supported
    let program = r#"
        invsqrt(10)
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
    let mut id_gen = NativeFunctionBuilder::new();

    let fn_sqrt = {
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("sqrt"),
            parameters: vec![builder.req_par("n", &TypeRef::INT)],
            return_type: TypeRef::INT.clone(),
            start_char: 0,
            generic_parameters: Vec::new(),
        }
    };
    let fn_div = {
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("div"),
            parameters: vec![
                builder.req_par("a", &TypeRef::INT),
                builder.req_par("b", &TypeRef::INT),
            ],
            return_type: TypeRef::INT.clone(),
            start_char: 0,
            generic_parameters: Vec::new(),
        }
    };
    let fn_lt = {
        let mut builder = built_in::function_builder::FunctionBuilder::new();
        ast::FunctionDeclaration {
            id: ast::GlobalFunctionTarget::Native(id_gen.new_id()),
            name: ast::Identifier::from("less_than"),
            parameters: vec![
                builder.req_par("a", &TypeRef::INT),
                builder.req_par("b", &TypeRef::INT),
            ],
            generic_parameters: Vec::new(),
            return_type: TypeRef::boolean(),
            start_char: 0,
        }
    };

    let functions = vec![fn_sqrt, fn_div, fn_lt];

    let mut namespace = built_in::types::get_types(&build_primitives());
    for fn_def in &functions {
        namespace.add_function(fn_def.clone());
    }

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();
    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens);

    if let Err(error) = syntax_tree {
        report_parse_error(program, error);
    }

    let parse_result =
        symbolizer::parse_faux_script(syntax_tree.unwrap(), &namespace, &mut function_collector);

    match parse_result {
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

            assert_eq!(entry_fn.return_type, TypeRef::boolean());
        },
    }
}
