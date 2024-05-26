use simple_error::SimpleError;

use crate::{
    parsing::{ebnf_parser, left_left_parser, lexer::Lexer, parser}, transformation::{grammar::Grammar, grammatificator}
};

use super::{ast::Namespace, symbolizer, type_collector::TypeCollector};

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
fn parse_function_definition() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    // it should be noted that floating point arithmatic is (probably) not supported
    let program = r#"
        fn invsq(int number) : int {
            number
                mul(number)
                (sq) { 
                    1 div(sq)
                }
        }

        5 invsq();
    "#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .unwrap();

    let tokens = Lexer::read_faux(&program).unwrap();
    let parser = left_left_parser::Parser::new(grammar, None);
    let syntax_tree = parser.parse_program(&tokens).unwrap();

    let root_namespace = Namespace::new("root", None);
    let mut type_collector = TypeCollector::new();

    let program_result = symbolizer::parse_faux_script(syntax_tree, &root_namespace, &mut type_collector);

    if let Err(error) = program_result {
        panic!(
            "Error parsing program: \n{error}"
        );
    }
}