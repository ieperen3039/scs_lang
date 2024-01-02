use crate::{
    parsing::{chomsky_parser, ebnf_parser, lexer::Lexer},
    symbolization::{
        ast::{DefinedRef, Identifier, Scope, TypeClass, TypeRef},
        built_in_types::TYPE_ID_STRING,
        symbolizer,
        type_collector::TypeCollector,
    },
    transforming::grammatificator,
};

#[test]
fn parse_simple_derived_type() {
    let definition = include_str!("../../doc/definition.ebnf");
    let grammar = grammatificator::convert_to_grammar(ebnf_parser::parse_ebnf(definition).unwrap());
    let xml_out = Some(std::fs::File::create("test_parse_simple_derived_type.xml").unwrap());
    let parser = chomsky_parser::Parser::new(grammar, xml_out);
    
    let program = r#"
        type FilePath : String
    "#;
    let tokens = Lexer::read(program).unwrap();
    let ast = parser.parse_program(&tokens).unwrap();

    println!("{:?}", ast);

    let external_scope = Scope::new("", None);
    let mut type_collector = TypeCollector::new();
    let parsed_program =
        symbolizer::parse_symbols(ast, &external_scope, &mut type_collector).unwrap();

    assert_eq!(parsed_program.type_definitions.len(), 2, "{:?}", parsed_program.type_definitions.values());

    let found_type = parsed_program
        .type_definitions
        .values()
        .find(|t| t.name.as_ref() == "FilePath");
    assert!(found_type.is_some());

    let found_type = found_type.unwrap();
    assert_eq!(found_type.name, Identifier::from("FilePath"));
    assert_eq!(found_type.full_scope, vec![]);
    assert_eq!(found_type.generic_parameters, vec![]);
    assert_eq!(
        found_type.type_class,
        TypeClass::Base {
            derived: Some(Box::from(TypeRef::Defined(DefinedRef {
                id: TYPE_ID_STRING,
                generic_parameters: vec![]
            })))
        }
    );
    assert!(found_type.member_functions.is_empty());
}

fn parse_implicit() {
    let definition = include_str!("../../doc/definition.ebnf");
    let program = include_str!("../../doc/implicit_impl.faux");
}
