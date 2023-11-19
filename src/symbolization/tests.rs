use std::rc::Rc;

use crate::parsing::{ebnf_parser, parser};

use super::{symbolizer, ast::*, built_in_types::FIRST_CUSTOM_TYPE_ID};


#[test]
fn collect_types_1() {
    // based on the observation that an implicit operator in the example could not be parsed
    let definition = include_str!("../../doc/definition.ebnf");
    let program = r#"
        type FilePath : String
    "#;

    let grammar = ebnf_parser::parse_ebnf(definition).unwrap();
    let parser = parser::Parser::new(grammar, None).unwrap();
    // let ast = parser.parse_program(&program).unwrap();
}

fn parse_implicit() {
    let definition = include_str!("../../doc/definition.ebnf");
    let program = include_str!("../../doc/implicit_impl.faux");
}