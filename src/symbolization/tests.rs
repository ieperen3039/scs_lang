use crate::parsing::{rule_nodes::RuleNode, ebnf_parser, parser};

use super::symbolizer;


#[test]
fn collect_types_1() {
    // based on the observation that an implicit operator in the example could not be parsed
    let definition = include_str!("../../doc/definition.ebnf");
    let program = r#"
        type FilePath : String
    "#;

    let grammar = ebnf_parser::parse_ebnf(definition).unwrap();
    let parser = parser::Parser::new(grammar, None).unwrap();
    let ast = parser.parse_program(&program).unwrap();
    let program = symbolizer::convert_to_program("collect_types_1", ast).unwrap();
}