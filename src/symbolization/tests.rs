use crate::parsing::{rule_nodes::RuleNode, ebnf_parser, parser};


#[test]
fn collect_types_1() {
    // based on the observation that an implicit operator in the example could not be parsed
    let definition = include_str!("../../doc/definition.ebnf");
    let program = r#"
        type FilePath : String
    "#;

    let grammar = ebnf_parser::parse_ebnf(definition).unwrap();
    let parser = parser::Parser::new(grammar, None).unwrap();
    let program_ast = parser.parse_program(&program);

    let collector = TypeCollector::new();

}