use crate::parsing::ebnf_ast;

use super::ebnf_parser;

#[test]
fn simple_ebnf() {
    let definition = r#"
        addition = number, "+", number;
        number = "0" | "1" | "2";
    "#;

    let ast = ebnf_parser::parse_ebnf(definition).unwrap();

    assert_eq!(ast.rules.len(), 2, "{:?}", ast.rules);

    assert_eq!(
        ast.rules[0].pattern,
        ebnf_ast::Term::Concatenation(vec![
            ebnf_ast::Term::Identifier(String::from("number")),
            ebnf_ast::Term::Terminal(String::from("+")),
            ebnf_ast::Term::Identifier(String::from("number")),
        ])
    );

    assert_eq!(
        ast.rules[1].pattern,
        ebnf_ast::Term::Alternation(vec![
            ebnf_ast::Term::Terminal(String::from("0")),
            ebnf_ast::Term::Terminal(String::from("1")),
            ebnf_ast::Term::Terminal(String::from("2")),
        ])
    );
}
