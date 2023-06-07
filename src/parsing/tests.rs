use crate::parsing::{ebnf_ast::*, parser::RuleNode};

use super::{ebnf_parser, parser::parse_program_with_grammar};

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
        Term::Concatenation(vec![
            Term::Identifier(String::from("number")),
            Term::Terminal(String::from("+")),
            Term::Identifier(String::from("number")),
        ])
    );

    assert_eq!(
        ast.rules[1].pattern,
        Term::Alternation(vec![
            Term::Terminal(String::from("0")),
            Term::Terminal(String::from("1")),
            Term::Terminal(String::from("2")),
        ])
    );
}

#[test]
fn complex_ebnf() {
    let definition = r#"
        a = b | c, { d, a };
        b = "?" | b, ( d | a );
        c = "!", [ b, b ], { d };
        d = '"';
    "#;

    let ast = ebnf_parser::parse_ebnf(definition).unwrap();

    assert_eq!(ast.rules.len(), 4, "{:?}", ast.rules);

    assert_eq!(
        // rule a
        ast.rules[0].pattern,
        Term::Alternation(vec![
            Term::Identifier(String::from("b")),
            Term::Concatenation(vec![
                Term::Identifier(String::from("c")),
                Term::Repetition(Box::new(Term::Concatenation(vec![
                    Term::Identifier(String::from("d")),
                    Term::Identifier(String::from("a")),
                ])))
            ])
        ])
    );

    assert_eq!(
        // rule c
        ast.rules[2].pattern,
        Term::Concatenation(vec![
            Term::Terminal(String::from("!")),
            Term::Optional(Box::new(Term::Concatenation(vec![
                Term::Identifier(String::from("b")),
                Term::Identifier(String::from("b")),
            ]))),
            Term::Repetition(Box::new(Term::Identifier(String::from("d")),))
        ])
    );
}

#[test]
fn simple_parser() {
    let definition = r#"
        addition = number, "+", number;
        number = "0" | "1" | "2";
    "#;
    let formula = r#"1+2"#;

    let grammar = ebnf_parser::parse_ebnf(definition).unwrap();
    let program_ast = parse_program_with_grammar(formula, &grammar).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule: "addition",
            tokens: formula,
            subrules: vec![
                RuleNode {
                    rule: "number",
                    tokens: "1",
                    subrules: Vec::new()
                },
                RuleNode {
                    rule: "number",
                    tokens: "2",
                    subrules: Vec::new()
                },
            ]
        }
    )
}

#[test]
fn simple_parser_with_simple_ignore() {
    let definition = r#"
        addition = number, "+", number;
        number = "0" | "1" | "2";
        ignored = " ";
    "#;
    let formula = r#" 1 + 2 "#;

    let grammar = ebnf_parser::parse_ebnf(definition).unwrap();
    let program_ast = parse_program_with_grammar(formula, &grammar).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule: "addition",
            tokens: formula,
            subrules: vec![
                RuleNode {
                    rule: "number",
                    tokens: "1",
                    subrules: Vec::new()
                },
                RuleNode {
                    rule: "number",
                    tokens: "2",
                    subrules: Vec::new()
                },
            ]
        }
    )
}
