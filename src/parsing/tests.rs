use std::borrow::Borrow;

use crate::parsing::{
    ebnf_ast::*,
    ebnf_parser,
    parser::{self, RuleNode},
};

#[test]
fn simple_ebnf() {
    let definition = r#"
        addition = number, "+", number;
        number = "0" | "1" | "2";
    "#;

    let ast = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    assert_eq!(ast.rules.len(), 2, "{:?}", ast.rules);

    assert_eq!(
        ast.rules[0].pattern,
        Term::Concatenation(vec![
            Term::Identifier(String::from("number")),
            Term::Literal(String::from("+")),
            Term::Identifier(String::from("number")),
        ])
    );

    assert_eq!(
        ast.rules[1].pattern,
        Term::Alternation(vec![
            Term::Literal(String::from("0")),
            Term::Literal(String::from("1")),
            Term::Literal(String::from("2")),
        ])
    );
}

#[test]
fn simple_ebnf_with_comments() {
    let definition = r#"
        (* this is an ebnf *)
        addition = number, (*comment here*) "+", (**) number;
        number = "0" | (** trying some annoying stuff ***) ( "1" | "2" );
        (* asdf *)
    "#;

    let ast = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    assert_eq!(ast.rules.len(), 2, "{:?}", ast.rules);

    assert_eq!(
        ast.rules[0].pattern,
        Term::Concatenation(vec![
            Term::Identifier(String::from("number")),
            Term::Literal(String::from("+")),
            Term::Identifier(String::from("number")),
        ])
    );

    assert_eq!(
        ast.rules[1].pattern,
        Term::Alternation(vec![
            Term::Literal(String::from("0")),
            Term::Alternation(vec![
                // nested alternation caused by the grouping
                Term::Literal(String::from("1")),
                Term::Literal(String::from("2")),
            ])
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

    let ast = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

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
            Term::Literal(String::from("!")),
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
    let formula = "1+2";

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();
    let program_ast = parser::parse_program_with_grammar(formula, &grammar).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule: "addition",
            tokens: "1+2",
            sub_rules: vec![
                RuleNode {
                    rule: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule: "number",
                    tokens: "2",
                    sub_rules: Vec::new()
                },
            ]
        }
    )
}

#[test]
fn simple_parser_with_simple_ignore() {
    let definition = r#"
        addition = number, _, "+", _, number;
        number = "0" | "1" | "2";
        _ = { " " };
    "#;
    let formula = r#"1 + 2"#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();
    let program_ast = parser::parse_program_with_grammar(formula, &grammar).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule: "addition",
            tokens: "1 + 2",
            sub_rules: vec![
                RuleNode {
                    rule: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule: "number",
                    tokens: "2",
                    sub_rules: Vec::new()
                },
            ]
        }
    )
}

#[test]
fn complex_parser_with_complex_ignore() {
    let definition = r#"
        sentence     = _, demand, _, ["and", _, demand], _;
        demand       = person, _, ["must", _], action, _, how;
        person       = "you" | "I" | "he";
        action       = _meet_action | _walk_action;
        _walk_action = "stay" | "come" | "go";
        _meet_action = "meet", _, person;
        how          = "here" | "there" | "with", _, person;
        _            = { " " | "," | "!" | "?" };
    "#;

    let formula = r#" you must go there, and I come with you! "#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();
    let program_ast = parser::parse_program_with_grammar(formula, &grammar).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule: "sentence",
            tokens: " you must go there, and I come with you! ",
            sub_rules: vec![
                RuleNode {
                    rule: "demand",
                    tokens: "you must go there",
                    sub_rules: vec![
                        RuleNode {
                            rule: "person",
                            tokens: "you",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule: "action",
                            tokens: "go",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule: "how",
                            tokens: "there",
                            sub_rules: Vec::new()
                        }
                    ]
                },
                RuleNode {
                    rule: "demand",
                    tokens: "I come with you",
                    sub_rules: vec![
                        RuleNode {
                            rule: "person",
                            tokens: "I",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule: "action",
                            tokens: "come",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule: "how",
                            tokens: "with you",
                            sub_rules: vec![RuleNode {
                                rule: "person",
                                tokens: "you",
                                sub_rules: Vec::new()
                            },]
                        }
                    ]
                },
            ]
        }
    )
}

#[test]
fn simple_parser_with_regex() {
    let definition = r#"
        addition = number, "+", number;
        number = "/\d+/";
    "#;
    let formula = "1+2";

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();
    let program_ast = parser::parse_program_with_grammar(formula, &grammar).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule: "addition",
            tokens: "1+2",
            sub_rules: vec![
                RuleNode {
                    rule: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule: "number",
                    tokens: "2",
                    sub_rules: Vec::new()
                },
            ]
        }
    )
}

#[test]
fn scs() {
    let definition = include_str!("../../doc/definition.ebnf");
    let program = include_str!("../../doc/example.scs");

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            err
        })
        .unwrap();

    let parse_result = parser::parse_program_with_grammar(program, &grammar);

    if parse_result.is_err() {
        print!(
            "Error parsing program: \n{}",
            parse_result
                .as_ref()
                .unwrap_err()
                .into_iter()
                .map(|err| parser::error_string(&err, program) + "\n---\n\n")
                .collect::<String>()
        );
        assert!(parse_result.is_ok());

    }
    
    let program_ast = parse_result.unwrap();

    assert!(program_ast.rule == "scs_program")
}
