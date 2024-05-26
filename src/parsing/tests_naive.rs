use simple_error::SimpleError;

use crate::parsing::{
    ebnf_parser,
    lexer::Lexer,
    parser,
    rule_nodes::RuleNode,
    naive_recursive_descent_parser
};

#[test]
fn simple_lexer_and_parser() {
    let definition = r#"
        addition = number, "+", number;
        number = "0" | "1" | "2";
    "#;
    let formula = "1+2";

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            SimpleError::new(ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = naive_recursive_descent_parser::Parser::new(grammar, None);
    let program_ast = parser.parse_program(&tokens).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule_name: "addition",
            tokens: &tokens[..],
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: &tokens[0..=0],
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "number",
                    tokens: &tokens[2..=2],
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

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = naive_recursive_descent_parser::Parser::new(grammar, None);
    let program_ast = parser.parse_program(&tokens).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule_name: "addition",
            tokens: &tokens[..],
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: &tokens[0..=0],
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "number",
                    tokens: &tokens[4..=4],
                    sub_rules: Vec::new()
                },
            ]
        }
    )
}

#[test]
fn simple_parser_with_token_usage() {
    let definition = r#"
        addition = number, _, "+", _, number;
        number = ? NUMBER ?;
        _ = ? WHITESPACE ?;
    "#;
    let formula = r#"1 + 2"#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = naive_recursive_descent_parser::Parser::new(grammar, None);
    let program_ast = parser.parse_program(&tokens).unwrap();

    assert_eq!(
        program_ast,
        RuleNode {
            rule_name: "addition",
            tokens: &tokens[..],
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: &tokens[0..=0],
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "number",
                    tokens: &tokens[4..=4],
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

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = naive_recursive_descent_parser::Parser::new(grammar, None);
    let program_ast = parser
        .parse_program(&tokens)
        .map_err(|v| {
            v.iter()
                .map(|e| e.error_string(formula))
                .fold(String::new(), |a, s| a + &s)
        })
        .unwrap();

    let expected = RuleNode {
        rule_name: "sentence",
        tokens: &tokens[..],
        sub_rules: vec![
            RuleNode {
                rule_name: "demand",
                tokens: &tokens[1..=7],
                sub_rules: vec![
                    RuleNode {
                        rule_name: "person",
                        tokens: &tokens[1..=1],
                        sub_rules: Vec::new(),
                    },
                    RuleNode {
                        rule_name: "action",
                        tokens: &tokens[5..=5],
                        sub_rules: Vec::new(),
                    },
                    RuleNode {
                        rule_name: "how",
                        tokens: &tokens[7..=7],
                        sub_rules: Vec::new(),
                    },
                ],
            },
            RuleNode {
                rule_name: "demand",
                tokens: &tokens[12..=18],
                sub_rules: vec![
                    RuleNode {
                        rule_name: "person",
                        tokens: &tokens[12..=12],
                        sub_rules: Vec::new(),
                    },
                    RuleNode {
                        rule_name: "action",
                        tokens: &tokens[14..=14],
                        sub_rules: Vec::new(),
                    },
                    RuleNode {
                        rule_name: "how",
                        tokens: &tokens[16..=18],
                        sub_rules: vec![RuleNode {
                            rule_name: "person",
                            tokens: &tokens[18..=18],
                            sub_rules: Vec::new(),
                        }],
                    },
                ],
            },
        ],
    };

    assert!(program_ast.is_similar_to(&expected));
    assert_eq!(program_ast, expected);
}

