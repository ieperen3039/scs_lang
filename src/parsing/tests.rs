use simple_error::SimpleError;

use crate::parsing::{
    ebnf_parser,
    lexer::Lexer,
    parser,
    rule_nodes::RuleNode,
    token::{Token, TokenClass},
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

    let tokens = Lexer {}
        .read_all(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    assert_eq!(
        tokens,
        vec![
            Token {
                class: TokenClass::NUMBER,
                slice: "1",
                char_idx: 0
            },
            Token {
                class: TokenClass::SYMBOL,
                slice: "+",
                char_idx: 1
            },
            Token {
                class: TokenClass::NUMBER,
                slice: "2",
                char_idx: 2
            }
        ]
    );

    let parser = parser::Parser::new(grammar, None).unwrap();
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

    let tokens = Lexer {}
        .read_all(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    assert_eq!(
        tokens,
        vec![
            Token {
                class: TokenClass::NUMBER,
                slice: "1",
                char_idx: 0
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 1
            },
            Token {
                class: TokenClass::SYMBOL,
                slice: "+",
                char_idx: 2
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 3
            },
            Token {
                class: TokenClass::NUMBER,
                slice: "2",
                char_idx: 4
            }
        ]
    );

    let parser = parser::Parser::new(grammar, None).unwrap();
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
        number = ? NUMERIC ?;
        _ = ? WHITESPACE ?;
    "#;
    let formula = r#"1 + 2"#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer {}
        .read_all(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    assert_eq!(
        tokens,
        vec![
            Token {
                class: TokenClass::NUMBER,
                slice: "1",
                char_idx: 0
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 1
            },
            Token {
                class: TokenClass::SYMBOL,
                slice: "+",
                char_idx: 2
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 3
            },
            Token {
                class: TokenClass::NUMBER,
                slice: "2",
                char_idx: 4
            }
        ]
    );

    let parser = parser::Parser::new(grammar, None).unwrap();
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

    let tokens = Lexer {}
        .read_all(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    assert_eq!(
        tokens,
        vec![
            Token {
                // 0
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 0
            },
            Token {
                class: TokenClass::IDENTIFIER,
                slice: "you",
                char_idx: 1
            },
            Token {
                // 2
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 4
            },
            Token {
                class: TokenClass::IDENTIFIER,
                slice: "must",
                char_idx: 5
            },
            Token {
                // 4
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 9
            },
            Token {
                // 5
                class: TokenClass::IDENTIFIER,
                slice: "go",
                char_idx: 10
            },
            Token {
                // 6
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 12
            },
            Token {
                class: TokenClass::IDENTIFIER,
                slice: "there",
                char_idx: 13
            },
            Token {
                // 8
                class: TokenClass::SYMBOL,
                slice: ",",
                char_idx: 18
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 19
            },
            Token {
                // 10
                class: TokenClass::IDENTIFIER,
                slice: "and",
                char_idx: 20
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 23
            },
            Token {
                // 12
                class: TokenClass::IDENTIFIER,
                slice: "I",
                char_idx: 24
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 25
            },
            Token {
                // 14
                class: TokenClass::IDENTIFIER,
                slice: "come",
                char_idx: 26
            },
            Token {
                //15
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 30
            },
            Token {
                // 16
                class: TokenClass::IDENTIFIER,
                slice: "with",
                char_idx: 31
            },
            Token {
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 35
            },
            Token {
                // 18
                class: TokenClass::IDENTIFIER,
                slice: "you",
                char_idx: 36
            },
            Token {
                class: TokenClass::SYMBOL,
                slice: "!",
                char_idx: 39
            },
            Token {
                // 20
                class: TokenClass::WHITESPACE,
                slice: " ",
                char_idx: 40
            },
        ]
    );

    let parser = parser::Parser::new(grammar, None).unwrap();
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

#[test]
fn try_parse_example_faux() {
    let definition = include_str!("../../doc/definition.ebnf");
    let program = include_str!("../../doc/example.faux");

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            err
        })
        .unwrap();

    let xml_out = std::fs::File::create("test_try_parse_example_faux_output.xml").unwrap();

    let tokens = Lexer {}.read_all(&program).map_err(|err| {
        SimpleError::new(parser::Failure::LexerError{char_idx : err}.error_string(definition))
    }).unwrap();
    let parser = parser::Parser::new(grammar, Some(xml_out)).unwrap();
    let parse_result = parser.parse_program(&tokens);

    if parse_result.is_err() {
        print!(
            "Error parsing program: \n{}",
            parse_result
                .as_ref()
                .unwrap_err()
                .into_iter()
                .map(|err| err.error_string(program) + "\n---\n\n")
                .collect::<String>()
        );
        assert!(parse_result.is_ok());
    }

    let program_ast = parse_result.unwrap();

    assert!(program_ast.rule_name == "faux_program")
}
