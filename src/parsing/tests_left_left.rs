use simple_error::SimpleError;

use crate::{
    parsing::{ebnf_parser, left_left_parser, lexer::Lexer, parser, rule_nodes::RuleNode},
    transforming::grammatificator,
};

#[test]
fn simple_lexer_and_parser() {
    let definition = r#"
        addition = number, "+", number;
        number = "0" | "1" | "2";
    "#;
    let formula = "1+2";

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .map_err(|err| {
            SimpleError::new(ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer::read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = left_left_parser::Parser::new(grammar, None);
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
fn simple_parser_with_repetition() {
    let definition = r#"
        addition = number, _, "+", _, number;
        number = "0" | "1" | "2";
        _ = { "." };
    "#;
    let formula = r#"1+..2"#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer::read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = left_left_parser::Parser::new(grammar, None);
    let program_ast = parser
        .parse_program(&tokens)
        .map_err(|v| {
            println!(
                "{}",
                v.iter()
                    .map(|e| e.error_string(formula))
                    .fold(String::new(), |a, s| a + "\n" + &s)
            );
        })
        .unwrap();

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
        .map(grammatificator::convert_to_grammar)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer::read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = left_left_parser::Parser::new(grammar, None);
    let program_ast = parser
        .parse_program(&tokens)
        .map_err(|v| {
            println!(
                "{}",
                v.iter()
                    .map(|e| e.error_string(formula))
                    .fold(String::new(), |a, s| a + "\n" + &s)
            );
        })
        .unwrap();

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
        sentence     = _, demand, _, [ "and", _, demand ], _;
        demand       = person, _, [ "must", _ ], action, _, how;
        person       = "you" | "I" | "he";
        action       = _meet_action | _walk_action;
        _walk_action = "stay" | "come" | "go";
        _meet_action = "meet", _, person;
        how          = "here" | "there" | "with", _, person;
        _            = { ? WHITESPACE ? | "," | "!" | "?" };
    "#;

    let formula = r#" you must go there, and I come with you! "#;

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .map_err(|err| {
            println!("{}", ebnf_parser::error_string(&err, definition));
            err
        })
        .unwrap();

    let tokens = Lexer::read(&formula)
        .map_err(|err| {
            SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
        })
        .unwrap();

    let parser = left_left_parser::Parser::new(grammar, None);
    let program_ast = parser
        .parse_program(&tokens)
        .map_err(|v| {
            println!(
                "{}",
                v.iter()
                    .map(|e| e.error_string(formula))
                    .fold(String::new(), |a, s| a + "\n" + &s)
            );
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

    let start = std::time::Instant::now();

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map(grammatificator::convert_to_grammar)
        .map_err(|err| {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            err
        })
        .unwrap();

    let xml_out = Some(std::fs::File::create("test_try_parse_example_faux_output.xml").unwrap());
    let parser = left_left_parser::Parser::new(grammar, xml_out);

    println!("Reading grammar done (took {:?})", start.elapsed());

    let start = std::time::Instant::now();

    let tokens = Lexer {
        ignore_whitespace: true,
    }
    .read_all(&program)
    .map_err(|err| {
        SimpleError::new(parser::Failure::LexerError { char_idx: err }.error_string(definition))
    })
    .unwrap();
    let parse_result = parser.parse_program(&tokens);

    println!("Parsing done (took {:?})", start.elapsed());

    assert!(
        parse_result.is_ok(),
        "Error parsing program: \n{}",
        parse_result
            .as_ref()
            .unwrap_err()
            .into_iter()
            .map(|err| err.error_string(program) + "\n---\n\n")
            .collect::<String>()
    );

    let program_ast = parse_result.unwrap();

    assert!(program_ast.rule_name == "faux_program")
}
