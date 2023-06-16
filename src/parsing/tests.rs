use crate::parsing::{
    ebnf_parser,
    parser::{self, RuleNode},
};

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

    let parser = parser::Parser::new(grammar, None).unwrap();
    let program_ast = parser.parse_program(formula).unwrap();

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

    let parser = parser::Parser::new(grammar, None).unwrap();
    let program_ast = parser.parse_program(formula).unwrap();

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

    let parser = parser::Parser::new(grammar, None).unwrap();
    let program_ast = parser.parse_program(formula).unwrap();

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

    let parser = parser::Parser::new(grammar, None).unwrap();
    let program_ast = parser.parse_program(formula).unwrap();

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
fn try_parse_example_scs() {
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

    let parser = parser::Parser::new(grammar, None).unwrap();
    let parse_result = parser.parse_program(program);

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
