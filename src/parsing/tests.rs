use crate::parsing::{
    ebnf_parser,
    parser, rule_nodes::RuleNode,
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
            rule_name: "addition",
            tokens: "1+2",
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "number",
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
            rule_name: "addition",
            tokens: "1 + 2",
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "number",
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
            rule_name: "sentence",
            tokens: " you must go there, and I come with you! ",
            sub_rules: vec![
                RuleNode {
                    rule_name: "demand",
                    tokens: "you must go there",
                    sub_rules: vec![
                        RuleNode {
                            rule_name: "person",
                            tokens: "you",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule_name: "action",
                            tokens: "go",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule_name: "how",
                            tokens: "there",
                            sub_rules: Vec::new()
                        }
                    ]
                },
                RuleNode {
                    rule_name: "demand",
                    tokens: "I come with you",
                    sub_rules: vec![
                        RuleNode {
                            rule_name: "person",
                            tokens: "I",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule_name: "action",
                            tokens: "come",
                            sub_rules: Vec::new()
                        },
                        RuleNode {
                            rule_name: "how",
                            tokens: "with you",
                            sub_rules: vec![RuleNode {
                                rule_name: "person",
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
            rule_name: "addition",
            tokens: "1+2",
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "number",
                    tokens: "2",
                    sub_rules: Vec::new()
                },
            ]
        }
    )
}

#[test]
fn test_implicit_operator()
{
    // based on the observation that an implicit operator in the example could not be parsed
    let definition = r#"
        formula = number, { _space, _chain }, _space, ";";
        _chain = chain_plus | chain_minus | chain_multiply;
        chain_plus = "+", _space, number;
        chain_minus = "-", _space, number;
        chain_multiply = number;
        _space = { "/[\s\n]+/" };
        number = "/\d+/";
    "#;
    let formula = "1 + 2 - 3 4 + 5;";

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
            rule_name: "formula",
            tokens: "1 + 2 - 3 4 + 5;",
            sub_rules: vec![
                RuleNode {
                    rule_name: "number",
                    tokens: "1",
                    sub_rules: Vec::new()
                },
                RuleNode {
                    rule_name: "chain_plus",
                    tokens: "+ 2",
                    sub_rules: vec![
                        RuleNode {
                            rule_name: "number",
                            tokens: "2",
                            sub_rules: Vec::new()
                        },
                    ]
                },
                RuleNode {
                    rule_name: "chain_minus",
                    tokens: "- 3",
                    sub_rules: vec![
                        RuleNode {
                            rule_name: "number",
                            tokens: "3",
                            sub_rules: Vec::new()
                        },
                    ]
                },
                RuleNode {
                    rule_name: "chain_multiply",
                    tokens: "4",
                    sub_rules: vec![
                        RuleNode {
                            rule_name: "number",
                            tokens: "4",
                            sub_rules: Vec::new()
                        },
                    ]
                },
                RuleNode {
                    rule_name: "chain_plus",
                    tokens: "+ 5",
                    sub_rules: vec![
                        RuleNode {
                            rule_name: "number",
                            tokens: "5",
                            sub_rules: Vec::new()
                        },
                    ]
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

    let xml_out = std::fs::File::create("test_try_parse_example_scs_output.xml").unwrap();

    let parser = parser::Parser::new(grammar, Some(xml_out)).unwrap();
    let parse_result = parser.parse_program(program);

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

    assert!(program_ast.rule_name == "scs_program")
}
