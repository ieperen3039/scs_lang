

use crate::parsing::{
    ebnf_ast::*,
    ebnf_parser,
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