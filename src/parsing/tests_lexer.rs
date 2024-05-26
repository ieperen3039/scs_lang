use simple_error::SimpleError;

use crate::parsing::{
    lexer::Lexer,
    parser::Failure,
    token::{Token, TokenClass},
};

#[test]
fn simple_lexer() {
    let formula = "1+2";

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(Failure::LexerError { char_idx: err }.error_string(formula))
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
                class: TokenClass::OPERATOR,
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
}

#[test]
fn simple_parser_with_simple_ignore() {
    let formula = r#"1 + 2"#;
    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(Failure::LexerError { char_idx: err }.error_string(formula))
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
                class: TokenClass::OPERATOR,
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
}

#[test]
fn simple_parser_with_token_usage() {
    let formula = r#"1 + 2"#;

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(Failure::LexerError { char_idx: err }.error_string(formula))
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
                class: TokenClass::OPERATOR,
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
}

#[test]
fn complex_parser_with_complex_ignore() {
    let formula = r#" you must go there, and I come with you! "#;

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(Failure::LexerError { char_idx: err }.error_string(formula))
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
                class: TokenClass::OPERATOR,
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
                class: TokenClass::OPERATOR,
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
}

#[test]
fn shebang_is_ignored() {
    let formula = "#! bin/null\n1+2";

    let tokens = Lexer::default().read(&formula)
        .map_err(|err| {
            SimpleError::new(Failure::LexerError { char_idx: err }.error_string(formula))
        })
        .unwrap();

    assert_eq!(
        tokens,
        vec![
            Token {
                class: TokenClass::WHITESPACE,
                slice: "\n",
                char_idx: 11
            },
            Token {
                class: TokenClass::NUMBER,
                slice: "1",
                char_idx: 12
            },
            Token {
                class: TokenClass::OPERATOR,
                slice: "+",
                char_idx: 13
            },
            Token {
                class: TokenClass::NUMBER,
                slice: "2",
                char_idx: 14
            }
        ]
    );
}


#[test]
fn try_parse_example_faux() {
    let program = include_str!("../../examples/example.faux");

    Lexer::new_faux_lexer().read(&program)
        .map_err(|err| {
            SimpleError::new(Failure::LexerError { char_idx: err }.error_string(program))
        })
        .unwrap();
}
