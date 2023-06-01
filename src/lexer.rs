use regex::Regex;
use simple_error::SimpleError;

const LEXER_ERROR_INDICATOR_OFFSET: usize = 20;

pub struct Lexer<T: Copy> {
    elements: Vec<TokenDefinition<T>>,
}

struct TokenDefinition<T: Copy> {
    class: T,
    lex_method: LexMethod,
}

enum LexMethod {
    Literal(&'static str, usize),
    Regex(Regex),
    CaptureRegex(Regex),
}

#[derive(Clone, Debug)]
pub struct Token<'a, T: Copy> {
    pub class: T,
    pub slice: &'a str,
    pub char_idx: usize,
}

pub enum RawLexMethod {
    Literal(&'static str),
    Word(&'static str),
    Regex(&'static str),
}

impl<T: Copy> Lexer<T> {
    pub fn new(tokens: Vec<(T, RawLexMethod)>) -> Lexer<T> {
        let mut elements = Vec::new();

        for (class, method) in tokens {
            match method {
                RawLexMethod::Regex(regex) => {
                    let final_regex = String::from("^") + regex;
                    elements.push(TokenDefinition {
                        class,
                        lex_method: LexMethod::Regex(Regex::new(&final_regex).unwrap()),
                    })
                }
                RawLexMethod::Literal(string) => elements.push(TokenDefinition {
                    class,
                    lex_method: LexMethod::Literal(string, string.len()),
                }),
                RawLexMethod::Word(string) => {
                    let final_regex = String::from("^(") + string + ")[^a-zA-Z0-9_]";
                    elements.push(TokenDefinition {
                        class,
                        lex_method: LexMethod::CaptureRegex(Regex::new(&final_regex).unwrap()),
                    });
                },
            }
        }

        Lexer { elements }
    }

    pub fn read_token(&self, string: &str) -> Option<(T, usize)> {
        for def in &self.elements {
            match &def.lex_method {
                LexMethod::Literal(literal, size) => {
                    if string.starts_with(literal) {
                        return Some((def.class, *size));
                    }
                }
                LexMethod::Regex(regex) => {
                    if let Some(found) = regex.find(string) {
                        return Some((def.class, found.end()));
                    }
                },
                LexMethod::CaptureRegex(regex) => {
                    if let Some(found) = regex.captures(string) {
                        return Some((def.class, found.get(1).unwrap().end()));
                    }
                }
            }
        }

        None
    }

    pub fn read_all<'a>(&self, string: &'a str) -> Result<Vec<Token<'a, T>>, SimpleError> {
        let mut cursor = 0;
        let mut tokens = Vec::new();

        while cursor < string.len() {
            match self.read_token(&string[cursor..]) {
                Some((class, size)) => {
                    let slice = &string[cursor..(cursor + size)];
                    tokens.push(Token {
                        class,
                        slice,
                        char_idx: cursor,
                    });
                    cursor += size;
                }
                None => return Err(SimpleError::new(error_message_parse(string, cursor))),
            }
        }

        Ok(tokens)
    }
}

fn error_message_parse(string: &str, index: usize) -> String {
    let start = index - LEXER_ERROR_INDICATOR_OFFSET;
    let end = index + LEXER_ERROR_INDICATOR_OFFSET;
    let mut message = String::from("Unknown symbol:\n");
    message += &string[start..end];
    message += "\n";
    for _i in [0..LEXER_ERROR_INDICATOR_OFFSET] {
        message += " ";
    }
    message += "^ here";
    message
}
