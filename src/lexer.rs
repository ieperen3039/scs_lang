use regex::Regex;
use simple_error::SimpleError;

const LEXER_ERROR_INDICATOR_OFFSET : usize = 20;

pub struct Lexer<T: Copy> {
    elements: Vec<TokenDefinition<T>>,
}

struct TokenDefinition<T: Copy> {
    class: T,
    regex: Regex,
}

#[derive(Clone, Debug)]
pub struct Token<'a, T: Copy> {
    pub class: T,
    pub slice: &'a str
}

impl<T: Copy> Lexer<T> {
    pub fn new(tokens: Vec<(T, &str)>) -> Lexer<T> {
        let mut elements = Vec::new();

        for (class, regex) in tokens {
            let final_regex = String::from("^") + regex;
            elements.push(TokenDefinition {
                class,
                regex: Regex::new(&final_regex).unwrap(),
            })
        }

        Lexer { elements }
    }

    pub fn read_token(&self, string: &str) -> Option<(T, usize)> {
        for def in &self.elements {
            if let Some(found) = def.regex.find(string) {
                return Some((def.class, found.end()));
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
                    tokens.push(Token { class, slice });
                    cursor += size;
                },
                None => {
                    return Err(SimpleError::new(error_message_parse(string, cursor)))
                }
            }
        }

        Ok(tokens)
    }
}

fn error_message_parse(string: &str, index: usize) -> String {
    let start = index - LEXER_ERROR_INDICATOR_OFFSET;
    let end = index + LEXER_ERROR_INDICATOR_OFFSET;
    let mut message = String::from("Unknown symbol:\n");
    message += &string[start .. end];
    message += "\n";
    for _i in [0..LEXER_ERROR_INDICATOR_OFFSET] {
        message += " ";
    }
    message += "^ here";
    message
}
