use regex::Regex;
use simple_error::SimpleError;

use super::parser::Failure;

const LEXER_ERROR_INDICATOR_OFFSET: usize = 20;

pub struct Lexer {}

#[derive(Clone, Debug)]
pub enum TokenClass {
    IDENTIFIER,
    NUMERIC,
    WHITESPACE,
    SYMBOL,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub class: TokenClass,
    pub slice: &'a str,
    pub char_idx: usize,
}

enum LexMethod {
    Literal(&'static str, usize),
    Regex(Regex),
    CaptureRegex(Regex),
}

impl Lexer {
    pub fn read_tokens(&self, program: &str) -> Option<(TokenClass, usize)> {
        if program.is_empty() {
            return None;
        }

        let chars = program.chars();

        let num_chars = Lexer::count(chars.clone(), char::is_whitespace);
        if num_chars > 0 {
            return Some((TokenClass::WHITESPACE, num_chars));
        }

        let first_char = chars.clone().next().unwrap();
        if first_char == '_' || first_char.is_alphabetic() {
            let num_chars = Lexer::count(chars, |c| c == '_' || c.is_alphanumeric());
            return Some((TokenClass::IDENTIFIER, num_chars));
        }

        if first_char.is_numeric() {
            let num_chars =
                Lexer::count(chars, |c| c == '_' || c == '.' || c.is_alphanumeric());
            return Some((TokenClass::IDENTIFIER, num_chars));
        }

        let num_chars = Lexer::count(chars, |c| !c.is_ascii_punctuation());
        if num_chars == 0 {
            None
        } else {
            Some((TokenClass::SYMBOL, num_chars))
        }
    }

    fn count(mut chars: std::str::Chars, predicate: fn(char) -> bool) -> usize {
        let mut num_chars = 0;
        while let Some(c) = chars.next() {
            if !predicate(c) {
                break;
            }

            num_chars += 1;
        }

        return num_chars;
    }

    pub fn read_all<'prog>(&self, string: &'prog str) -> Result<Vec<Token<'prog>>, usize> {
        let mut cursor = 0;
        let mut tokens = Vec::new();

        while cursor < string.len() {
            match self.read_tokens(&string[cursor..]) {
                Some((class, size)) => {
                    let slice = &string[cursor..(cursor + size)];
                    tokens.push(Token {
                        class,
                        slice,
                        char_idx: cursor,
                    });
                    cursor += size;
                }
                None => {
                    return Err(cursor)
                }
            }
        }

        Ok(tokens)
    }
}
