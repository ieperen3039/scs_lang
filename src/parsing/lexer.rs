use regex::Regex;
use simple_error::SimpleError;

const LEXER_ERROR_INDICATOR_OFFSET: usize = 20;

pub struct FauxLexer {}

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

impl FauxLexer {
    pub fn read_token<'prog>(&self, string: &str) -> Option<(TokenClass, usize)> {
        if string.is_empty() {
            return None;
        }

        let chars = string.chars();

        let num_chars = FauxLexer::count(chars.clone(), char::is_whitespace);
        if num_chars > 0 {
            return Some((TokenClass::WHITESPACE, num_chars));
        }

        let first_char = chars.clone().next().unwrap();
        if first_char == '_' || first_char.is_alphabetic() {
            let num_chars = FauxLexer::count(chars, |c| c == '_' || c.is_alphanumeric());
            return Some((TokenClass::IDENTIFIER, num_chars));
        }

        if first_char.is_numeric() {
            let num_chars = FauxLexer::count(chars, |c| c == '_' || c == '.' || c.is_alphanumeric());
            return Some((TokenClass::IDENTIFIER, num_chars));
        }

        let num_chars = FauxLexer::count(chars, |c| !c.is_ascii_punctuation());
        return Some((TokenClass::SYMBOL, num_chars));
    }

    fn count(mut chars: std::str::Chars<'_>, predicate: fn(char) -> bool) -> usize {
        let mut num_chars = 0;
        while let Some(c) = chars.next() {
            if !predicate(c) {
                break;
            }

            num_chars += 1;
        }

        return num_chars;
    }

    pub fn read_all<'a>(&self, string: &'a str) -> Result<Vec<Token<'a>>, SimpleError> {
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