use super::token::{Token, TokenClass};

// dot is illegal, because it makes them indistinguishable from method calls and scope references
// slash is illegal, because operators may be chained to produce a start-of-comment
// assignment is illegal, because the assignment operation is handled specially in the language
const SPECIAL_SYMBOLS: &[char] = &['(', ')', '[', ']', '{', '}', ';', '.', '=', '/'];

pub struct Lexer {
    pub ignore_whitespace: bool,
}

impl Lexer {
    pub fn read<'prog>(string: &'prog str) -> Result<Vec<Token<'prog>>, usize> {
        Lexer {
            ignore_whitespace: false,
        }
        .read_all(string)
    }

    pub fn read_all<'prog>(&self, string: &'prog str) -> Result<Vec<Token<'prog>>, usize> {
        let mut cursor = 0;
        let mut tokens = Vec::new();

        if string.starts_with("#!") {
            cursor = string.find('\n').ok_or(2 as usize)?;
        }

        while cursor < string.len() {
            match self.read_tokens(&string[cursor..]) {
                Some((TokenClass::WHITESPACE, size)) if self.ignore_whitespace => {
                    cursor += size;
                },
                Some((class, size)) => {
                    let slice = &string[cursor..(cursor + size)];
                    tokens.push(Token {
                        class,
                        slice,
                        char_idx: cursor,
                    });
                    cursor += size;
                },
                None => return Err(cursor),
            }
        }

        Ok(tokens)
    }

    pub fn read_tokens(&self, program_slice: &str) -> Option<(TokenClass, usize)> {
        if program_slice.is_empty() {
            return None;
        }

        let mut chars = program_slice.chars().peekable();
        let &first_char = chars.peek().unwrap();

        if first_char == '/' || first_char == '\r' || first_char.is_whitespace() {
            // we want to capture whitespaces and comments in the same token, because both are ignored by the parser
            let num_chars = Lexer::count_whitespaces_and_comments(chars.clone());
            if num_chars > 0 {
                return Some((TokenClass::WHITESPACE, num_chars));
            }
            // else continue
        }

        if first_char == '\"' {
            let num_chars = Lexer::count_string_chars(chars);
            return Some((TokenClass::STRING, num_chars));
        }

        // digit: [0-9][0-9_]*
        if first_char.is_digit(10) {
            let num_chars = Lexer::count(chars, |c| c == '_' || c.is_digit(10));
            return Some((TokenClass::NUMBER, num_chars));
        }

        // identifier: [a-zA-Z_][a-zA-Z0-9_]*
        if first_char == '_' || first_char.is_alphabetic() {
            let num_chars = Lexer::count(chars, |c| c == '_' || c.is_alphanumeric());
            return Some((TokenClass::IDENTIFIER, num_chars));
        }

        // all symbols with special meaning (= not part of any group)
        if SPECIAL_SYMBOLS.contains(&first_char) {
            return Some((TokenClass::SPECIAL, 1));
        }

        if first_char.is_ascii_punctuation() {
            return Some((TokenClass::OPERATOR, 1));
        }

        if first_char.is_control() {
            print!("Control caracter {:?} encountered", first_char);
        }

        None
    }

    fn count(mut chars: impl Iterator<Item = char>, predicate: fn(char) -> bool) -> usize {
        let mut num_chars = 0;
        while let Some(c) = chars.next() {
            if !predicate(c) {
                break;
            }

            num_chars += 1;
        }

        return num_chars;
    }

    fn count_whitespaces_and_comments(
        mut chars: std::iter::Peekable<impl Iterator<Item = char>>,
    ) -> usize {
        let mut num_chars = 0;
        while let Some(c) = chars.next() {
            if c == '\r' || c.is_whitespace() {
                num_chars += 1;
            } else if c == '/' {
                let next = chars.next();
                if next == Some('/') {
                    num_chars += 2;
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            num_chars += 1;
                            break;
                        }
                        num_chars += 1;
                    }
                } else if next == Some('*') {
                    num_chars += 2;
                    while let Some(c) = chars.next() {
                        if c == '*' && chars.next() == Some('/') {
                            num_chars += 2;
                            // lets just say it is valid to open a `/*` comment and never close it
                            break;
                        }

                        num_chars += 1;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        num_chars
    }

    fn count_string_chars(chars: impl Iterator<Item = char>) -> usize {
        let mut chars = chars.skip(1);
        let mut num_chars = 1;
        while let Some(c) = chars.next() {
            if c == '\"' {
                num_chars += 1;
                break;
            } else if c == '\\' {
                // escape the next char, whatever it is
                chars.next();
                num_chars += 1;
            }

            num_chars += 1;
        }
        num_chars
    }
}

pub struct StreamLexer {
    base: Lexer,
    buffer: String,
    cursor: usize,
}

impl StreamLexer {
    pub fn new(ignore_whitespace: bool) -> StreamLexer {
        StreamLexer {
            base: Lexer { ignore_whitespace },
            buffer: String::new(),
            cursor: 0,
        }
    }

    pub fn next_token<'lexer>(&'lexer mut self) -> Result<Token<'lexer>, (usize, &'lexer str)> {
        loop {
            if self.cursor >= self.buffer.len() {
                self.buffer.clear();
                std::io::stdin().read_line(&mut self.buffer).ok();

                if self.buffer.is_empty() {
                    return Err((0, &self.buffer));
                }
            }

            let char_idx = self.cursor;

            match self.base.read_tokens(&self.buffer[char_idx..]) {
                Some((TokenClass::WHITESPACE, size)) if self.base.ignore_whitespace => {
                    self.cursor += size;
                },
                Some((class, size)) => {
                    let slice = &self.buffer[char_idx..(char_idx + size)];
                    self.cursor += size;
                    return Ok(Token {
                        class,
                        slice,
                        char_idx,
                    });
                },
                None => return Err((char_idx, &self.buffer)),
            }
        }

        // Err(self.cursor)
    }
}
