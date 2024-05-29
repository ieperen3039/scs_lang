use super::token::{Token, TokenClass};

pub struct Lexer {
    // if true, no WHITESPACE tokens are emitted
    pub ignore_whitespace: bool,
    // any symbol that must be marked 'special'
    // must be a single char, and is considered after comments, but before literals
    pub symbols: Vec<char>,
    // any identifier that must be marked 'special'
    // is only considerd for tokens matching the 'IDENTIFIER' class
    pub keywords: Vec<&'static str>,
}

impl Lexer {
    pub fn default() -> Lexer {
        Lexer {
            ignore_whitespace: false,
            symbols: Vec::new(),
            keywords: Vec::new(),
        }
    }

    pub fn new_faux_lexer() -> Lexer {
        Lexer {
            ignore_whitespace: true,
            // any symbol here cannot be used as an operator.
            // dot is illegal, because it makes them indistinguishable from method calls and scope references.
            // slash is illegal, because operators may be chained to produce a start-of-comment.
            // assignment is illegal, because the assignment operation is a special case in the language.
            symbols: vec!['(', ')', '[', ']', '{', '}', ';', '.', '=', '/'],
            keywords: vec![
                "version", "type", "fn", "enum", "variant", "extern", "const", "this", "use", "as",
                "impl", "flag"
            ],
        }
    }

    pub fn read_faux<'prog>(string: &'prog str) -> Result<Vec<Token<'prog>>, usize> {
        Self::new_faux_lexer().read(string)
    }

    pub fn read<'prog>(&self, string: &'prog str) -> Result<Vec<Token<'prog>>, usize> {
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

        // all symbols with special meaning (= not part of any group)
        if self.symbols.contains(&first_char) {
            return Some((TokenClass::SPECIAL, 1));
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

            for &keyword in &self.keywords {
                if &program_slice[..num_chars] == keyword {
                    return Some((TokenClass::SPECIAL, num_chars));
                }
            }

            return Some((TokenClass::IDENTIFIER, num_chars));
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
    pub fn new() -> StreamLexer {
        StreamLexer {
            base: Lexer::new_faux_lexer(),
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
