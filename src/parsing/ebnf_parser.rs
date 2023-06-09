use simple_error::SimpleError;

use super::ebnf_ast::{EbnfAst, Rule, Term};

pub type EbnfParseResult<'a, T> = Result<OkResult<'a, T>, ErrResult>;

#[derive(Debug)]
pub struct OkResult<'a, T: std::fmt::Debug> {
    val: T,
    remaining_tokens: &'a str,
}

#[derive(Debug)]
pub enum ErrResult {
    // the worst error of no errors at all
    EmptyError {
        tokens_remaining: usize,
    },
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        tokens_remaining: usize,
        expected: &'static str,
    },
    // the end of the program was reached, but more was expected (equivalent to an UnexpectedToken at the end of the program)
    OutOfTokens {
        expected: &'static str,
    },
    // a group was opened, but never closed (equivalent to an early-evaluated OutOfTokens)
    UnclosedGroup {
        tokens_remaining: usize,
    },
    // the program or grammar has some unspecified syntax error
    Error(SimpleError),
    // the parser has a bug
    InternalError(SimpleError),
}

struct EbnfParser {
    ignore_matcher: regex::Regex,
}

impl EbnfParser {
    // grammar = { rule } ;
    pub fn parse_ebnf(definition: &str) -> Result<EbnfAst, ErrResult> {
        let ignore_matcher = regex::Regex::new(r#"^\(\*.*\*\)"#)
            .map_err(|err| ErrResult::InternalError(SimpleError::from(err)))?;
        let parser = EbnfParser { ignore_matcher };

        let mut rules = parser.process_repeated(parser.next_of(definition), Self::process_rule)?;

        let ignore_rule = rules
            .val
            .iter()
            .position(|rule| rule.identifier == "ignored")
            .map(|index_of_ignored| {
                let ignore_rule = rules.val[index_of_ignored].clone();
                rules.val.remove(index_of_ignored);
                ignore_rule.pattern
            });

        let remaining_tokens = parser.next_of(rules.remaining_tokens);

        if remaining_tokens.is_empty() {
            rules.val.reverse();
            Ok(EbnfAst {
                rules: rules.val,
                ignore_rule,
            })
        } else {
            Err(ErrResult::UnclosedGroup {
                tokens_remaining: remaining_tokens.len(),
            })
        }
    }
    fn next_of(&self, tokens: &str) -> &str {
        // ignore all whitespace and comments
        let mut bytes = tokens.bytes();
        let mut index = 0;

        while let Some(byte) = bytes.next() {
            if !byte.is_ascii_whitespace() {
                if let Some(found) = self.ignore_matcher.find_at(tokens, index) {
                    return self.next_of(&tokens[found.end()..]);
                }

                return &tokens[index..];
            }

            index += 1;
        }

        &tokens[tokens.len()..]
    }

    // alternation
    fn process_first_of<'a, T: std::fmt::Debug, F>(
        &self, 
        tokens: &'a str,
        rules: &[F],
    ) -> EbnfParseResult<'a, T>
    where
        F: Fn(&Self, &'a str) -> EbnfParseResult<'a, T>,
    {
        let mut problems = Vec::new();

        for rule in rules {
            let result = rule(self, tokens);

            match result {
                Ok(ok_result) => {
                    let num_bytes_parsed = tokens.len() - ok_result.remaining_tokens.len();
                    // if the remaining_tokens is the same as the input tokens, that means that this term evaluated to nothing
                    if num_bytes_parsed > 0 {
                        // return the first successful
                        return Ok(ok_result);
                    }
                }
                Err(ErrResult::InternalError(_)) => {
                    // if this is a internal error, return it.
                    return result;
                }
                Err(err_result) => {
                    // otherwise, just store it in case no rule applies
                    problems.push(err_result)
                }
            }
        }

        let mut least_remaining: usize = 0;
        let mut furthest_err = ErrResult::EmptyError {
            tokens_remaining: tokens.len(),
        };

        for err in problems {
            match err {
                ErrResult::UnexpectedToken {
                    tokens_remaining,
                    expected: _,
                } => {
                    if tokens_remaining < least_remaining {
                        least_remaining = tokens_remaining;
                        furthest_err = err;
                    }
                }
                ErrResult::OutOfTokens { .. } => return Err(err),
                _ => (),
            }
        }

        Err(furthest_err)
    }

    // repetition
    pub fn process_repeated<'a, 's, T: std::fmt::Debug, F>(
        &'s self, 
        tokens: &'a str,
        process_fn: F,
    ) -> EbnfParseResult<Vec<T>>
    where
        F: Fn(&Self, &'a str) -> EbnfParseResult<'a, T>,
    {
        let new_rule = process_fn(self, tokens)?;

        match self.process_repeated(new_rule.remaining_tokens, process_fn) {
            Ok(mut terms) => {
                terms.val.push(new_rule.val);
                Ok(terms)
            }
            Err(_) => Ok(OkResult {
                val: vec![new_rule.val],
                remaining_tokens: new_rule.remaining_tokens,
            }),
        }
    }

    // rule = identifier , "=" , alternation , terminator ;
    pub fn process_rule(&self, tokens: &str) -> EbnfParseResult<Rule> {
        let identifier = self.process_identifier(tokens)?;
        let equal = self.check_starts_with(identifier.remaining_tokens, "=")?;
        let term = self.process_alternation(equal.remaining_tokens)?;
        let terminator = self.check_starts_with(term.remaining_tokens, ";")?;

        if let Term::Identifier(id_name) = identifier.val {
            Ok(OkResult {
                val: Rule {
                    identifier: id_name,
                    pattern: term.val,
                },
                remaining_tokens: terminator.remaining_tokens,
            })
        } else {
            Err(ErrResult::Error(SimpleError::new(
                "Identifier was not of the identifier type",
            )))
        }
    }

    // term = group | optional | repetition | terminal | identifier ;
    fn process_term(&self, tokens: &str) -> EbnfParseResult<Term> {
        self.process_first_of(
            tokens,
            &[
                Self::process_group,
                Self::process_optional,
                Self::process_repetition,
                Self::process_terminal,
                Self::process_identifier,
            ][..],
        )
    }

    // group = "(" , alternation , ")"
    fn process_group(&self, tokens: &str) -> EbnfParseResult<Term> {
        let open = self.check_starts_with(tokens, "(")?;
        let term = self.process_alternation(open.remaining_tokens)?;
        let close = self.check_starts_with(term.remaining_tokens, ")")?;

        // we return the internal term, because the grouping only matters for parsing the BNF
        Ok(OkResult {
            val: term.val,
            remaining_tokens: close.remaining_tokens,
        })
    }

    // optional = "[" , alternation , "]"
    fn process_optional(&self, tokens: &str) -> EbnfParseResult<Term> {
        let open = self.check_starts_with(tokens, "[")?;
        let term = self.process_alternation(open.remaining_tokens)?;
        let close = self.check_starts_with(term.remaining_tokens, "]")?;

        Ok(OkResult {
            val: Term::Optional(Box::new(term.val)),
            remaining_tokens: close.remaining_tokens,
        })
    }

    // repetition = "{" , alternation , "}"
    fn process_repetition(&self, tokens: &str) -> EbnfParseResult<Term> {
        let open = self.check_starts_with(tokens, "{")?;
        let term = self.process_alternation(open.remaining_tokens)?;
        let close = self.check_starts_with(term.remaining_tokens, "}")?;

        Ok(OkResult {
            val: Term::Repetition(Box::new(term.val)),
            remaining_tokens: close.remaining_tokens,
        })
    }

    // concatenation = term , "," , concatenation_continue;
    fn process_concatenation(&self, tokens: &str) -> EbnfParseResult<Term> {
        let first = self.process_term(tokens)?;
        match self.process_concatenation_continue(first.remaining_tokens) {
            Ok(mut terms) => {
                terms.val.push(first.val);
                terms.val.reverse();
                Ok(OkResult {
                    val: Term::Concatenation(terms.val),
                    remaining_tokens: terms.remaining_tokens,
                })
            }
            Err(ErrResult::UnexpectedToken { .. }) | Err(ErrResult::OutOfTokens { .. }) => {
                Ok(first)
            }
            Err(err) => Err(err),
        }
    }

    // concatenation_continue = "," , term, [ concatenation_continue ]
    fn process_concatenation_continue(&self, tokens: &str) -> EbnfParseResult<Vec<Term>> {
        let bar = self.check_starts_with(tokens, ",")?;
        let new_term = self.process_term(bar.remaining_tokens)?;

        match self.process_concatenation_continue(new_term.remaining_tokens) {
            Ok(mut terms) => {
                terms.val.push(new_term.val);
                Ok(terms)
            }
            Err(_) => Ok(OkResult {
                val: vec![new_term.val],
                remaining_tokens: new_term.remaining_tokens,
            }),
        }
    }

    // alternation = concatenation , alternation_continue;
    fn process_alternation(&self, tokens: &str) -> EbnfParseResult<Term> {
        let first = self.process_concatenation(tokens)?;

        match self.process_alternation_continue(first.remaining_tokens) {
            Ok(mut terms) => {
                terms.val.push(first.val);
                terms.val.reverse();
                Ok(OkResult {
                    val: Term::Alternation(terms.val),
                    remaining_tokens: terms.remaining_tokens,
                })
            }
            Err(ErrResult::UnexpectedToken { .. }) | Err(ErrResult::OutOfTokens { .. }) => {
                Ok(first)
            }
            Err(err) => Err(err),
        }
    }

    // alternation_continue = "|" , concatenation, [ alternation_continue ]
    fn process_alternation_continue(&self, tokens: &str) -> EbnfParseResult<Vec<Term>> {
        let bar = self.check_starts_with(tokens, "|")?;
        let new_term = self.process_concatenation(bar.remaining_tokens)?;

        match self.process_alternation_continue(new_term.remaining_tokens) {
            Ok(mut terms) => {
                terms.val.push(new_term.val);
                Ok(terms)
            }
            Err(_) => Ok(OkResult {
                val: vec![new_term.val],
                remaining_tokens: new_term.remaining_tokens,
            }),
        }
    }

    // terminal = terminal_quote | terminal_double_quote
    fn process_terminal<'a>(&self, tokens: &str) -> EbnfParseResult<Term> {
        self.process_first_of(
            tokens,
            &[Self::process_terminal_single_quote, Self::process_terminal_double_quote][..],
        )
    }

    fn process_terminal_quote<'a>(
        &self, 
        tokens: &'a str,
        char: &'static str,
    ) -> EbnfParseResult<'a, Term> {
        let mut tmp = [0; 4];
        self.check_starts_with(tokens, char)?;

        let string_length = tokens[1..].find(char).ok_or(ErrResult::UnclosedGroup {
            tokens_remaining: tokens.len(),
        })?;

        let string_end = string_length + 1;
        Ok(OkResult {
            val: Term::Terminal(String::from(&tokens[1..string_end])),
            remaining_tokens: self.next_of(&tokens[(string_end + 1)..]),
        })
    }

    // terminal_double_quote = ('"' , character , { character } , '"')
    fn process_terminal_double_quote(&self, tokens: &str) -> EbnfParseResult<Term> {
        self.process_terminal_quote(tokens, "\"")
    }
    // terminal_quote = ('"' , character , { character } , '"')
    fn process_terminal_single_quote(&self, tokens: &str) -> EbnfParseResult<Term> {
        self.process_terminal_quote(tokens, "\'")
    }

    // identifier = letter , { letter | digit | "_" } ;
    fn process_identifier(&self, tokens: &str) -> EbnfParseResult<Term> {
        let mut iterator = tokens.bytes();

        let mut name_len = 0;
        if let Some(c) = iterator.next() {
            if !c.is_ascii_alphabetic() {
                return Err(ErrResult::UnexpectedToken {
                    tokens_remaining: tokens.len(),
                    expected: "identifier",
                });
            }
            name_len += 1;

            while let Some(c) = iterator.next() {
                if !c.is_ascii_alphanumeric() && c != b'_' {
                    let actual_name = &tokens[..name_len];
                    return Ok(OkResult {
                        val: Term::Identifier(String::from(actual_name)),
                        remaining_tokens: self.next_of(&tokens[name_len..]),
                    });
                }
                name_len += 1;
            }

            // out of tokens; finish the current identifier
            return Ok(OkResult {
                val: Term::Identifier(String::from(tokens)),
                remaining_tokens: &tokens[tokens.len()..],
            });
        }

        return Err(ErrResult::OutOfTokens {
            expected: "identifier",
        });
    }

    fn check_starts_with<'a>(
        &self, 
        tokens: &'a str,
        prefix: &'static str,
    ) -> Result<OkResult<'a, ()>, ErrResult> {
        if tokens.len() < prefix.len() {
            return Err(ErrResult::OutOfTokens { expected: prefix });
        } else if !tokens.starts_with(prefix) {
            return Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: prefix,
            });
        } else {
            Ok(OkResult {
                val: (),
                remaining_tokens: self.next_of(&tokens[prefix.len()..]),
            })
        }
    }
}
