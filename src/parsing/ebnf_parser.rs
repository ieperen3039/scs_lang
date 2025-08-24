use std::cmp;

use simple_error::SimpleError;

use super::{
    ebnf_ast::{EbnfAst, Rule, Term},
    token::TokenClass,
};

type EbnfParseResult<'a, T> = Result<OkResult<'a, T>, GrammarError>;

#[derive(Debug)]
struct OkResult<'a, T: std::fmt::Debug> {
    val: T,
    remaining_tokens: &'a str,
}

#[derive(Debug)]
pub enum GrammarError {
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
    #[allow(dead_code)]
    InternalError(SimpleError),
}

pub fn error_string(error: &GrammarError, source: &str) -> String {
    match error {
        GrammarError::EmptyError { tokens_remaining }
        | GrammarError::UnexpectedToken {
            tokens_remaining, ..
        }
        | GrammarError::UnclosedGroup { tokens_remaining } => {
            let offset = source.len() - tokens_remaining;
            let line_number = source[..=offset].bytes().filter(|c| c == &b'\n').count();
            let lower_newline = source[..=offset].rfind("\n").map(|v| v + 1).unwrap_or(0);
            let upper_newline = source[offset..]
                .find("\n")
                .map(|v| v + offset)
                .unwrap_or(source.len());
            let lb = cmp::max(offset as i64 - 40, lower_newline as i64) as usize;
            let ub = cmp::min(offset as i64 + 40, upper_newline as i64) as usize;
            format!(
                "{:?} on line {line_number},\n{:>40}{:<40}\n{:>40}^ when parsing here",
                error,
                &source[lb..offset],
                &source[offset..ub],
                ""
            )
        },
        _ => format!("{:?}", error),
    }
}

// grammar = { rule } ;
pub fn parse_ebnf(definition: &str) -> Result<EbnfAst, GrammarError> {
    let mut rules = process_repeated(skip_ignored(definition), process_rule)?;

    let ignore_rule = rules
        .val
        .iter()
        .position(|rule| rule.identifier == "ignored")
        .map(|index_of_ignored| {
            let ignore_rule = rules.val[index_of_ignored].clone();
            rules.val.remove(index_of_ignored);
            ignore_rule.pattern
        });

    let remaining_tokens = skip_ignored(rules.remaining_tokens);

    if remaining_tokens.is_empty() {
        rules.val.reverse();
        Ok(EbnfAst {
            rules: rules.val,
            ignore_rule,
        })
    } else {
        // if we remove remaining_tokens, then we have a valid program.
        // This happens whenever a rule is invalid. We try parsing that part again, which must result in a parse error.
        return Err(process_repeated(remaining_tokens, process_rule).unwrap_err());
    }
}

fn skip_ignored(tokens: &str) -> &str {
    // ignore all whitespace and comments
    let mut bytes = tokens.bytes().peekable();
    let mut index = 0;

    while let Some(byte) = bytes.next() {
        if !byte.is_ascii_whitespace() {
            if byte == b'(' && bytes.next() == Some(b'*') {
                index += 2;
                while let Some(byte) = bytes.next() {
                    if byte == b'*' && bytes.peek() == Some(&b')') {
                        bytes.next(); // consume the ')'
                        index += 1;
                        break;
                    }
                    index += 1;
                }
            } else {
                return &tokens[index..];
            }
        }

        index += 1;
    }

    &tokens[tokens.len()..]
}

// alternation
fn process_first_of<'a, T: std::fmt::Debug, F>(
    tokens: &'a str,
    rules: &[F],
) -> EbnfParseResult<'a, T>
where
    F: Fn(&'a str) -> EbnfParseResult<'a, T>,
{
    let mut problems = Vec::new();

    for rule in rules {
        let result = rule(tokens);

        match result {
            Ok(ok_result) => {
                let num_bytes_parsed = tokens.len() - ok_result.remaining_tokens.len();
                // if the remaining_tokens is the same as the input tokens, that means that this term evaluated to nothing
                if num_bytes_parsed > 0 {
                    // return the first successful
                    return Ok(ok_result);
                }
            },
            Err(GrammarError::InternalError(_)) => {
                // if this is a internal error, return it.
                return result;
            },
            Err(err_result) => {
                // otherwise, just store it in case no rule applies
                problems.push(err_result)
            },
        }
    }

    let mut least_remaining: usize = tokens.len();
    let mut furthest_err = GrammarError::EmptyError {
        tokens_remaining: least_remaining,
    };

    for err in problems {
        match err {
            GrammarError::UnexpectedToken {
                tokens_remaining,
                expected: _,
            } => {
                if tokens_remaining < least_remaining {
                    least_remaining = tokens_remaining;
                    furthest_err = err;
                }
            },
            GrammarError::OutOfTokens { .. } => return Err(err),
            _ => (),
        }
    }

    Err(furthest_err)
}

// repetition
fn process_repeated<'a, T: std::fmt::Debug, F>(
    tokens: &'a str,
    process_fn: F,
) -> EbnfParseResult<'a, Vec<T>>
where
    F: Fn(&'a str) -> EbnfParseResult<'a, T>,
{
    let new_rule = process_fn(tokens)?;

    match process_repeated(new_rule.remaining_tokens, process_fn) {
        Ok(mut terms) => {
            terms.val.push(new_rule.val);
            Ok(terms)
        },
        Err(_) => Ok(OkResult {
            val: vec![new_rule.val],
            remaining_tokens: new_rule.remaining_tokens,
        }),
    }
}

// rule = identifier , "=" , alternation , terminator ;
fn process_rule(tokens: &str) -> EbnfParseResult<Rule> {
    let identifier = process_identifier(tokens)?;
    let equal = check_starts_with(identifier.remaining_tokens, "=")?;
    let term = process_alternation(equal.remaining_tokens)?;
    let terminator = check_starts_with(term.remaining_tokens, ";")?;

    if let Term::Identifier(id_name) = identifier.val {
        Ok(OkResult {
            val: Rule {
                identifier: id_name,
                pattern: term.val,
            },
            remaining_tokens: terminator.remaining_tokens,
        })
    } else {
        Err(GrammarError::Error(SimpleError::new(
            "Identifier was not of the identifier type",
        )))
    }
}

// term = group | optional | repetition | terminal | identifier ;
fn process_term(tokens: &str) -> EbnfParseResult<Term> {
    process_first_of(
        tokens,
        &[
            process_group,
            process_optional,
            process_repetition,
            process_terminal,
            process_identifier,
        ][..],
    )
}

// group = "(" , alternation , ")"
fn process_group(tokens: &str) -> EbnfParseResult<Term> {
    let open = check_starts_with(tokens, "(")?;
    let term = process_alternation(open.remaining_tokens)?;
    let close = check_starts_with(term.remaining_tokens, ")")?;

    // we return the internal term, because the grouping only matters for parsing the BNF
    Ok(OkResult {
        val: term.val,
        remaining_tokens: close.remaining_tokens,
    })
}

// optional = "[" , alternation , "]"
fn process_optional(tokens: &str) -> EbnfParseResult<Term> {
    let open = check_starts_with(tokens, "[")?;
    let term = process_alternation(open.remaining_tokens)?;
    let close = check_starts_with(term.remaining_tokens, "]")?;

    Ok(OkResult {
        val: Term::Optional(Box::new(term.val)),
        remaining_tokens: close.remaining_tokens,
    })
}

// repetition = "{" , alternation , "}"
fn process_repetition(tokens: &str) -> EbnfParseResult<Term> {
    let open = check_starts_with(tokens, "{")?;
    let term = process_alternation(open.remaining_tokens)?;
    let close = check_starts_with(term.remaining_tokens, "}")?;

    Ok(OkResult {
        val: Term::Repetition(Box::new(term.val)),
        remaining_tokens: close.remaining_tokens,
    })
}

// concatenation = term , "," , concatenation_continue;
fn process_concatenation(tokens: &str) -> EbnfParseResult<Term> {
    let first = process_term(tokens)?;
    match process_concatenation_continue(first.remaining_tokens) {
        Ok(mut terms) => {
            terms.val.push(first.val);
            terms.val.reverse();
            Ok(OkResult {
                val: Term::Concatenation(terms.val),
                remaining_tokens: terms.remaining_tokens,
            })
        },
        Err(GrammarError::UnexpectedToken { .. }) | Err(GrammarError::OutOfTokens { .. }) => Ok(first),
        Err(err) => Err(err),
    }
}

// concatenation_continue = "," , term, [ concatenation_continue ]
fn process_concatenation_continue(tokens: &str) -> EbnfParseResult<Vec<Term>> {
    let bar = check_starts_with(tokens, ",")?;
    let new_term = process_term(bar.remaining_tokens)?;

    match process_concatenation_continue(new_term.remaining_tokens) {
        Ok(mut terms) => {
            terms.val.push(new_term.val);
            Ok(terms)
        },
        Err(_) => Ok(OkResult {
            val: vec![new_term.val],
            remaining_tokens: new_term.remaining_tokens,
        }),
    }
}

// alternation = concatenation , alternation_continue;
fn process_alternation(tokens: &str) -> EbnfParseResult<Term> {
    let first = process_concatenation(tokens)?;

    match process_alternation_continue(first.remaining_tokens) {
        Ok(mut terms) => {
            terms.val.push(first.val);
            terms.val.reverse();
            Ok(OkResult {
                val: Term::Alternation(terms.val),
                remaining_tokens: terms.remaining_tokens,
            })
        },
        Err(GrammarError::UnexpectedToken { .. }) | Err(GrammarError::OutOfTokens { .. }) => Ok(first),
        Err(err) => Err(err),
    }
}

// alternation_continue = "|" , concatenation, [ alternation_continue ]
fn process_alternation_continue(tokens: &str) -> EbnfParseResult<Vec<Term>> {
    let bar = check_starts_with(tokens, "|")?;
    let new_term = process_concatenation(bar.remaining_tokens)?;

    match process_alternation_continue(new_term.remaining_tokens) {
        Ok(mut terms) => {
            terms.val.push(new_term.val);
            Ok(terms)
        },
        Err(_) => Ok(OkResult {
            val: vec![new_term.val],
            remaining_tokens: new_term.remaining_tokens,
        }),
    }
}

// terminal = terminal_quote | terminal_double_quote
fn process_terminal<'a>(tokens: &str) -> EbnfParseResult<Term> {
    process_first_of(
        tokens,
        &[
            process_terminal_questionmark,
            process_terminal_single_quote,
            process_terminal_double_quote,
        ][..],
    )
}

// terminal_double_quote = ('"' , character , { character } , '"')
fn process_terminal_double_quote(tokens: &str) -> EbnfParseResult<Term> {
    process_terminal_with(tokens, "\"")
}

// terminal_quote = ('"' , character , { character } , '"')
fn process_terminal_single_quote(tokens: &str) -> EbnfParseResult<Term> {
    process_terminal_with(tokens, "\'")
}

fn process_terminal_with<'a>(tokens: &'a str, char: &'static str) -> EbnfParseResult<'a, Term> {
    check_starts_with(tokens, char)?;

    let string_length = tokens[1..].find(char).ok_or(GrammarError::UnclosedGroup {
        tokens_remaining: tokens.len(),
    })?;

    let string_end = string_length + 1;
    Ok(OkResult {
        val: Term::Literal(String::from(&tokens[1..string_end])),
        remaining_tokens: skip_ignored(&tokens[(string_end + 1)..]),
    })
}

// terminal_token = ('?' , character , { character } , '?')
fn process_terminal_questionmark(tokens: &str) -> EbnfParseResult<Term> {
    check_starts_with(tokens, "? ")?;

    let string_length = tokens[2..].find(" ?").ok_or(GrammarError::UnclosedGroup {
        tokens_remaining: tokens.len(),
    })?;

    let string_end = string_length + 2;
    let token_class = TokenClass::from_str(&tokens[2..string_end]);

    if token_class == TokenClass::INVALID {
        return Err(GrammarError::UnexpectedToken {
            tokens_remaining: tokens.len() - 2,
            expected: "some TokenClass",
        });
    }

    Ok(OkResult {
        val: Term::Token(token_class),
        remaining_tokens: skip_ignored(&tokens[(string_end + 2)..]),
    })
}

// identifier = letter , { letter | digit | "_" } ;
fn process_identifier(tokens: &str) -> EbnfParseResult<Term> {
    let mut iterator = tokens.bytes();

    let mut name_len = 0;
    if let Some(c) = iterator.next() {
        if !c.is_ascii_alphabetic() && c != b'_' {
            return Err(GrammarError::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: "identifier",
            });
        }
        name_len += 1;

        while let Some(c) = iterator.next() {
            // note: numbers are allowed here
            if !c.is_ascii_alphanumeric() && c != b'_' {
                let actual_name = &tokens[..name_len];
                return Ok(OkResult {
                    val: Term::Identifier(String::from(actual_name)),
                    remaining_tokens: skip_ignored(&tokens[name_len..]),
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

    return Err(GrammarError::OutOfTokens {
        expected: "identifier",
    });
}

fn check_starts_with<'a>(
    tokens: &'a str,
    prefix: &'static str,
) -> Result<OkResult<'a, ()>, GrammarError> {
    if tokens.len() < prefix.len() {
        return Err(GrammarError::OutOfTokens { expected: prefix });
    } else if !tokens.starts_with(prefix) {
        return Err(GrammarError::UnexpectedToken {
            tokens_remaining: tokens.len(),
            expected: prefix,
        });
    } else {
        Ok(OkResult {
            val: (),
            remaining_tokens: skip_ignored(&tokens[prefix.len()..]),
        })
    }
}
