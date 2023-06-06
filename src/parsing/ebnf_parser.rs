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
    Error(SimpleError),
    OutOfTokens,
    UnclosedGroup {
        tokens_remaining: usize,
    },
    UnexpectedToken {
        tokens_remaining: usize,
        while_parsing: String,
    },
}

fn next_of(tokens: &str) -> &str {
    // ignore all whitespace and comments
    let mut bytes = tokens.bytes();
    let mut index = 0;

    while let Some(byte) = bytes.next() {
        if !byte.is_ascii_whitespace() {
            let comment_match = regex::Regex::new(r#"^\(\*.*\*\)"#).unwrap();
            if let Some(found) = comment_match.find_at(tokens, index) {
                return next_of(&tokens[found.end()..]);
            }

            return &tokens[index..];
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
        if result.is_ok() {
            return result;
        }

        if let Err(result_err) = result {
            match result_err {
                ErrResult::UnclosedGroup { .. } | ErrResult::Error(_) => {
                    return Err(result_err)
                }
                _ => (),
            }

            problems.push(result_err)
        }
    }

    let mut least_remaining: usize = 0;
    let mut furthest_err = ErrResult::OutOfTokens;

    for err in problems {
        match err {
            ErrResult::UnexpectedToken {
                    tokens_remaining,
                    while_parsing: _,
                } => {
                if tokens_remaining < least_remaining {
                    least_remaining = tokens_remaining;
                    furthest_err = err;
                }
            }
            ErrResult::OutOfTokens => { return Err(err) },
            _ => (),
        }
    }

    Err(furthest_err)
}

// repetition
pub fn process_repeated<'a, T: std::fmt::Debug, F>(
    tokens: &'a str,
    process_fn: F,
) -> EbnfParseResult<Vec<T>>
where
    F: Fn(&'a str) -> EbnfParseResult<'a, T>,
{
    let new_rule = process_fn(tokens)?;

    match process_repeated(new_rule.remaining_tokens, process_fn) {
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

// grammar = { rule } ;
pub fn parse_ebnf(definition: &str) -> Result<EbnfAst, ErrResult> {
    let mut rules = process_repeated(next_of(definition), process_rule)?;
    rules.val.reverse();
    // TODO check that no valid tokens are remaining
    Ok(EbnfAst { rules: rules.val })
}

// rule = identifier , "=" , alternation , terminator ;
pub fn process_rule(tokens: &str) -> EbnfParseResult<Rule> {
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
        Err(ErrResult::Error(SimpleError::new(
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
        }
        Err(ErrResult::UnexpectedToken { .. }) | Err(ErrResult::OutOfTokens) => Ok(first),
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
        }
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
        }
        Err(ErrResult::UnexpectedToken { .. }) | Err(ErrResult::OutOfTokens) => Ok(first),
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
        }
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
        &[process_terminal_simgle_quote, process_terminal_double_quote][..],
    )
}

fn process_terminal_quote(tokens: &str, char: char) -> EbnfParseResult<Term> {
    let mut tmp = [0; 4];
    check_starts_with(tokens, char.encode_utf8(&mut tmp))?;

    let string_length = tokens[1..].find(char).ok_or(ErrResult::UnclosedGroup {
        tokens_remaining: tokens.len(),
    })?;

    let string_end = string_length + 1;
    Ok(OkResult {
        val: Term::Terminal(String::from(&tokens[1..string_end])),
        remaining_tokens: next_of(&tokens[(string_end + 1)..]),
    })
}

// terminal_double_quote = ('"' , character , { character } , '"')
fn process_terminal_double_quote(tokens: &str) -> EbnfParseResult<Term> {
    process_terminal_quote(tokens, '\"')
}
// terminal_quote = ('"' , character , { character } , '"')
fn process_terminal_simgle_quote(tokens: &str) -> EbnfParseResult<Term> {
    process_terminal_quote(tokens, '\'')
}

// identifier = letter , { letter | digit | "_" } ;
fn process_identifier(tokens: &str) -> EbnfParseResult<Term> {
    let mut iterator = tokens.bytes();

    let mut name_len = 0;
    if let Some(c) = iterator.next() {
        if !c.is_ascii_alphabetic() {
            return Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                while_parsing: String::from("identifier"),
            });
        }
        name_len += 1;

        while let Some(c) = iterator.next() {
            if !c.is_ascii_alphanumeric() && c != b'_' {
                let actual_name = &tokens[..name_len];
                return Ok(OkResult {
                    val: Term::Identifier(String::from(actual_name)),
                    remaining_tokens: next_of(&tokens[name_len..]),
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

    return Err(ErrResult::OutOfTokens);
}

fn check_starts_with<'a>(tokens: &'a str, prefix: &str) -> Result<OkResult<'a, ()>, ErrResult> {
    if tokens.len() < prefix.len() {
        return Err(ErrResult::OutOfTokens);
    } else if !tokens.starts_with(prefix) {
        return Err(ErrResult::UnexpectedToken {
            tokens_remaining: tokens.len(),
            while_parsing: prefix.to_string(),
        });
    } else {
        Ok(OkResult {
            val: (),
            remaining_tokens: next_of(&tokens[prefix.len()..]),
        })
    }
}
