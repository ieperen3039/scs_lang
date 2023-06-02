use simple_error::SimpleError;

use super::ebnf_ast::{EbnfAst, Rule, Term};

type EbnfParseResult<'a, T> = Result<OkResult<'a, T>, ErrResult>;

struct OkResult<'a, T> {
    val: T,
    remaining_tokens: &'a str,
}

enum ErrResult {
    Error(SimpleError),
    OutOfTokens,
    UnclosedGroup {
        tokens_remaining: usize,
    },
    UnexpectedToken {
        tokens_remaining: usize,
        while_parsing: &'static str,
    },
}

// grammar = { rule } ;
pub fn parse_ebnf(grammar: &str) -> EbnfParseResult<EbnfAst> {
    todo!()
}

// rule = identifier , "=" , term , terminator ;
pub fn process_rule(tokens: &str) -> EbnfParseResult<Rule> {
    todo!()
}

// term = "(" , term , ")"
//      | "[" , term , "]"
//      | "{" , term , "}"
//      | concatenation
//      | alternation
//      | terminal
//      | identifier ;
fn process_term(tokens: &str) -> EbnfParseResult<Term> {
    todo!()
}

// concatenation = term , "," , term , { "," , term };
fn process_concatenation(tokens: &str) -> EbnfParseResult<Term> {
    todo!()
}

// alternation = term , "|" , term , { "|" , term };
fn process_alternation(tokens: &str) -> EbnfParseResult<Term> {
    todo!()
}


// terminal = ("'" , character , { character } , "'") | ('"' , character , { character } , '"') ;
fn process_terminal_quote(tokens: &str) -> EbnfParseResult<String> {
    check_starts_with(tokens, "\'")?;

    let string_length = tokens[1..].find('\'').ok_or(ErrResult::UnclosedGroup {
        tokens_remaining: tokens.len(),
    })?;
    Ok(OkResult {
        val: String::from(&tokens[1..string_length]),
        remaining_tokens: &tokens[string_length..],
    })
}

fn process_terminal_double_quote(tokens: &str) -> EbnfParseResult<String> {
    check_starts_with(tokens, "\"")?;

    let string_length = tokens[1..].find('\"').ok_or(ErrResult::UnclosedGroup {
        tokens_remaining: tokens.len(),
    })?;
    Ok(OkResult {
        val: String::from(&tokens[1..string_length]),
        remaining_tokens: &tokens[string_length..],
    })
}

// identifier = letter , { letter | digit | "_" } ;
fn process_identifier(tokens: &str) -> EbnfParseResult<String> {
    let mut iterator = tokens.bytes();

    if let Some(c) = iterator.next() {
        if !c.is_ascii_alphabetic() {
            return Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                while_parsing: "identifier",
            });
        }

        while let Some(c) = iterator.next() {
            if !c.is_ascii_alphanumeric() && c != b'_' {
                return Err(ErrResult::UnexpectedToken {
                    tokens_remaining: tokens.len(),
                    while_parsing: "alphanumeric or underscore",
                });
            }
        }
    }

    return Err(ErrResult::OutOfTokens);
}

fn check_starts_with(tokens: &str, character: &'static str) -> Result<(), ErrResult> {
    if tokens.len() < character.len() {
        return Err(ErrResult::OutOfTokens);
    } else if !tokens.starts_with(character) {
        return Err(ErrResult::UnexpectedToken {
            tokens_remaining: tokens.len(),
            while_parsing: character,
        });
    } else {
        Ok(())
    }
}
