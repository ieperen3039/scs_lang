mod ast;
mod scs_parser;
mod ebnf_ast;
mod ebnf_parser;

use simple_error::SimpleError;

use crate::{lexer::Token, scs_lexer::ScsToken};

type ParseResult<'a, T> = Result<OkResult<'a, T>, ErrResult<'a>>;

pub struct OkResult<'a, T> {
    val: T,
    remaining_tokens: &'a [Token<'a, ScsToken>],
}

#[derive(Debug, Clone)]
pub enum ErrResult<'a> {
    Error(SimpleError),
    OutOfTokens {
        while_parsing: String,
    },
    UnexpectedToken {
        found: Token<'a, ScsToken>,
        expected: Vec<ScsToken>,
    },
}

impl <'a, T> std::fmt::Debug for OkResult<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OkResult").finish_non_exhaustive()
    }
}

impl<'a> ErrResult<'a> {
    fn combine(a: ErrResult<'a>, b: ErrResult<'a>) -> ErrResult<'a> {
        match &a {
            ErrResult::Error(_) => a,
            ErrResult::OutOfTokens { .. } => match b {
                ErrResult::Error(_) => b,
                ErrResult::OutOfTokens { .. } => a,
                ErrResult::UnexpectedToken { .. } => b,
            },
            ErrResult::UnexpectedToken {
                found: found_a,
                expected: exp_a,
            } => match &b {
                ErrResult::Error(_) => b,
                ErrResult::OutOfTokens { .. } => a,
                // pick the last one, or combine the expected tokens
                ErrResult::UnexpectedToken {
                    found: found_b,
                    expected: _,
                } if found_a.char_idx > found_b.char_idx => a,
                ErrResult::UnexpectedToken {
                    found: found_b,
                    expected: _,
                } if found_a.char_idx < found_b.char_idx => b,
                ErrResult::UnexpectedToken {
                    found: _,
                    expected: exp_b,
                } => {
                    let mut new_vec = Vec::new();
                    new_vec.append(&mut exp_a.clone());
                    new_vec.append(&mut exp_b.clone());

                    ErrResult::UnexpectedToken {
                        found: found_a.clone(),
                        expected: exp_a.clone(),
                    }
                }
            },
        }
    }
}
