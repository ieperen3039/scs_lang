mod ast;
mod proto_ast;
mod scs_parser;

use simple_error::SimpleError;

use crate::{lexer::Token, scs_lexer::ScsToken};

type ParseResult<'a, T> = Result<OkResult<'a, T>, ErrResult<'a>>;

pub struct OkResult<'a, T> {
    val: T,
    remaining_tokens: &'a [Token<'a, ScsToken>],
}

#[derive(Debug)]
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
        match a {
            ErrResult::Error(err) => a,
            ErrResult::OutOfTokens { while_parsing: wpa } => match b {
                ErrResult::Error(_) => b,
                ErrResult::OutOfTokens { .. } => a,
                ErrResult::UnexpectedToken { found, expected } => b,
            },
            ErrResult::UnexpectedToken {
                found: found_a,
                expected: mut exp_a,
            } => match b {
                ErrResult::Error(_) => b,
                ErrResult::OutOfTokens { .. } => a,
                // pick the last one, or combine the expected tokens
                ErrResult::UnexpectedToken {
                    found: found_b,
                    expected: mut exp_b,
                } if found_a.char_idx > found_b.char_idx => a,
                ErrResult::UnexpectedToken {
                    found: found_b,
                    expected: mut exp_b,
                } if found_a.char_idx < found_b.char_idx => b,
                ErrResult::UnexpectedToken {
                    found: found_b,
                    expected: mut exp_b,
                } => {
                    exp_a.append(&mut exp_b);
                    ErrResult::UnexpectedToken {
                        found: found_a,
                        expected: exp_a,
                    }
                }
            },
        }
    }
}
