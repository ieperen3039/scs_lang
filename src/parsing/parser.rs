use std::cmp;

use simple_error::SimpleError;

use super::{
    rule_nodes::RuleNode,
    token::Token,
};

pub type ParseResult<'prog, 'bnf> =
    Box<dyn Iterator<Item = Result<Interpretation<'prog, 'bnf>, Failure<'bnf>>> + 'bnf>;

pub const MAX_NUM_TOKENS_BACKTRACE_ON_ERROR: i32 = 5;
pub const MAX_NUM_TOKENS_BACKTRACE_ON_SUCCESS: usize = 100;
pub const MAX_ERRORS_PER_RULE: usize = 50;

#[derive(Debug, Clone)]
pub enum Failure<'bnf> {
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        char_idx: usize,
        expected: &'bnf str,
    },
    // the parser finished before the end of the file
    IncompleteParse {
        char_idx: usize,
    },
    // reached end of file while parsing
    EndOfFile {
        expected: &'bnf str,
    },
    // there was an error reading the characters of the file
    LexerError {
        char_idx: usize,
    },
    // the program or grammar has some unspecified syntax error
    #[allow(dead_code)]
    Error(SimpleError),
    // the parser has 'crashed' due to a compiler bug
    #[allow(dead_code)]
    InternalError(SimpleError),
}

impl Failure<'_> {
    pub fn error_string(&self, source: &str) -> String {
        match self {
            Failure::UnexpectedToken { char_idx, .. }
            | Failure::LexerError { char_idx }
            | Failure::IncompleteParse { char_idx } => {
                let offset = *char_idx;
                let line_number = source[..offset].bytes().filter(|&c| c == b'\n').count() + 1;
                let lower_newline = source[..offset]
                    .rfind(|c| c == '\n' || c == '\r')
                    .map(|v| v + 1)
                    .unwrap_or(0);
                let upper_newline = source[offset..]
                    .find(|c| c == '\n' || c == '\r')
                    .map(|v| v + offset)
                    .unwrap_or(source.len());
                let lb = cmp::max(offset as i64 - 40, lower_newline as i64) as usize;
                let ub = cmp::min(offset as i64 + 40, upper_newline as i64) as usize;
                format!(
                    "{:?} on line {line_number}:\n\n{:>40}{:<40}\n{:>40}^ when parsing here",
                    self,
                    &source[lb..offset],
                    &source[offset..ub],
                    ""
                )
            }
            _ => format!("{:?}", self),
        }
    }
}

pub struct Interpretation<'prog, 'bnf> {
    pub val: ParseNode<'prog, 'bnf>,
    pub remaining_tokens: &'prog [Token<'prog>],
}

impl<'prog, 'bnf> std::fmt::Debug for Interpretation<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum ParseNode<'prog, 'bnf> {
    Rule(RuleNode<'prog, 'bnf>),
    EmptyNode,
    Pair(Box<ParseNode<'prog, 'bnf>>, Box<ParseNode<'prog, 'bnf>>),
    Terminal(&'prog Token<'prog>),
}

// ok < err, see compare_ok_result and compare_err_result
fn compare_result(
    a: &Result<Interpretation<'_, '_>, Failure<'_>>,
    b: &Result<Interpretation<'_, '_>, Failure<'_>>,
) -> cmp::Ordering {
    match (a.is_ok(), b.is_ok()) {
        (true, false) => cmp::Ordering::Less,
        (false, true) => cmp::Ordering::Greater,
        (true, true) => compare_ok_result(a.as_ref().unwrap(), b.as_ref().unwrap()),
        (false, false) => compare_err_result(a.as_ref().unwrap_err(), b.as_ref().unwrap_err()),
    }
}

// return whether a has more tokens than b
fn compare_ok_result(a: &Interpretation<'_, '_>, b: &Interpretation<'_, '_>) -> cmp::Ordering {
    let a_len = a.remaining_tokens.len();
    let b_len = b.remaining_tokens.len();
    // smallest number of remaining tokens first
    a_len.cmp(&b_len)
}

// return whether a is more or less significant than b
pub fn compare_err_result(a: &Failure<'_>, b: &Failure<'_>) -> cmp::Ordering {
    get_err_significance(a).cmp(&get_err_significance(b))
}

pub fn get_err_significance(failure: &Failure<'_>) -> i32 {
    match failure {
        Failure::UnexpectedToken { char_idx, .. } | Failure::IncompleteParse { char_idx } => {
            *char_idx as i32
        }
        Failure::EndOfFile { .. } => i32::MAX - 5,
        Failure::LexerError { .. } => i32::MAX - 3,
        Failure::Error(_) => i32::MAX - 2,
        Failure::InternalError(_) => i32::MAX - 1,
    }
}

pub fn unwrap_to_rulenodes<'prog, 'bnf>(node: ParseNode<'prog, 'bnf>) -> Vec<RuleNode<'prog, 'bnf>> {
    match node {
        ParseNode::Rule(rule) => vec![rule],
        ParseNode::Pair(sub_rule_1, sub_rule_2) => {
            let mut first_bit = unwrap_to_rulenodes(*sub_rule_1);
            let mut second_bit = unwrap_to_rulenodes(*sub_rule_2);
            first_bit.append(&mut second_bit);
            first_bit
        }
        ParseNode::Terminal(_) | ParseNode::EmptyNode => Vec::new(),
    }
}

#[allow(dead_code)]
fn sanitize(original: String) -> String {
    original
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace("\r\n", "\\n")
        .replace("\r", "\\r")
        .replace("\n", "\\n")
}
