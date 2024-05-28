use std::fmt::Display;

use super::ast::{self};

#[derive(Debug)]
pub enum SemanticError {
    NodeNotFound {
        expected: &'static str,
        parent_node: ast::Identifier,
    },
    UnexpectedNode {
        found: ast::Identifier,
        parent_node: &'static str,
    },
    BrokenControl(&'static str),
    TypeMismatchError {
        expected: ast::TypeRef,
        found: ast::TypeRef,
    },
    VariableExists {
        name: ast::Identifier,
    },
    ArgumentRequired {
        par: ast::Identifier,
        function: ast::Identifier,
    },
    ArgumentInvalid {
        arg: ast::Identifier,
        function: ast::Identifier,
    },
    InvalidNumerOfParameters {
        what: &'static str,
        num_found: usize,
        expected: String,
    },
    SymbolNotFound {
        kind: &'static str,
        symbol: ast::Identifier,
    },
    SymbolNotFoundInScope {
        kind: &'static str,
        symbol: ast::Identifier,
        scope: Vec<ast::Identifier>,
    },
    WhileParsing {
        rule_name: &'static str,
        char_idx: usize,
        cause: Box<SemanticError>,
    },
    InternalError(&'static str),
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::NodeNotFound { expected, parent_node } => f.write_fmt(format_args!("node \"{expected}\" not found in node \"{parent_node}\"")),
            SemanticError::UnexpectedNode { found, parent_node } => f.write_fmt(format_args!("node \"{parent_node}\" contained unexpected node \"{found}\"")),
            SemanticError::BrokenControl(what) => f.write_str(what),
            SemanticError::TypeMismatchError { expected, found } => f.write_fmt(format_args!("expected type \"{:?}\", but found type \"{:?}\"", expected, found)),
            SemanticError::VariableExists { name } => f.write_fmt(format_args!("a variable with the name \"{name}\" already exists in this scope")),
            SemanticError::ArgumentRequired { par, function } => f.write_fmt(format_args!("parameter \"{par}\" of function \"{function}\" is required")),
            SemanticError::ArgumentInvalid { arg, function } => f.write_fmt(format_args!("argument \"{arg}\" of function \"{function}\" is invalid")),
            SemanticError::InvalidNumerOfParameters { what, num_found, expected, } => f.write_fmt(format_args!("invalid number of parameters for {what}: found {num_found}, but expected {expected}")),
            SemanticError::SymbolNotFound { kind, symbol } => f.write_fmt(format_args!("could not find {kind} \"{symbol}\"")),
            SemanticError::SymbolNotFoundInScope { kind, symbol, scope, } => f.write_fmt(format_args!("could not find {kind} \"{symbol}\" in namespace {:?}", scope)),
            SemanticError::InternalError(what) => f.write_fmt(format_args!("internal compiler error: {what}")),
            SemanticError::WhileParsing { rule_name, char_idx, cause } => {
                f.write_fmt(format_args!("{cause}\n\twhile parsing \"{rule_name}\" at char index {char_idx}"))
            },
        }
    }
}

impl std::error::Error for SemanticError {}

pub type SemanticResult<T> = Result<T, SemanticError>;
