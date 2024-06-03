use std::fmt::Display;

use crate::parsing::rule_nodes::RuleNode;

use super::ast::{self, Identifier};

#[derive(Debug)]
pub enum SemanticError {
    NodeNotFound {
        expected: &'static str,
    },
    UnexpectedNode {
        found: ast::Identifier,
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
        num_target: usize,
        num_this: usize,
    },
    FunctionGivenWhereValueExpected {
        found_type: ast::FunctionType,
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
        rule_name: ast::Identifier,
        first_char: usize,
        last_char: usize,
        cause: Box<SemanticError>,
    },
    InternalError(&'static str),
}

impl SemanticError {
    pub fn while_parsing(self, node: &RuleNode) -> SemanticError {
        SemanticError::WhileParsing {
            rule_name: Identifier::from(node.rule_name),
            first_char: node.first_char(),
            last_char: node.last_char(),
            cause: Box::from(self),
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::NodeNotFound { expected } => f.write_fmt(format_args!("Could not find node \"{expected}\"")),
            SemanticError::UnexpectedNode { found } => f.write_fmt(format_args!("Found unexpected node \"{found}\"")),
            SemanticError::BrokenControl(what) => f.write_str(what),
            SemanticError::TypeMismatchError { expected, found } => f.write_fmt(format_args!("Expected type \"{:?}\", but found type \"{:?}\"", expected, found)),
            SemanticError::VariableExists { name } => f.write_fmt(format_args!("A variable with the name \"{name}\" already exists in this scope")),
            SemanticError::ArgumentRequired { par, function } => f.write_fmt(format_args!("Parameter \"{par}\" of function \"{function}\" is required")),
            SemanticError::ArgumentInvalid { arg, function } => f.write_fmt(format_args!("Argument \"{arg}\" of function \"{function}\" is invalid")),
            SemanticError::InvalidNumerOfParameters { what, num_target: num_found, num_this: expected, } => f.write_fmt(format_args!("Invalid number of parameters for {what}: found {num_found}, but expected {expected}")),
            SemanticError::FunctionGivenWhereValueExpected { found_type } => f.write_fmt(format_args!("Found function type {:?} while paring value_expression", found_type)),
            SemanticError::SymbolNotFound { kind, symbol } => f.write_fmt(format_args!("Could not find {kind} \"{symbol}\"")),
            SemanticError::SymbolNotFoundInScope { kind, symbol, scope, } => f.write_fmt(format_args!("Could not find {kind} \"{symbol}\" in namespace {:?}", scope)),
            SemanticError::InternalError(what) => f.write_fmt(format_args!("Internal compiler error: {what}")),
            SemanticError::WhileParsing { rule_name, first_char, last_char, cause } => {
                f.write_fmt(format_args!("{cause}\n\twhile parsing \"{rule_name}\" (char {first_char} to {last_char})"))
            },
        }
    }
}

impl std::error::Error for SemanticError {}

pub type SemanticResult<T> = Result<T, SemanticError>;
