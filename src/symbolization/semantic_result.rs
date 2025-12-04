use std::{cmp, fmt::Display};

use crate::parsing::rule_nodes::RuleNode;
use crate::symbolization::ast;

#[derive(Debug)]
pub enum SemanticError {
    SyntaxError {
        error_lines: Vec<String>
    },
    NodeNotFound {
        expected: &'static str,
        actual: Vec<ast::Identifier>,
    },
    UnexpectedNode {
        found: ast::Identifier,
    },
    BrokenControl(&'static str),
    TypeMismatchError {
        expected: ast::TypeRef,
        found: ast::TypeRef,
    },
    AmbiguousGenericType {
        generic_name: ast::Identifier,
        first_type: ast::TypeRef,
        second_type: ast::TypeRef,
    },    
    UnresolvedGenericType {
        generic_name: ast::Identifier,
        function: ast::Identifier,
    },
    VariableExists {
        name: ast::Identifier,
    },
    ArgumentRequired {
        par: ast::Identifier,
        function: ast::Identifier,
    },
    AmbiguousUnnamedArgument {
        arg: ast::Identifier,
        function: ast::Identifier,
    },
    ArgumentNotFound {
        arg: ast::Identifier,
        function: ast::Identifier,
    },
    InvalidNumerOfParameters {
        num_target: usize,
        num_this: usize,
    },
    ExpectedValueGotFunction {
        found_type: ast::FunctionType,
    },
    ExpectedFunctionGotValue {
        found_type: ast::TypeRef,
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
    WhileComparingType {
        expected: ast::TypeRef,
        found: ast::TypeRef,
        cause: Box<SemanticError>,
    },
    WhileParsing {
        rule_name: ast::Identifier,
        first_char: usize,
        last_char: usize,
        cause: Box<SemanticError>,
    },
    InternalError(String),
}

impl SemanticError {
    pub fn while_parsing(self, node: &RuleNode) -> SemanticError {
        SemanticError::WhileParsing {
            rule_name: ast::Identifier::from(node.rule_name),
            first_char: node.first_char(),
            last_char: node.last_char(),
            cause: Box::from(self),
        }
    }

    pub fn error_string(&self, source: &str) -> String {
        match self {
            SemanticError::WhileParsing {
                rule_name,
                first_char,
                last_char,
                cause,
            } => {
                let first_char_idx = *first_char;
                let last_char_idx = *last_char;
                let start_line_number = source[..first_char_idx]
                    .bytes()
                    .filter(|&c| c == b'\n')
                    .count()
                    + 1;
                // let end_line_number = source[first_char_idx..=last_char_idx].bytes().filter(|&c| c == b'\n').count() + start_line_number;

                let lower_newline = source[..first_char_idx]
                    .rfind(|c| c == '\n' || c == '\r')
                    .map(|v| v + 1)
                    .unwrap_or(0);

                let upper_newline = source[last_char_idx..]
                    .find(|c| c == '\n' || c == '\r')
                    .map(|v| v + last_char_idx)
                    .unwrap_or(source.len());

                let lb = cmp::max(first_char_idx as i64 - 40, lower_newline as i64) as usize;
                let ub = cmp::min(last_char_idx as i64 + 40, upper_newline as i64) as usize;
                format!(
                    "Error while parsing rule '{rule_name}' on line {start_line_number}:\n\n{0:>40}{1:<40}\n{2:>40}{2:^<width$} when parsing here\n{cause}",
                    &source[lb..first_char_idx].replace("\n", " "),
                    &source[first_char_idx..ub].replace("\n", " "),
                    "",
                    width = last_char_idx - first_char_idx
                )
            },
            _ => format!("{:?}", self),
        }
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::NodeNotFound { expected, actual } => {
                        f.write_fmt(format_args!("Could not find node \"{expected}\" among nodes {actual:?}"))
                    },
            SemanticError::UnexpectedNode { found } => {
                        f.write_fmt(format_args!("Found unexpected node \"{found}\""))
                    },
            SemanticError::BrokenControl(what) => f.write_str(what),
            SemanticError::TypeMismatchError { expected, found } => f.write_fmt(format_args!(
                        "Expected type \"{expected:?}\", but found type \"{found:?}\""
                    )),
            SemanticError::AmbiguousGenericType { generic_name, first_type, second_type } => f.write_fmt(format_args!(
                "Multiple types for generic \"{generic_name}\": first found type \"{first_type:?}\", later found type \"{second_type:?}\""
            )),
            SemanticError::UnresolvedGenericType { generic_name, function } => f.write_fmt(format_args!(
                "The type for generic \"{generic_name}\" of function \"{function}\" cannot be resolved\""
            )),
            SemanticError::VariableExists { name } => f.write_fmt(format_args!(
                        "A variable with the name \"{name}\" already exists in this scope"
                    )),
            SemanticError::ArgumentRequired { par, function } => f.write_fmt(format_args!(
                        "Parameter \"{par}\" of function \"{function}\" is required"
                    )),
            SemanticError::AmbiguousUnnamedArgument { arg, function } => f.write_fmt(format_args!(
                        "Argument \"{arg}\" of function \"{function}\" is ambiguous"
                    )),
            SemanticError::ArgumentNotFound { arg, function } => f.write_fmt(format_args!(
                        "Function \"{function}\" has no argument \"{arg}\""
                    )),
            SemanticError::InvalidNumerOfParameters {
                        num_target: num_found,
                        num_this: expected,
                    } => f.write_fmt(format_args!(
                        "Invalid number of parameters: found {num_found}, but expected {expected}"
                    )),
            SemanticError::ExpectedValueGotFunction { found_type } => f.write_fmt(format_args!(
                        "Found function type {:?} while paring value_expression",
                        found_type
                    )),
            SemanticError::ExpectedFunctionGotValue { found_type } => f.write_fmt(format_args!(
                        "Found value {:?} while paring function_expression",
                        found_type
                    )),
            SemanticError::SymbolNotFound { kind, symbol } => {
                        f.write_fmt(format_args!("Could not find {kind} \"{symbol}\""))
                    },
            SemanticError::SymbolNotFoundInScope {
                        kind,
                        symbol,
                        scope,
                    } => f.write_fmt(format_args!(
                        "Could not find {kind} \"{symbol}\" in namespace {scope:?}",
                    )),
            SemanticError::InternalError(what) => {
                        f.write_fmt(format_args!("Internal compiler error: {what}"))
                    },
            SemanticError::WhileParsing {
                        rule_name,
                        first_char,
                        last_char,
                        cause,
                    } => f.write_fmt(format_args!(
                        "{cause}\n\twhile parsing \"{rule_name}\" (char {first_char} to {last_char})"
                    )),
            SemanticError::WhileComparingType {
                expected,
                found,
                cause,
            } => f.write_fmt(format_args!(
                "{cause}\n\t(comparing \"{expected:?}\" with \"{found:?}\")"
            )),
            SemanticError::SyntaxError { error_lines } => {
                for line in error_lines {
                    f.write_str(&line)?;
                    f.write_str("\n")?;
                }
                Ok(())
            },
        }
    }
}

impl std::error::Error for SemanticError {}

pub type SemanticResult<T> = Result<T, SemanticError>;
