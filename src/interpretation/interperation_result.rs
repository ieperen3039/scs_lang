use std::{fmt::Display, rc::Rc};

use super::value::Value;
use crate::symbolization::ast;

pub type InterpResult<T> = Result<T, InterpretationError>;

#[derive(Debug)]
pub enum InterpretationError {
    SymbolNotFound {
        kind: &'static str,
        symbol: ast::Identifier,
    },
    ArgumentTypeMismatch {
        par_name: ast::Identifier,
        expected_type: ast::TypeRef,
        arg: Value,
    },
    VariableHasNoValue {
        variable: Rc<ast::VariableDeclaration>
    },
    InternalError(String),
    WhileParsing {
        line_nr: usize,
        cause: Box<InterpretationError>,
    },
}

impl InterpretationError {
    pub fn while_parsing(self, line_nr: usize) -> InterpretationError {
        InterpretationError::WhileParsing {
            line_nr,// function.first_char(),
            cause: Box::from(self),
        }
    }
}

impl Display for InterpretationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpretationError::SymbolNotFound { kind, symbol } => 
                f.write_fmt(format_args!("Could not find {kind} \"{symbol}\"")),
            InterpretationError::ArgumentTypeMismatch { par_name, expected_type, arg } => 
                f.write_fmt(format_args!("Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}", expected_type, arg, par_name)),
            InterpretationError::VariableHasNoValue{ variable } => 
                f.write_fmt(format_args!("Variable {} was used, but no value has been assigned to it", variable.name)),
            InterpretationError::InternalError(what) => 
                f.write_fmt(format_args!("Internal error: {what}")),
            InterpretationError::WhileParsing { line_nr, cause } =>
                f.write_fmt(format_args!("{cause} (line {line_nr})")),    
        }
    }
}

impl std::error::Error for InterpretationError {}
