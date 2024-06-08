use std::{fmt::Display, rc::Rc};

use crate::symbolization::ast;

pub type InterpResult<T> = Result<T, InterpretationError>;

#[derive(Debug)]
pub enum InterpretationError {
    SymbolNotFound {
        kind: &'static str,
        symbol: ast::Identifier,
    },
    ExpectedValueGotNothing,
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
            InterpretationError::ExpectedValueGotNothing => 
                f.write_fmt(format_args!("A value was requried, but no value was returned")),
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
