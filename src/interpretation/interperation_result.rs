use std::{fmt::Display, rc::Rc};

use crate::symbolization::ast::{self, FunctionDeclaration};

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
    DoubleAssignment(),
    InternalError(String),
    WhileParsing {
        function_name: ast::Identifier,
        char_idx: usize,
        cause: Box<InterpretationError>,
    },
}

impl InterpretationError {
    pub fn while_parsing(self, function: &FunctionDeclaration) -> InterpretationError {
        InterpretationError::WhileParsing {
            function_name: function.name.clone(),
            char_idx: function.start_char,
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
            InterpretationError::DoubleAssignment() => f.write_str("Assignment lamda was executed more than once"),
            InterpretationError::InternalError(what) => 
                f.write_fmt(format_args!("Internal error: {what}")),
            InterpretationError::WhileParsing { function_name, char_idx, cause } =>
            f.write_fmt(format_args!("{cause}\n\twhile parsing \"{function_name}\" (at char {char_idx})")),
        }
    }
}

impl std::error::Error for InterpretationError {}
