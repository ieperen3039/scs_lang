use std::fmt::Display;

use super::meta_structures::Value;
use crate::symbolization::ast::{self, FunctionDeclaration, Identifier};

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
    InternalError(String),
    WhileParsing {
        function_name: Identifier,
        first_char: usize,
        last_char: usize,
        cause: Box<InterpretationError>,
    },
}

impl InterpretationError {
    pub fn while_parsing(self, function: &FunctionDeclaration) -> InterpretationError {
        todo!("locating function back to script");
        InterpretationError::WhileParsing {
            function_name: function.name.clone(),
            first_char: 0,// function.first_char(),
            last_char: 0,// function.last_char(),
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
            InterpretationError::InternalError(what) => 
                f.write_fmt(format_args!("Internal error: {what}")),
            InterpretationError::WhileParsing { function_name, first_char, last_char, cause } =>
                f.write_fmt(format_args!("{cause}\n\twhile parsing \"{function_name}\" (char {first_char} to {last_char})")),    
        }
    }
}

impl std::error::Error for InterpretationError {}
