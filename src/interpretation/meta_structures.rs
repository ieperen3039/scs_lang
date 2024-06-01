use std::{rc::Rc, result::Result};

use crate::symbolization::ast::*;

pub type InterpResult<T> = Result<T, InterpretationError>;

#[derive(Debug, Clone)]
pub enum Value {
    Nothing, // the value does not exist
    Void, // a value of type void
    Boolean(bool),
    Int(i32),
    String(Rc<str>),
    Function(FunctionBody),
    Tuple(Vec<Value>),
}

pub type NativeFunction = fn(Vec<Value>) -> InterpResult<Value>;

#[derive(Debug)]
pub enum InterpretationError {
    TypeError { expected: TypeRef, found: TypeRef },
    ArgumentRequiredError(Parameter),
    SymbolNotFound { kind: &'static str, symbol: Identifier },
    ArgumentTypeMismatch{ par: Parameter, args: Vec<Value> },
    InternalError(&'static str),
}
