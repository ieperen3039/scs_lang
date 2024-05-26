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
    InternalFunction(NativeFunction),
    Tuple(Vec<Value>),
}

pub type NativeFunction = fn(Vec<Value>) -> InterpResult<Value>;

pub enum InterpretationError {
    TypeError { expected: TypeRef, found: TypeRef },
    ArgumentRequiredError(Parameter),
    SymbolNotFound { kind: &'static str, symbol: Identifier },
    InternalError(&'static str),
}
