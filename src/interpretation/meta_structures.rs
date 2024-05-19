use std::{collections::HashMap, result::Result};

use crate::symbolization::ast::*;

pub type InterpResult<T> = Result<T, InterpretationError>;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Int(i32),
    Function(FunctionBody),
    InternalFunction(NativeFunction),
    Result(Box<(Value, Value)>)
}

pub type NativeFunction = fn(HashMap<String, Value>) -> InterpResult<Value>;

pub enum InterpretationError {
    TypeError { expected: TypeRef, found: TypeRef },
    ArgumentRequiredError(Parameter),
    SymbolNotFound{ kind: &'static str, symbol: String },
    InternalError(&'static str),
}