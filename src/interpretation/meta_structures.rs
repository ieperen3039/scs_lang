use std::{collections::HashMap, rc::Rc, result::Result};

use crate::symbolization::ast::*;

pub type InterpResult<T> = Result<T, InterpretationError>;

#[derive(Debug, Clone)]
pub enum Value {
    Nothing, // a value of type void
    Boolean(bool),
    Int(i32),
    String(Rc<str>),
    Function(FunctionBody),
    InternalFunction(NativeFunction),
    Tuple(Vec<Value>),
    // a _reference_ to a variable is an expression
    Variable(Rc<VariableDeclaration>),
}

pub type NativeFunction = fn(HashMap<String, Value>) -> InterpResult<Value>;

pub enum InterpretationError {
    TypeError { expected: TypeRef, found: TypeRef },
    ArgumentRequiredError(Parameter),
    SymbolNotFound{ kind: &'static str, symbol: String },
    InternalError(&'static str),
}