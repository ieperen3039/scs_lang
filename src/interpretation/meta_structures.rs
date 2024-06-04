use std::{rc::Rc, result::Result};

use crate::symbolization::ast::*;

use super::execution_state::StackFrame;

pub type InterpResult<T> = Result<T, InterpretationError>;

#[derive(Debug, Clone)]
pub enum Value {
    Nothing, // the value does not exist
    Break, // execution of this function must halt
    Boolean(bool),
    Int(i32),
    String(Rc<str>),
    FunctionLamda(FunctionId, StackFrame),
    InlineLamda(Rc<FunctionBody>, StackFrame),
    IdentityLamda,
    Tuple(Vec<Value>),
}

impl std::fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}

pub type NativeFunction = fn(Vec<Value>) -> InterpResult<Value>;

#[derive(Debug)]
pub enum InterpretationError {
    TypeError { expected: TypeRef, found: TypeRef },
    ArgumentRequiredError(Parameter),
    SymbolNotFound { kind: &'static str, symbol: Identifier },
    ArgumentTypeMismatch{ par_name : Identifier, expected_type: TypeRef, arg: Value },
    InternalError(&'static str),
}
