use std::rc::Rc;

use crate::symbolization::ast::*;

use super::{execution_state::StackFrame, Interperation_result::InterpResult};

#[derive(Debug, Clone)]
pub enum Value {
    Nothing, // the value does not exist
    Break,   // execution of this function must halt
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
