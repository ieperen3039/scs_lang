use std::{cell::OnceCell, rc::Rc};

use crate::symbolization::ast::*;

use super::stack_frame::StackFrame;

#[derive(Debug, Clone)]
pub enum Value {
    Nothing,            // execution continues, but value is of void-type
    Break,              // execution of this statement must halt
    Return(Box<Value>), // execution of this function must halt
    Boolean(bool),
    Int(i32),
    Float(f64),
    String(String),
    Variant(u32, Box<Value>),
    Tuple(Vec<Value>),
    FunctionLamda(GlobalFunctionTarget, StackFrame),
    InlineLamda(Rc<FunctionBody>, StackFrame),
    AssignmentLamda(Rc<OnceCell<Value>>),
    IdentityLamda,
}

impl std::fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}
