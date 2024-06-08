use std::rc::Rc;

use crate::symbolization::ast::*;

use super::stack_frame::{StackFrame, Variable};

#[derive(Debug, Clone)]
pub enum Value {
    Nothing,
    Break,                          // execution of this statement must halt
    Return(Box<Value>),             // execution of this function must halt
    DelayedAssignment(Box<Variable>), // result of evaluating an AssignmentLamda
    Boolean(bool),
    Int(i32),
    String(String),
    FunctionLamda(GlobalFunctionTarget, StackFrame),
    InlineLamda(Rc<FunctionBody>, StackFrame),
    AssignmentLamda(VariableId),
    IdentityLamda,
    Tuple(Vec<Value>),
}

impl std::fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}
