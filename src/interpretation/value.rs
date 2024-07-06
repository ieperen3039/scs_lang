use std::{cell::OnceCell, rc::Rc};

use crate::symbolization::{ast::*, ast_util::{RESULT_VARIANT_ID_NEG, RESULT_VARIANT_ID_POS}};

use super::stack_frame::StackFrame;

#[derive(Debug, Clone)]
pub enum Value {
    Nothing,            // execution continues, but value is of void-type
    Break,              // execution of this statement must halt
    Return(Box<Value>), // execution of this function must halt
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

impl Value {
    pub fn as_true(val: Value) -> Value {
        Value::Variant(RESULT_VARIANT_ID_POS, Box::new(val))
    }

    pub fn as_false(val: Value) -> Value {
        Value::Variant(RESULT_VARIANT_ID_NEG, Box::new(val))
    }

    pub fn new_boolean(as_true: bool) -> Value {
        if as_true {
            Value::Variant(RESULT_VARIANT_ID_POS, Box::new(Value::Nothing))
        } else {
            Value::Variant(RESULT_VARIANT_ID_NEG, Box::new(Value::Nothing))
        }
    }
}
