use crate::symbolization::ast::*;
use std::fmt::Debug;

use super::meta_structures::Value;

#[derive(Clone)]
pub struct StackFrame {
    data: Vec<Variable>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VariableId,
    pub value: Value,
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame {
            data: Vec::new(),
        }
    }

    pub fn resolve_variable(&mut self, id: VariableId) -> Option<&Value> {
        self.data.iter().find(|v| v.id == id).map(|v| &v.value)
    }

    pub fn add_variable(&mut self, var: Variable) {
        self.data.push(var);
    }

    pub fn add_argument(&mut self, arg: Value) {
        
    }

    pub fn unwrap_return(self, id: VariableId) -> Option<Value> {
        self.data.into_iter().find(|v| v.id == id).map(|v| v.value)
    }
}

impl Debug for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("StackFrame({} variables)", self.data.len()))
    }
}
