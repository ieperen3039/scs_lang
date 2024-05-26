use crate::symbolization::ast::*;
use std::fmt::Debug;

use super::meta_structures::Value;

pub struct StackFrame {
    data: Vec<Variable>,
    scope_size: Vec<usize>,
}

#[derive(Clone)]
pub struct Variable {
    pub id: VariableId,
    pub value: Value,
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}

impl StackFrame {
    pub fn new(initial_values: Vec<Variable>) -> StackFrame {
        StackFrame {
            data: initial_values,
            scope_size: Vec::new(),
        }
    }

    pub fn resolve_variable(&mut self, id: VariableId) -> Option<&mut Variable> {
        self.data.iter_mut().find(|v| v.id == id)
    }

    pub fn add_variable(&mut self, var: Variable) {
        self.data.push(var);
    }

    pub fn open_scope(&mut self) {
        self.scope_size.push(self.data.len())
    }

    pub fn close_scope(&mut self) {
        let data_retain_size = self
            .scope_size
            .pop()
            .expect("close_scope called, but no scope was open");

        while self.data.len() > data_retain_size {
            self.data.pop();
        }
    }

    pub fn unravel_to(self, id: VariableId) -> Option<Value> {
        self.data.into_iter().find(|v| v.id == id).map(|v| v.value)
    }
}
