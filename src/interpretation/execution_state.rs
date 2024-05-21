use crate::symbolization::ast::*;
use std::fmt::Debug;

use super::meta_structures::Value;

pub struct ExecutionState {
    stack: Vec<StackFrame>,
}

pub struct StackFrame {
    data: Vec<Variable>,
    scope_size: Vec<usize>,
}

pub struct Variable {
    pub var_type: TypeRef,
    pub name: Identifier,
    pub value: Value,
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}

impl ExecutionState {
    pub fn new() -> ExecutionState {
        ExecutionState { stack: Vec::new() }
    }

    pub fn resolve_variable(&self, name: &str) -> Option<&Variable> {
        self.stack.last().unwrap().resolve_variable(name)
    }

    pub fn get_stack(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame {
            data: Vec::new(),
            scope_size: Vec::new(),
        }
    }

    pub fn resolve_variable(&self, name: &str) -> Option<&Variable> {
        self.get().iter().find(|v| v.name.as_ref() == name)
    }

    pub fn add_variable(&mut self, var: Variable) {
        self.data.push(var);
    }

    pub fn get(&self) -> &[Variable] {
        &self.data
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
}
