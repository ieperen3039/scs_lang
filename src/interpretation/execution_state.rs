use crate::{built_in, symbolization::ast::*};
use std::{collections::HashMap, fmt::Debug};

use super::meta_structures::Value;

pub struct ExecutionState {
    pub namespaces: Namespace,
    pub type_definitions: HashMap<NumericTypeIdentifier, TypeDefinition>,
    pub global_constants: Vec<Variable>,
    pub stack: Vec<StackFrame>,
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
    pub fn new(source: Program) -> ExecutionState {
        let built_in_functions = built_in::functions::get_built_in_functions();

        ExecutionState {
            namespaces: source.namespaces,
            type_definitions: source.type_definitions,
            global_constants: built_in_functions,
            stack: Vec::new(),
        }
    }

    pub fn resolve(&self, name: Identifier) -> Option<&Variable> {
        self.stack
            .last()
            .unwrap()
            .get()
            .iter()
            .find(|v| v.name == name)
            .or_else(|| self.global_constants.iter().find(|v| v.name == name))
    }
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame {
            data: Vec::new(),
            scope_size: Vec::new(),
        }
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
