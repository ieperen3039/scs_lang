use crate::symbolization::ast::*;
use std::fmt::Debug;

use super::value::Value;

#[derive(Clone)]
pub struct StackFrame {
    data: Vec<Option<Value>>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VariableId,
    pub value: Value,
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame { data: Vec::new() }
    }

    pub fn resolve_variable(&self, id: VariableId) -> Option<&Value> {
        self.data.get(id)?.as_ref()
    }

    pub fn add_variable(&mut self, var: Variable) {
        if self.data.len() <= var.id {
            self.data.resize(var.id + 1, None)
        }
        self.data[var.id] = Some(var.value);
    }

    // adds the value as a variable with the first id that is not in use
    pub fn add_argument(&mut self, arg: Value) {
        let found = self.data.iter_mut().find(|v| v.is_none());
        if let Some(position) = found {
            *position = Some(arg);
        } else {
            self.data.push(Some(arg))
        }
    }

    pub fn to_vec(self) -> Vec<Value> {
        self.data.into_iter().filter_map(|v| v).collect()
    }
}

impl Debug for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("StackFrame({} variables)", self.data.len()))
    }
}
