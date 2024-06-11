use crate::symbolization::ast::*;
use std::{
    borrow::BorrowMut,
    cell::{self, OnceCell},
    fmt::Debug,
    rc::Rc,
};

use super::value::Value;

#[derive(Clone)]
enum VarState {
    Unset,
    Value(Value),
    Future(Rc<OnceCell<Value>>),
}

#[derive(Clone)]
pub struct StackFrame {
    data: Vec<VarState>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VariableId,
    pub value: Value,
}

impl VarState {
    #[allow(dead_code)]
    pub fn is_none(&self) -> bool {
        matches!(self, VarState::Unset)
    }

    #[allow(dead_code)]
    pub fn is_some(&self) -> bool {
        !self.is_none()
    }

    pub fn to_value(self) -> Option<Value> {
        match self {
            VarState::Unset => None,
            VarState::Value(v) => Some(v),
            VarState::Future(cell) => Rc::try_unwrap(cell)
                .expect("Variable resolved while AssignmentLamda still exists")
                .into_inner(),
        }
    }
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame { data: Vec::new() }
    }

    pub fn resolve_variable(&mut self, id: VariableId) -> Option<&Value> {
        let var = self.data.get_mut(id)?;
        
        if let VarState::Future(cell_ref) = var {
            // steal the Rc
            let mut cell_val = Rc::new(OnceCell::new());
            std::mem::swap(cell_ref, &mut cell_val);

            // unwrap the cell
            let cell = Rc::try_unwrap(cell_val)
                .expect("Variable resolved while AssignmentLamda still exists");

            let new_val = match cell.into_inner() {
                Some(v) => VarState::Value(v),
                None => VarState::Unset,
            };

            self.data[id] = new_val;
        }

        match &self.data[id] {
            VarState::Unset => None,
            VarState::Value(v) => Some(v),
            _ => unreachable!(),
        }
    }

    pub fn add_variable(&mut self, var: Variable) {
        self.reserve(var.id);
        self.data[var.id] = VarState::Value(var.value);
    }

    // adds the value as a variable with the first id that is not in use
    pub fn add_argument(&mut self, arg: Value) {
        let found = self.data.iter_mut().find(|v| v.is_none());
        if let Some(position) = found {
            *position = VarState::Value(arg);
        } else {
            self.data.push(VarState::Value(arg))
        }
    }

    pub fn promise_variable(&mut self, id: VariableId) -> Rc<OnceCell<Value>> {
        self.reserve(id);
        let value_cell = Rc::new(OnceCell::new());
        self.data[id] = VarState::Future(value_cell.clone());
        value_cell
    }

    fn reserve(&mut self, size: usize) {
        if self.data.len() <= size {
            self.data.resize(size + 1, VarState::Unset)
        }
    }

    pub fn to_vec(self) -> Vec<Value> {
        self.data.into_iter().filter_map(|v| v.to_value()).collect()
    }
}

impl Debug for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("StackFrame({} variables)", self.data.len()))
    }
}
