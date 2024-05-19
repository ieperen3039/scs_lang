use std::collections::HashMap;

use simple_error::SimpleError;

use crate::symbolization::ast::{self, FunctionBody, Identifier};

use super::{
    built_in,
    execution_state::{ExecutionState, Variable},
    meta_structures::*,
};

pub struct Interpreter {
    state: ExecutionState,
}

impl Interpreter {
    pub fn new(program: ast::Program) -> Interpreter {

        Interpreter {
            state: ExecutionState::new(program),
        }
    }

    pub fn execute_by_name(&mut self, function: &str) -> InterpResult<String> {
        let to_execute = self.resolve_function(function)?;
        let result = self.execute_fn(to_execute)?;

        return Ok(format!("{:?}", result));
    }

    pub fn execute_fn(&mut self, target: &FunctionBody) -> InterpResult<Value> {}

    // only used to resolve the function to call, all subsequent functions are already resolved in the parsing stage
    fn resolve_function(&self, full_name: &str) -> InterpResult<&ast::FunctionBody> {
        let mut full_function_scope: Vec<&str> = full_name.split(".").collect();
        let function_name = full_function_scope.pop().unwrap();

        let mut target_scope = &self.state.namespaces;
        for ele in full_function_scope {
            target_scope = target_scope.namespaces.get(ele).ok_or_else(|| {
                InterpretationError::SymbolNotFound {
                    kind: "scope",
                    symbol: ele.to_string(),
                }
            })?;
        }
        let function_id = target_scope.functions.get(function_name).ok_or_else(|| {
            InterpretationError::SymbolNotFound {
                kind: "function",
                symbol: function_name.to_string(),
            }
        })?;

        let to_execute = self.state.resolve(full_name)
            .ok_or_else(|| InterpretationError::InternalError(
                "Function exists, but definition is not found",
            ))?;

        return match &to_execute.value {
            Value::Function(target) => Ok(target),
            Value::InternalFunction(target) => todo!("Attempt to call native function directly")
            _ => Err(InterpretationError::SymbolNotFound {
                kind: "function",
                symbol: function_name.to_string(),
            })
        };
    }
}
