use std::collections::HashMap;

use simple_error::SimpleError;

use crate::{
    interpretation::execution_state::StackFrame,
    symbolization::ast::{self, FunctionBody, FunctionCall, Identifier, Program},
};

use super::{
    execution_state::{ExecutionState, Variable},
    meta_structures::*,
};

pub struct Interpreter {
    program: Program,
}

impl Interpreter {
    pub fn new(program: ast::Program) -> Interpreter {
        Interpreter { program }
    }

    pub fn execute_by_name(&mut self, function: &str) -> InterpResult<String> {
        let to_execute = self.resolve_function(function)?;
        let result = self.evaluate_fn_body(to_execute)?;

        return Ok(format!("{:?}", result));
    }

    pub fn evaluate_fn_body(&mut self, target: &FunctionBody) -> InterpResult<Value> {
        let mut value = Value::Nothing;
        let mut stack = StackFrame::new();

        for stmt in target.statements {
            value = self.evaluate_value_expression(&stmt.base_element, &mut stack)?;

            for expr in stmt.mutations {
                match expr {
                    ast::FunctionExpression::FunctionCall(call) => {
                        value = self.evaluate_function_call(call, value)?
                    },
                    ast::FunctionExpression::Assignment(var) => {
                        stack.add_variable(Variable {
                            var_type: var.var_type,
                            name: var.name,
                            value,
                        });
                        value = Value::Nothing;
                        break;
                    },
                }
            }
        }

        todo!("this does not yet follow conditional assignments etc.");
        Ok(value)
    }

    fn evaluate_value_expression(
        &mut self,
        expr: &ast::ValueExpression,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            ast::ValueExpression::Tuple(tuple) => {
                let mut tuple_values = Vec::new();
                for ele in tuple {
                    tuple_values.push(self.evaluate_value_expression(ele, stack)?);
                }
                Ok(Value::Tuple(tuple_values))
            },
            ast::ValueExpression::Literal(ast::Literal::Number(lit)) => Ok(Value::Int(*lit)),
            ast::ValueExpression::Literal(ast::Literal::String(lit)) => Ok(Value::String(*lit)),
            ast::ValueExpression::Literal(ast::Literal::Boolean(lit)) => Ok(Value::Boolean(*lit)),
            ast::ValueExpression::Variable(var) => stack
                .resolve_variable(&var.name)
                .map(|v| v.value)
                .ok_or_else(|| InterpretationError::SymbolNotFound {
                    kind: "variable",
                    symbol: var.name.to_string(),
                }),
        }
    }

    fn evaluate_function_call(
        &self,
        call: FunctionCall,
        input_value: Value,
    ) -> InterpResult<Value> {
        todo!()
        // self.program.function_definitions.get(call.id)
    }

    // only used to resolve the function to call, all subsequent functions are already resolved in the parsing stage
    fn resolve_function(&self, full_name: &str) -> InterpResult<&ast::FunctionBody> {
        let mut full_function_scope: Vec<&str> = full_name.split(".").collect();
        let function_name = full_function_scope.pop().unwrap();

        let mut target_scope = &self.program.namespaces;
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

        let to_execute = self.program.function_definitions.get(function_id);

        to_execute.ok_or_else(|| InterpretationError::SymbolNotFound {
            kind: "function",
            symbol: function_name.to_string(),
        })
    }
}
