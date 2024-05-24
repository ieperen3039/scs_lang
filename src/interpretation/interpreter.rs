use crate::{
    interpretation::execution_state::StackFrame,
    symbolization::ast::{self, FunctionBody, FunctionCall, Identifier, Program},
};

use super::{execution_state::Variable, meta_structures::*};

pub struct Interpreter {
    program: Program,
}

impl Interpreter {
    pub fn new(program: ast::Program) -> Interpreter {
        Interpreter { program }
    }

    pub fn execute_by_name(&self, function: &str) -> InterpResult<String> {
        let to_execute = self.resolve_function(function)?;
        let result = self.evaluate_fn_body(to_execute)?;

        return Ok(format!("{:?}", result));
    }

    pub fn evaluate_fn_body(&self, target: &FunctionBody) -> InterpResult<Value> {
        let mut value = Value::Nothing;
        let mut stack = StackFrame::new();

        for stmt in target.statements {
            value = self.evaluate_value_expression(&stmt.base_element, &mut stack)?;

            for expr in stmt.mutations {
                if matches!(value, Value::Nothing) {
                    // the value does not exist, and the expressions that follow are not executed
                    break;
                }

                value = match expr {
                    ast::FunctionExpression::FunctionCall(call) => {
                        let body = self
                            .program
                            .function_definitions
                            .get(&call.id)
                            .expect("FunctionCall must be valid");
                        self.evaluate_fn_body(body)?
                    },
                    ast::FunctionExpression::Assignment(var) => {
                        stack.add_variable(Variable {
                            var_type: var.var_type,
                            name: var.name,
                            value,
                        });
                        Value::Nothing
                    },
                    ast::FunctionExpression::Operator(_) => todo!(),
                    ast::FunctionExpression::Lamda(_) => todo!(),
                }
            }

            debug_assert!(matches!(value, Value::Nothing));
        }

        stack.resolve_variable("return")
            .map(|v| v.value)
            .ok_or_else(|| InterpretationError::SymbolNotFound { kind: "variable", symbol: String::from("return") })
    }

    fn evaluate_value_expression(
        &self,
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
            ast::ValueExpression::Literal(ast::Literal::String(lit)) => {
                Ok(Value::String(lit.clone()))
            },
            ast::ValueExpression::Literal(ast::Literal::Boolean(lit)) => Ok(Value::Boolean(*lit)),
            ast::ValueExpression::Variable(var) => Ok(stack
                .resolve_variable(&var.name)
                .map(|v| v.value)
                // syntactially the variable exists in this scope.
                // If we do not have a value stored, then the variable is conditionally (not) assigned
                .unwrap_or(Value::Nothing)),
        }
    }

    // only used to resolve the function to call, all subsequent functions are already resolved in the parsing stage
    fn resolve_function(&self, full_name: &str) -> InterpResult<&ast::FunctionBody> {
        let mut full_function_scope: Vec<&str> = full_name.split(".").collect();
        let function_name = full_function_scope.pop().unwrap();

        let mut target_scope = &self.program.namespaces;
        for ele in full_function_scope {
            target_scope = target_scope.namespaces.get(ele).ok_or_else(|| {
                InterpretationError::SymbolNotFound {
                    kind: "namespace",
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
