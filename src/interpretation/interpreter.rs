use crate::{
    interpretation::execution_state::StackFrame,
    symbolization::ast::{self, FunctionBody, Identifier, Program},
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
        let result = self.evaluate_fn_body(to_execute, Vec::new())?;

        return Ok(format!("{:?}", result));
    }

    pub fn evaluate_fn_body(&self, target: &FunctionBody, arguments: Vec<Variable>) -> InterpResult<Value> {
        let mut stack = StackFrame::new(arguments);
        
        let mut expr_value;

        for stmt in &target.statements {
            expr_value = self.evaluate_value_expression(&stmt.base_element, &mut stack)?;

            for expr in &stmt.mutations {
                if matches!(expr_value, Value::Nothing) {
                    // the value does not exist, and the expressions that follow are not executed
                    break;
                }

                expr_value = match expr {
                    ast::FunctionExpression::FunctionCall(call) => {
                        let body = self
                            .program
                            .function_definitions
                            .get(&call.id)
                            .expect("FunctionCall must be valid");

                        let mut actual_arguments = Vec::new();
                        let mut var_id = 0;
                        for ele in &call.arguments {
                            let arg_value = match ele {
                                Some(expr) => self.evaluate_expression(expr, &mut stack)?,
                                None => {
                                    debug_assert!(!matches!(expr_value, Value::Nothing));
                                    std::mem::replace(&mut expr_value, Value::Nothing)
                                },
                            };
                            actual_arguments.push(Variable{ id: var_id, value: arg_value});
                            // assume that all parameters have consectutive variable ids starting from 0
                            var_id += 1;
                        }

                        self.evaluate_fn_body(body, actual_arguments)?
                    },
                    ast::FunctionExpression::Assignment(var) => {
                        stack.add_variable(Variable {
                            id: var.id,
                            value: expr_value,
                        });
                        Value::Nothing
                    },
                    ast::FunctionExpression::Operator(op) => todo!(),
                    ast::FunctionExpression::Lamda(lamda) => {
                        let mut actual_arguments = Vec::new();
                        for ele in &lamda.capture {
                            let variable = stack.resolve_variable(ele.id).unwrap();
                            actual_arguments.push(variable.clone())
                        }

                        self.evaluate_fn_body(&lamda.body, actual_arguments)?
                    }
                }
            }

            debug_assert!(matches!(expr_value, Value::Nothing));
        }

        stack.unravel_to(target.return_var.id)
            .ok_or_else(|| InterpretationError::SymbolNotFound { kind: "variable", symbol: target.return_var.name.clone() })
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
                .resolve_variable(var.id)
                .map(|v| v.value.clone())
                // syntactially the variable exists in this scope.
                // If we do not have a value stored, then the variable is conditionally (not) assigned
                .unwrap_or(Value::Nothing)),
            ast::ValueExpression::FunctionAsValue(fn_value) => self.evaluate_expression(expr, stack),
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
                    symbol: Identifier::from(ele),
                }
            })?;
        }
        let function_id = target_scope.functions.get(function_name).ok_or_else(|| {
            InterpretationError::SymbolNotFound {
                kind: "function",
                symbol: Identifier::from(function_name),
            }
        })?;

        let to_execute = self.program.function_definitions.get(function_id);

        to_execute.ok_or_else(|| InterpretationError::SymbolNotFound {
            kind: "function",
            symbol: Identifier::from(function_name),
        })
    }
}
