use crate::{
    built_in::functions::InternalFunctions, interpretation::stack_frame::StackFrame,
    symbolization::ast::*,
};

use super::{
    interperation_result::{InterpResult, InterpretationError},
    stack_frame::Variable,
    value::*,
};

pub struct Interpreter {
    program: FileAst,
    internal_functions: InternalFunctions,
}

impl Interpreter {
    pub fn new(program: FileAst, internal_functions: InternalFunctions) -> Interpreter {
        Interpreter {
            program,
            internal_functions,
        }
    }

    pub fn execute_main(&self) -> InterpResult<String> {
        let to_execute = self.get_defined_fn(self.program.entry_function);
        let result = self.evaluate_fn_body(to_execute, StackFrame::new())?;

        return Ok(format!("{:?}", result));
    }

    pub fn execute_by_name(&self, function: &str) -> InterpResult<String> {
        let to_execute = self
            .program
            .resolve_function(function)
            .map_err(|e| InterpretationError::InternalError(e.to_string()))?;
        let result = self.evaluate_fn_body(to_execute, StackFrame::new())?;

        return Ok(format!("{:?}", result));
    }

    pub fn get_defined_fn(&self, id: FunctionId) -> &FunctionBody {
        self.program
            .function_definitions
            .get(&id)
            .expect("FunctionCall must be valid")
    }

    pub fn evaluate_fn_body(
        &self,
        target: &FunctionBody,
        arguments: StackFrame,
    ) -> InterpResult<Value> {
        let mut stack = arguments;

        let mut expr_value: Value;

        for stmt in &target.statements {
            expr_value = self.evaluate_value_expression(&stmt.base_element, &mut stack)?;

            for expr in &stmt.mutations {
                match expr_value {
                    Value::Break => break,
                    Value::Nothing => panic!("expr_value == Nothing"),
                    Value::DelayedAssignment(variable) => {
                        stack.add_variable(*variable);
                        expr_value = Value::Break;
                        break;
                    },
                    Value::Return(value) => return Ok(*value),
                    _ => {},
                }

                expr_value = self.evaluate_function_expression(expr, expr_value, &mut stack)?;
            }

            debug_assert!(matches!(expr_value, Value::Break));
        }

        unreachable!("functions must have a return expression")
    }

    fn evaluate_value_expression(
        &self,
        expr: &ValueExpression,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            ValueExpression::Literal(Literal::Number(lit)) => Ok(Value::Int(*lit)),
            ValueExpression::Literal(Literal::String(lit)) => Ok(Value::String(lit.to_string())),
            ValueExpression::Literal(Literal::Boolean(lit)) => Ok(Value::Boolean(*lit)),
            ValueExpression::Tuple(tuple) => {
                let mut tuple_values = Vec::new();
                for ele in tuple {
                    let expr_result = self.evaluate_value_expression(ele, stack);
                    match expr_result {
                        Ok(Value::Break) | Ok(Value::Nothing) => return Err(InterpretationError::ExpectedValueGotNothing),
                        Ok(value) => tuple_values.push(value),
                        Err(err) => return Err(err),
                    }
                }
                Ok(Value::Tuple(tuple_values))
            },
            ValueExpression::Variable(var) => {
                let value = stack
                    .resolve_variable(*var)
                    .cloned()
                    // syntactially the variable exists in this scope.
                    // If we do not have a value stored, then the variable is conditionally (not) assigned
                    .unwrap_or(Value::Break);
                Ok(value)
            },
            ValueExpression::FunctionAsValue(fn_expr) => {
                self.evaluate_function_expression(fn_expr, Value::Nothing, stack)
            },
            ValueExpression::FunctionCall(fn_call) => self.evaluate_function_call(
                fn_call.target,
                &fn_call.arguments,
                true,
                Value::Nothing,
                stack,
            ),
        }
    }

    fn evaluate_function_expression(
        &self,
        expr: &FunctionExpression,
        expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            FunctionExpression::FunctionCall(call) => {
                self.evaluate_function_call(call.target, &call.arguments, false, expr_value, stack)
            },
            FunctionExpression::Assignment(var) => {
                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::AssignmentLamda(var.id));
                }

                println!("{} = {:?}", var.name, expr_value);

                if var.is_return {
                    Ok(Value::Return(Box::from(expr_value)))
                } else {
                    stack.add_variable(Variable {
                        id: var.id,
                        value: expr_value,
                    });
                    Ok(Value::Break)
                }
            },
            FunctionExpression::Operator(op) => {
                let arg_value =
                    self.evaluate_function_expression(&op.arg, Value::Nothing, stack)?;

                let mut function_stack = StackFrame::new();
                function_stack.add_variable(Variable {
                    id: 1,
                    value: arg_value,
                });

                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::FunctionLamda(op.target, function_stack));
                }

                function_stack.add_variable(Variable {
                    id: 0,
                    value: expr_value,
                });

                match op.target {
                    GlobalFunctionTarget::Defined(id) => {
                        self.evaluate_fn_body(self.get_defined_fn(id), function_stack)
                    },
                    GlobalFunctionTarget::Native(id) => self.evaluate_native(id, function_stack),
                }
            },
            FunctionExpression::Lamda(lamda) => {
                let mut lamda_arguments = StackFrame::new();
                let mut var_id = lamda.parameters.len();
                for ele in &lamda.capture {
                    if let Some(value) = stack.resolve_variable(ele.id) {
                        lamda_arguments.add_variable(Variable {
                            id: var_id,
                            value: value.clone(),
                        });
                        var_id += 1;
                    } else {
                        return Err(InterpretationError::VariableHasNoValue {
                            variable: ele.clone(),
                        });
                    }
                }

                if matches!(expr_value, Value::Nothing) {
                    if lamda.parameters.is_empty() {
                        return self.evaluate_fn_body(&lamda.body, lamda_arguments);
                    } else {
                        return Ok(Value::InlineLamda(lamda.body.clone(), lamda_arguments));
                    }
                }

                assert!(lamda.parameters.len() == 1);
                lamda_arguments.add_variable(Variable {
                    id: 0,
                    value: expr_value,
                });

                self.evaluate_fn_body(&lamda.body, lamda_arguments)
            },
            FunctionExpression::Cast(_) => {
                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::IdentityLamda);
                }

                Ok(expr_value) /* nothing to do */
            },
        }
    }

    fn evaluate_function_call(
        &self,
        target: LocalFunctionTarget,
        arguments: &Vec<Option<ValueExpression>>,
        require_full_evaluation: bool,
        expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match target {
            LocalFunctionTarget::Local(id) => {
                self.evaluate_lamda_call(id, arguments, expr_value, stack)
            },
            LocalFunctionTarget::Defined(id) => {
                let function_stack = self.build_function_stack(
                    arguments,
                    require_full_evaluation,
                    expr_value,
                    stack,
                )?;
                self.evaluate_fn_body(self.get_defined_fn(id), function_stack)
            },
            LocalFunctionTarget::Native(id) => {
                let function_stack = self.build_function_stack(
                    arguments,
                    require_full_evaluation,
                    expr_value,
                    stack,
                )?;
                self.evaluate_native(id, function_stack)
            },
        }
    }

    fn build_function_stack(
        &self,
        arguments: &Vec<Option<ValueExpression>>,
        require_full_evaluation: bool,
        mut expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<StackFrame> {
        let mut function_stack = StackFrame::new();
        let mut var_id = 0;
        for ele in arguments {
            let arg_value = match ele {
                Some(expr) => self.evaluate_value_expression(expr, stack)?,
                // if expr_value starts not-nothing, then the semantic analysis has verified
                // that this function call requires exactly one parameter.
                None => std::mem::replace(&mut expr_value, Value::Nothing),
            };

            if require_full_evaluation && matches!(arg_value, Value::Nothing) {
                panic!(
                    "Function call must evaluate to a value, but parameter {var_id} has no value"
                );
            } else {
                function_stack.add_variable(Variable {
                    id: var_id,
                    value: arg_value,
                });
            }
            // assume that all parameters have consectutive variable ids starting from 0
            var_id += 1;
        }

        Ok(function_stack)
    }

    fn evaluate_lamda_call(
        &self,
        variable_id: VariableId,
        arguments: &Vec<Option<ValueExpression>>,
        expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        // if expr_value is not nothing, then the semantic analysis has verified
        // that this lamda call requires exactly one parameter.

        let value = stack.resolve_variable(variable_id).unwrap();
        match &value {
            Value::FunctionLamda(target, capture) => {
                // this copy is needed because it borrows from `value` which borrows from `stack`
                let target = *target;
                let mut function_stack = capture.clone();
                match expr_value {
                    Value::Nothing => {},
                    anything_else => function_stack.add_argument(anything_else),
                }

                for expr in arguments {
                    let expr = expr.as_ref().expect("partial call of lamda is not allowed");
                    let arg_value = self.evaluate_value_expression(expr, stack)?;
                    function_stack.add_argument(arg_value);
                }

                match target {
                    GlobalFunctionTarget::Defined(id) => {
                        self.evaluate_fn_body(self.get_defined_fn(id), function_stack)
                    },
                    GlobalFunctionTarget::Native(id) => self.evaluate_native(id, function_stack),
                }
            },
            Value::InlineLamda(body, capture) => {
                // this clone is needed because it borrows from `value` which borrows from `stack`
                let body = body.clone();

                let mut function_stack = capture.clone();
                match expr_value {
                    Value::Nothing => {},
                    anything_else => function_stack.add_argument(anything_else),
                }

                for expr in arguments {
                    let expr = expr.as_ref().expect("partial call of lamda is not allowed");
                    let arg_value = self.evaluate_value_expression(expr, stack)?;
                    function_stack.add_argument(arg_value);
                }

                // the function body contains references to either the lamda parameters, or the capture
                self.evaluate_fn_body(&body, function_stack)
            },
            Value::AssignmentLamda(_target_id) => {
                panic!("AssignmentLamda in user code is not supported");
            },
            Value::IdentityLamda => Ok(expr_value),
            _ => panic!("LamdaCall call did not refer to a callable variable"),
        }
    }

    fn evaluate_native(
        &self,
        id: NativeFunctionId,
        function_stack: StackFrame,
    ) -> InterpResult<Value> {
        let target = self
            .internal_functions
            .get(&id)
            .expect("FunctionCall must be valid");

        target.call(function_stack.to_vec())
    }
}
