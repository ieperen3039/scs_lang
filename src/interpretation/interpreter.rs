use crate::{
    built_in::functions::InternalFunctions,
    interpretation::stack_frame::StackFrame,
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
                    Value::Nothing => panic!("expr_value == {:?}", expr_value),
                    Value::Return(value) => return Ok(*value),
                    _ => {},
                }

                expr_value = self.evaluate_function_expression(expr, expr_value, &mut stack)?;
            }

            // now expr_value should be either a statement break, or a return value
            match expr_value {
                Value::Break => {},
                Value::Return(value) => return Ok(*value),
                _ => panic!("expr_value == {:?}", expr_value),
            }
        }

        unreachable!("functions must have a return expression")
    }

    fn evaluate_value_expression(
        &self,
        expr: &ValueExpression,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match &expr.inner {
            ValueExpressionInner::Literal(Literal::Number(lit)) => Ok(Value::Int(*lit)),
            ValueExpressionInner::Literal(Literal::String(lit)) => Ok(Value::String(lit.to_string())),
            ValueExpressionInner::Literal(Literal::Boolean(lit)) => Ok(Value::new_boolean(*lit)),
            ValueExpressionInner::Tuple(tuple) => {
                let mut tuple_values = Vec::new();
                for ele in tuple {
                    let expr_result = self.evaluate_value_expression(ele, stack);
                    match expr_result {
                        Ok(Value::Break) | Ok(Value::Nothing) => {
                            return Err(InterpretationError::ExpectedValueGotNothing)
                        },
                        Ok(value) => tuple_values.push(value),
                        Err(err) => return Err(err),
                    }
                }
                Ok(Value::Tuple(tuple_values))
            },
            ValueExpressionInner::Variable(var) => {
                let value = stack
                    .resolve_variable(*var)
                    .cloned()
                    // syntactially the variable exists in this scope.
                    // If we do not have a value stored, then the variable is conditionally (not) assigned
                    .unwrap_or(Value::Break);
                Ok(value)
            },
            ValueExpressionInner::FunctionAsValue(fn_expr) => {
                self.evaluate_function_expression(fn_expr, Value::Nothing, stack)
            },
            ValueExpressionInner::FunctionCall(fn_call) => self.evaluate_function_call(
                fn_call.target,
                &fn_call.arguments,
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
        match &expr.inner {
            FunctionExpressionInner::FunctionCall(call) => {
                // function call as function expression: we know this cannot evaluate to a value
                let function_stack = self.build_function_stack(
                    &call.arguments,
                    false,
                    expr_value,
                    stack,
                )?;

                Ok(Value::FunctionLamda(call.target, function_stack))
            },
            FunctionExpressionInner::Assignment(var) => {
                if matches!(expr_value, Value::Nothing) {
                    if var.is_return {
                        panic!("char_idx {}: assigning to return via lambda is not yet supported", expr.char_idx);
                    }

                    let promise = stack.promise_variable(var.id);
                    return Ok(Value::AssignmentLamda(promise));
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
            FunctionExpressionInner::Operator(op) => {
                let inner_function = self.evaluate_function_expression(&op.inner_expr, Value::Nothing, stack)?;

                let mut function_stack = StackFrame::new();
                function_stack.add_variable(Variable {
                    id: 1,
                    value: inner_function,
                });

                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::FunctionLamda(op.target.into(), function_stack));
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
            FunctionExpressionInner::Lamda(lamda) => {
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
                    return if lamda.parameters.is_empty() {
                        self.evaluate_fn_body(&lamda.body, lamda_arguments)
                    } else {
                        Ok(Value::InlineLamda(lamda.body.clone(), lamda_arguments))
                    }
                }

                assert_eq!(lamda.parameters.len(), 1);
                lamda_arguments.add_variable(Variable {
                    id: 0,
                    value: expr_value,
                });

                self.evaluate_fn_body(&lamda.body, lamda_arguments)
            },
            FunctionExpressionInner::Cast(_) => {
                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::IdentityLamda);
                }

                Ok(expr_value) /* nothing to do */
            },
        }
    }

    pub fn evaluate_function_call(
        &self,
        target: LocalFunctionTarget,
        arguments: &Vec<Option<ValueExpression>>,
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
                    true,
                    expr_value,
                    stack,
                )?;
                self.evaluate_fn_body(self.get_defined_fn(id), function_stack)
            },
            LocalFunctionTarget::Native(id) => {
                let function_stack = self.build_function_stack(
                    arguments,
                    true,
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
        full_evaluation: bool,
        mut expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<StackFrame> {
        let mut function_stack = StackFrame::new();
        let mut par_idx = 0;
        for ele in arguments {
            let arg_value = match ele {
                Some(expr) => self.evaluate_value_expression(expr, stack)?,
                // if expr_value starts not-nothing, then the semantic analysis has verified
                // that this function call requires exactly one parameter.
                None => std::mem::replace(&mut expr_value, Value::Nothing),
            };

            if matches!(arg_value, Value::Nothing) || matches!(arg_value, Value::Break) {
                if full_evaluation {
                    panic!(
                        "Function call must evaluate to a value, but parameter {par_idx} is {arg_value:?}"
                    );
                }
            } else {
                function_stack.add_argument(arg_value);
            }
            
            par_idx += 1;
        }

        if !full_evaluation && matches!(expr_value, Value::Nothing) {
            panic!(
                "Function call evaluated to a value, but should have evaluated to a function"
            );
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
                    let expr = expr.as_ref().expect("partial call of lambda is not allowed");
                    let arg_value = self.evaluate_value_expression(expr, stack)?;
                    function_stack.add_argument(arg_value);
                }

                match target {
                    LocalFunctionTarget::Defined(id) => self
                        .evaluate_fn_body(self.get_defined_fn(id), function_stack)
                        .map_err(|err| {
                            err.while_parsing(
                                self.get_declaration(GlobalFunctionTarget::Defined(id)),
                            )
                        }),
                    LocalFunctionTarget::Native(id) => self.evaluate_native(id, function_stack),
                    LocalFunctionTarget::Local(next_variable) => self.evaluate_lamda_call(next_variable, arguments, Value::Nothing, &mut function_stack),
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
            Value::AssignmentLamda(promise) => {
                // this clone is needed because it borrows from `value` which borrows from `stack`
                let promise = promise.clone();

                let value = match expr_value {
                    Value::Nothing => {
                        let expr = arguments[0]
                            .as_ref()
                            .expect("partial call of lamda is not allowed");
                        self.evaluate_value_expression(expr, stack)?
                    },
                    anything_else => anything_else,
                };

                match promise.set(value) {
                    Ok(_) => Ok(Value::Break),
                    Err(_) => Err(InterpretationError::DoubleAssignment()),
                }
            },
            Value::IdentityLamda => Ok(expr_value),
            _ => panic!("lambda call did not refer to a callable variable"),
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

        target
            .call(function_stack.to_vec(), &self)
            .map_err(|err| err.while_parsing(&target.get_declaration()))
    }

    fn get_declaration(&self, id: GlobalFunctionTarget) -> &FunctionDeclaration {
        self.program
            .namespaces
            .find_fn(id)
            .expect("function id not found")
    }
}
