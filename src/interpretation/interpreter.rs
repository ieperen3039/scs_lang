use std::rc::Rc;

use crate::{
    built_in::functions::InternalFunctions,
    interpretation::execution_state::StackFrame,
    symbolization::ast::*,
};

use super::{execution_state::Variable, meta_structures::*};

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

    pub fn execute_by_name(&self, function: &str) -> InterpResult<String> {
        let to_execute = self.resolve_function(function)?;
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
                    Value::Nothing => break,
                    Value::Break => return Ok(expr_value),
                    _ => {},
                }

                expr_value = self.evaluate_function_expression(expr, expr_value, &mut stack)?;
            }

            debug_assert!(matches!(expr_value, Value::Nothing));
        }

        unreachable!("functions must have a return expression")
    }

    fn evaluate_value_expression(
        &self,
        expr: &ValueExpression,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            ValueExpression::Literal(Literal::Break) => Ok(Value::Break),
            ValueExpression::Literal(Literal::Number(lit)) => Ok(Value::Int(*lit)),
            ValueExpression::Literal(Literal::String(lit)) => {
                Ok(Value::String(lit.clone()))
            },
            ValueExpression::Literal(Literal::Boolean(lit)) => Ok(Value::Boolean(*lit)),
            ValueExpression::Tuple(tuple) => {
                let mut tuple_values = Vec::new();
                for ele in tuple {
                    tuple_values.push(self.evaluate_value_expression(ele, stack)?);
                }
                Ok(Value::Tuple(tuple_values))
            },
            ValueExpression::Variable(var) => Ok(stack
                .resolve_variable(*var)
                .cloned()
                // syntactially the variable exists in this scope.
                // If we do not have a value stored, then the variable is conditionally (not) assigned
                .unwrap_or(Value::Nothing)),
            ValueExpression::FunctionAsValue(fn_expr) => {
                self.evaluate_function_expression(fn_expr, Value::Nothing, stack)
            },
        }
    }

    fn evaluate_function_expression(
        &self,
        expr: &FunctionExpression,
        expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            FunctionExpression::FunctionCall(call) => match call.target {
                FunctionTarget::Defined(id) => self.evaluate_function_call(
                    id,
                    |function_stack| self.evaluate_fn_body(self.get_defined_fn(id), function_stack),
                    &call.arguments,
                    expr_value,
                    stack,
                ),
                FunctionTarget::Variable(id) => {
                    self.evaluate_lamda_call(id, &call.arguments, expr_value, stack)
                },
                FunctionTarget::Native(id) => self.evaluate_function_call(
                    id,
                    |function_stack| self.evaluate_native(id, function_stack),
                    &call.arguments,
                    expr_value,
                    stack,
                ),
            },
            FunctionExpression::Assignment(var) => {
                if matches!(expr_value, Value::Nothing) {
                    let assignment_impl = Rc::new(FunctionBody {
                        parameters: vec![0],
                        statements: vec![Statement {
                            base_element: ValueExpression::Literal(Literal::Break),
                            mutations: vec![],
                        }],
                        return_type: TypeRef::NoReturn,
                    });
                    return Ok(Value::InlineLamda(assignment_impl, StackFrame::new()));
                }

                stack.add_variable(Variable {
                    id: var.id,
                    value: expr_value,
                });

                if var.is_return {
                    Ok(Value::Break)
                } else {
                    Ok(Value::Nothing)
                }
            },
            FunctionExpression::Operator(op) => {
                let arg_value =
                    self.evaluate_function_expression(&op.arg, Value::Nothing, stack)?;

                let mut arguments = StackFrame::new();
                arguments.add_variable(Variable {
                    id: 1,
                    value: arg_value,
                });

                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::FunctionLamda(op.id, arguments));
                }

                arguments.add_variable(Variable {
                    id: 0,
                    value: expr_value,
                });

                let body = self.get_defined_fn(op.id);
                self.evaluate_fn_body(body, arguments)
            },
            FunctionExpression::Lamda(lamda) => {
                let mut lamda_arguments = StackFrame::new();
                let mut var_id = lamda.parameters.len();
                for ele in &lamda.capture {
                    let value = stack.resolve_variable(ele.id).unwrap().clone();
                    lamda_arguments.add_variable(Variable { id: var_id, value });
                    var_id += 1;
                }

                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::InlineLamda(lamda.body.clone(), lamda_arguments));
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

    fn evaluate_function_call<TargetFn>(
        &self,
        id: FunctionId,
        target: TargetFn,
        arguments: &Vec<Option<ValueExpression>>,
        mut expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value>
    where
        TargetFn: Fn(StackFrame) -> InterpResult<Value>,
    {
        let mut function_stack = StackFrame::new();
        let mut var_id = 0;
        let mut is_complete = true;
        for ele in arguments {
            let arg_value = match ele {
                Some(expr) => self.evaluate_value_expression(expr, stack)?,
                // if expr_value starts not-nothing, then the semantic analysis has verified
                // that this function call requires exactly one parameter.
                None => std::mem::replace(&mut expr_value, Value::Nothing),
            };
            if matches!(arg_value, Value::Nothing) {
                is_complete = false;
            } else {
                function_stack.add_variable(Variable {
                    id: var_id,
                    value: arg_value,
                });
            }
            // assume that all parameters have consectutive variable ids starting from 0
            var_id += 1;
        }

        if is_complete {
            target(function_stack)
        } else {
            Ok(Value::FunctionLamda(id, function_stack))
        }
    }

    fn evaluate_lamda_call(
        &self,
        variable_id: VariableId,
        arguments: &Vec<Option<ValueExpression>>,
        expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        // the function body contains references to either the lamda parameters, or the capture

        // if expr_value is not nothing, then the semantic analysis has verified
        // that this lamda call requires exactly one parameter.

        let value = stack.resolve_variable(variable_id).unwrap();
        match &value {
            Value::FunctionLamda(id, capture) => {
                // this copy is needed because it borrows from `value` which borrows from `stack`
                let id = *id;
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
                self.evaluate_fn_body(self.get_defined_fn(id), function_stack)
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
                self.evaluate_fn_body(&body, function_stack)
            },
            Value::IdentityLamda => Ok(expr_value),
            _ => panic!("LamdaCall call did not refer to a callable variable"),
        }
    }

    fn evaluate_native(&self, id: FunctionId, function_stack: StackFrame) -> InterpResult<Value> {
        let target = self
            .internal_functions
            .get(&id)
            .expect("FunctionCall must be valid");

        target.call(function_stack.to_vec())
    }

    // only used to resolve the function to call, all subsequent functions are already resolved in the parsing stage
    fn resolve_function(&self, full_name: &str) -> InterpResult<&FunctionBody> {
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
        let function_decl = target_scope.functions.get(function_name).ok_or_else(|| {
            InterpretationError::SymbolNotFound {
                kind: "function",
                symbol: Identifier::from(function_name),
            }
        })?;

        let to_execute = self.program.function_definitions.get(&function_decl.id);

        to_execute.ok_or_else(|| InterpretationError::SymbolNotFound {
            kind: "function",
            symbol: Identifier::from(function_name),
        })
    }
}
