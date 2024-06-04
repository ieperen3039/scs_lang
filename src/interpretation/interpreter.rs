use std::rc::Rc;

use crate::{
    interpretation::execution_state::StackFrame,
    symbolization::ast::{self, FunctionBody, Identifier, Program, Statement, VariableDeclaration},
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
        let result = self.evaluate_fn_body(to_execute, StackFrame::new())?;

        return Ok(format!("{:?}", result));
    }

    pub fn get_fn(&self, id: ast::FunctionId) -> &FunctionBody {
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
        expr: &ast::ValueExpression,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            ast::ValueExpression::Literal(ast::Literal::Break) => Ok(Value::Break),
            ast::ValueExpression::Literal(ast::Literal::Number(lit)) => Ok(Value::Int(*lit)),
            ast::ValueExpression::Literal(ast::Literal::String(lit)) => {
                Ok(Value::String(lit.clone()))
            },
            ast::ValueExpression::Literal(ast::Literal::Boolean(lit)) => Ok(Value::Boolean(*lit)),
            ast::ValueExpression::Tuple(tuple) => {
                let mut tuple_values = Vec::new();
                for ele in tuple {
                    tuple_values.push(self.evaluate_value_expression(ele, stack)?);
                }
                Ok(Value::Tuple(tuple_values))
            },
            ast::ValueExpression::Variable(var) => Ok(stack
                .resolve_variable(*var)
                .cloned()
                // syntactially the variable exists in this scope.
                // If we do not have a value stored, then the variable is conditionally (not) assigned
                .unwrap_or(Value::Nothing)),
            ast::ValueExpression::FunctionAsValue(fn_expr) => {
                self.evaluate_function_expression(fn_expr, Value::Nothing, stack)
            },
        }
    }

    fn evaluate_function_expression(
        &self,
        expr: &ast::FunctionExpression,
        mut expr_value: Value,
        stack: &mut StackFrame,
    ) -> InterpResult<Value> {
        match expr {
            ast::FunctionExpression::FunctionCall(call) => {
                let mut function_stack = StackFrame::new();
                let mut var_id = 0;
                let mut is_complete = true;
                for ele in &call.arguments {
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
                    let body = self.get_fn(call.id);
                    self.evaluate_fn_body(body, function_stack)
                } else {
                    Ok(Value::FunctionLamda(call.id, function_stack))
                }
            },
            ast::FunctionExpression::Assignment(var) => {
                if matches!(expr_value, Value::Nothing) {
                    let assignment_impl = Rc::new(FunctionBody {
                        parameters: vec![0],
                        statements: vec![Statement {
                            base_element: ast::ValueExpression::Literal(ast::Literal::Break),
                            mutations: vec![],
                        }],
                        return_type: ast::TypeRef::NoReturn,
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
            ast::FunctionExpression::Operator(op) => {
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

                let body = self.get_fn(op.id);
                self.evaluate_fn_body(body, arguments)
            },
            ast::FunctionExpression::Lamda(lamda) => {
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
            ast::FunctionExpression::LamdaCall(variable) => {
                // the function body contains references to either the lamda parameters, or the capture

                // if expr_value is not nothing, then the semantic analysis has verified
                // that this lamda call requires exactly one parameter.

                let value = stack.resolve_variable(variable.id).unwrap();
                match &value {
                    Value::FunctionLamda(id, capture) => {
                        // this copy is needed because it borrows from `value` which borrows from `stack`
                        let id = *id;
                        let mut function_stack = capture.clone();
                        match expr_value {
                            Value::Nothing => {},
                            anything_else => function_stack.add_argument(anything_else),
                        }

                        for expr in &variable.arguments {
                            let arg_value = self.evaluate_value_expression(expr, stack)?;
                            function_stack.add_argument(arg_value);
                        }
                        self.evaluate_fn_body(self.get_fn(id), function_stack)
                    },
                    Value::InlineLamda(body, capture) => {
                        // this clone is needed because it borrows from `value` which borrows from `stack`
                        let body = body.clone();
                        let mut function_stack = capture.clone();
                        match expr_value {
                            Value::Nothing => {},
                            anything_else => function_stack.add_argument(anything_else),
                        }

                        for expr in &variable.arguments {
                            let arg_value = self.evaluate_value_expression(expr, stack)?;
                            function_stack.add_argument(arg_value);
                        }
                        self.evaluate_fn_body(&body, function_stack)
                    },
                    Value::IdentityLamda => Ok(expr_value),
                    _ => panic!("LamdaCall call did not refer to a callable variable"),
                }
            },
            ast::FunctionExpression::Cast(_) => {
                if matches!(expr_value, Value::Nothing) {
                    return Ok(Value::IdentityLamda);
                }

                Ok(expr_value) /* nothing to do */
            },
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
