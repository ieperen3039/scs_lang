use std::{collections::HashMap, rc::Rc};

use crate::parsing::rule_nodes::RuleNode;

use super::{
    ast::*,
    function_collector::FunctionCollector,
    semantic_result::{SemanticError, SemanticResult},
    type_collector::TypeCollector,
    type_resolver,
    variable_storage::VarStorage,
};

pub struct FunctionParser<'ns, 'fc> {
    pub root_namespace: &'ns Namespace,
    pub function_collector: &'fc mut FunctionCollector,
}

struct ArgumentsResult {
    pub arguments: Vec<Option<ValueExpression>>,
    pub remaining_parameters: Vec<Parameter>,
}

impl FunctionParser<'_, '_> {
    pub fn new<'ns, 'fc>(
        root_namespace: &'ns Namespace,
        function_collector: &'fc mut FunctionCollector,
    ) -> FunctionParser<'ns, 'fc> {
        FunctionParser {
            root_namespace,
            function_collector,
        }
    }

    // statement, { statement_separator, statement }, [ statement_separator ]
    pub fn read_statements(
        &self,
        nodes: &[RuleNode],
        this_scope: &Namespace,
        mut variables: &mut VarStorage,
    ) -> SemanticResult<FunctionBody> {
        assert!(!nodes.is_empty());
        debug_assert_eq!(nodes.first().map(|n| n.rule_name), Some("statement"));

        let parameters = variables.get_var_ids();
        let mut statements = Vec::new();

        for pair in nodes.chunks(2) {
            let statement_node = &pair[0];
            if statement_node.rule_name != "statement" {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(statement_node.rule_name),
                }
                .while_parsing(statement_node));
            };

            let mut statement = self.read_statement(statement_node, this_scope, &mut variables)?;

            if pair.len() == 2 {
                let separator_node = &pair[1];
                if separator_node.rule_name != "statement_separator" {
                    return Err(SemanticError::UnexpectedNode {
                        found: Identifier::from(separator_node.rule_name),
                    }
                    .while_parsing(separator_node));
                };
            } else {
                // this is an implicit return
                let last_expression_type = match statement.mutations.last() {
                    Some(last_mutation) => {
                        if let FunctionExpressionInner::Assignment(_) = last_mutation.inner {
                            return Err(SemanticError::BrokenControl(
                                "Function ended with an assignment, but no semicolon. \
                            Assignments do not return a result, but the missing semicolon implies that we wnat to return this result",
                            ).while_parsing(statement_node.sub_rules.last().unwrap_or(statement_node)));
                        }
                        last_mutation.get_return_type()
                    },
                    None => statement.base_element.get_type(variables),
                };
                // TODO check whether last_expression_type is TypeRef::NoReturn

                let return_var = match variables.get_return_var() {
                    Some(var) => {
                        if var.var_type != last_expression_type {
                            return Err(SemanticError::TypeMismatchError {
                                expected: var.var_type.clone(),
                                found: last_expression_type,
                            }
                            .while_parsing(statement_node));
                        }
                        var
                    },
                    None => variables.insert_return(last_expression_type),
                };

                statement.mutations.push(FunctionExpression {
                    inner: FunctionExpressionInner::Assignment(return_var),
                    char_idx: statement.base_element.char_idx,
                })
            }

            statements.push(statement);
        }

        let return_type = variables
            .get_return_var()
            .expect("no return var found")
            .var_type
            .clone();

        Ok(FunctionBody {
            statements,
            return_type,
            parameters,
        })
    }

    pub fn read_function_body(
        &self,
        function_declaration: &FunctionDeclaration,
        node: &RuleNode,
        this_scope: &Namespace,
    ) -> SemanticResult<FunctionBody> {
        debug_assert_eq!(node.rule_name, "function_body");
        let mut variables = VarStorage::new();

        for param in &function_declaration.parameters {
            variables.insert_from_param(param)?;
        }

        let function_body = self.read_statements(&node.sub_rules, this_scope, &mut variables)?;

        if &function_body.return_type != &function_declaration.return_type {
            return Err(SemanticError::TypeMismatchError {
                expected: function_declaration.return_type.clone(),
                found: function_body.return_type.clone(),
            }
            .while_parsing(node.sub_rules.last().unwrap_or(node)));
        }

        for unused in variables.get_unused_vars() {
            println!(
                "WARNING: parameter {} of function {} not used",
                unused.name, function_declaration.name
            )
        }

        Ok(function_body)
    }

    // statement = value_expression, { function_expression };
    fn read_statement(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<Statement> {
        debug_assert_eq!(node.rule_name, "statement");

        let expression_node = node.expect_node("value_expression")?;

        let expression =
            self.read_value_expression(expression_node, this_scope, variables, None)?;

        let mut expression_type = expression.get_type(variables);
        let mut mutations = Vec::new();
        let mut previous_expr_node = expression_node;

        for mutator_node in node.find_nodes("function_expression") {
            if expression_type == TypeRef::Break {
                return Err(SemanticError::BrokenControl(
                    "Expression does not return, but the statement continues. Did you forget a semicolon?",
                )
                .while_parsing(previous_expr_node));
            }

            let mutator = self.read_function_expression(
                mutator_node,
                &vec![expression_type],
                this_scope,
                variables,
            )?;

            // verify that mutators always have exactly one parameter
            if let TypeRef::Function(function_type) = mutator.get_type() {
                if function_type.parameters.len() != 1 {
                    return Err(SemanticError::InvalidNumerOfParameters {
                        num_target: 1,
                        num_this: function_type.parameters.len(),
                    }
                    .while_parsing(mutator_node));
                }
            }

            expression_type = mutator.get_return_type();
            previous_expr_node = mutator_node;
            mutations.push(mutator);
        }

        Ok(Statement {
            base_element: expression,
            mutations,
        })
    }

    fn verify_parameters(
        argument_types: &Vec<TypeRef>,
        parameters: &Vec<TypeRef>,
        node: &RuleNode,
    ) -> SemanticResult<()> {
        let mut arg_iter = argument_types.iter();
        for par in parameters {
            if let Some(found_type) = arg_iter.next() {
                FunctionParser::verify_value_type(found_type, Some(par), node)?;
            } else {
                return Err(SemanticError::InvalidNumerOfParameters {
                    num_target: argument_types.len(),
                    num_this: parameters.len(),
                });
            }
        }
        Ok(())
    }

    fn verify_value_type(
        found_type: &TypeRef,
        expected_type: Option<&TypeRef>,
        node: &RuleNode,
    ) -> SemanticResult<()> {
        assert!(
            !matches!(found_type, TypeRef::GenericName(_)),
            "Generic types cannot be instantiated"
        );

        if let Some(expected_type) = expected_type {
            if found_type == expected_type {
                return Ok(());
            }

            if let TypeRef::GenericName(_) = expected_type {
                // this is checked at a higher level
                return Ok(());
            }

            if let TypeRef::Function(function_type) = found_type {
                if function_type.parameters.is_empty()
                    && function_type.return_type.as_ref() == expected_type
                {
                    return Ok(());
                }
            }

            return Err(SemanticError::TypeMismatchError {
                expected: expected_type.clone(),
                found: found_type.clone(),
            }
            .while_parsing(node));
        }

        return Ok(());
    }

    // argument_expression     = function_call | variable_name | tuple_construction | _literal | implicit_par_lamda | explicit_par_lamda | mutator_cast | mutator_assign;
    fn read_argument_expression(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
        target_type: &TypeRef,
    ) -> SemanticResult<ValueExpression> {
        // because argument_expression is a subset of the union of function_expression and
        // value_expression, we just forward to one of those
        match target_type {
            TypeRef::Function(fn_type) => {
                let fn_expr = self.read_function_expression(
                    node,
                    &fn_type.parameters,
                    this_scope,
                    variables,
                )?;
                // only need to validate the return type
                Self::verify_value_type(
                    &fn_expr.get_return_type(),
                    Some(&fn_type.return_type),
                    node,
                )?;
                Ok(ValueExpression {
                    inner: ValueExpressionInner::FunctionAsValue(fn_expr),
                    char_idx: node.first_char(),
                })
            },
            _ => self.read_value_expression(node, this_scope, variables, Some(target_type)),
        }
    }

    // value_expression = function_call | variable_name | tuple_construction | _literal;
    fn read_value_expression(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
        target_type: Option<&TypeRef>,
    ) -> SemanticResult<ValueExpression> {
        debug_assert!(
            node.rule_name == "value_expression" || node.rule_name == "argument_expression"
        );

        let sub_node = node
            .sub_rules
            .first()
            .expect("value_expression must have subnodes");

        let expr = match sub_node.rule_name {
            "variable_name" => {
                let var_name = sub_node.as_identifier();
                let variable = variables.use_var_by_name(&var_name);
                if let Some(variable) = variable {
                    Self::verify_value_type(&variable.var_type, target_type, sub_node)?;
                    ValueExpressionInner::Variable(variable.id)
                } else if let Some(constant) = this_scope.constants.get(&var_name) {
                    Self::verify_value_type(&constant.get_type(variables), target_type, sub_node)?;
                    constant.inner.clone()
                } else {
                    return Err(SemanticError::SymbolNotFound {
                        kind: "variable",
                        symbol: var_name,
                    }
                    .while_parsing(sub_node));
                }
            },
            "tuple_construction" => {
                let mut tuple_elements = Vec::new();
                for tuple_elt_node in &sub_node.sub_rules {
                    let value =
                        self.read_value_expression(tuple_elt_node, this_scope, variables, None)?;
                    tuple_elements.push(value);
                }
                ValueExpressionInner::Tuple(tuple_elements)
            },
            "string_literal" => {
                Self::verify_value_type(&TypeRef::STRING, target_type, sub_node)?;
                let string_value = extract_string(sub_node);
                ValueExpressionInner::Literal(Literal::String(string_value))
            },
            "integer_literal" => {
                Self::verify_value_type(&TypeRef::INT, target_type, sub_node)?;
                let as_string = sub_node.tokens_as_string();
                let parse_result = as_string.parse::<i32>();

                match parse_result {
                    Ok(integer_value) => {
                        ValueExpressionInner::Literal(Literal::Number(integer_value))
                    },
                    Err(err) => {
                        return Err(SemanticError::InternalError(format!(
                            "Could not parse integer literal \"{as_string}\": {err}"
                        ))
                        .while_parsing(sub_node))
                    },
                }
            },
            "dollar_string_literal" => {
                Self::verify_value_type(&TypeRef::STRING, target_type, sub_node)?;
                unimplemented!("{}", sub_node.rule_name)
            },
            "raw_string_literal" => {
                Self::verify_value_type(&TypeRef::STRING, target_type, sub_node)?;
                unimplemented!("{}", sub_node.rule_name)
            },
            "function_call" => {
                let function_call = self.read_function_call(sub_node, this_scope, variables)?;

                let found_type = &function_call.value_type;
                if !found_type.parameters.is_empty() {
                    return Err(SemanticError::ExpectedValueGotFunction {
                        found_type: found_type.clone(),
                    });
                }

                if let Some(expected_type) = target_type {
                    // require that the function call is
                    if found_type.return_type.as_ref() != expected_type {
                        return Err(SemanticError::TypeMismatchError {
                            expected: expected_type.clone(),
                            found: TypeRef::Function(found_type.clone()),
                        }
                        .while_parsing(sub_node));
                    }
                }

                ValueExpressionInner::FunctionCall(function_call)
            },
            "function_as_value" => {
                let expr_node = sub_node.expect_node("function_expression")?;

                if let Some(TypeRef::Function(expected_type)) = target_type {
                    let fn_expr = self.read_function_expression(
                        expr_node,
                        &expected_type.parameters,
                        this_scope,
                        variables,
                    )?;
                    // only need to validate the return type
                    Self::verify_value_type(
                        &fn_expr.get_return_type(),
                        Some(&expected_type.return_type),
                        sub_node,
                    )?;
                    ValueExpressionInner::FunctionAsValue(fn_expr)
                } else {
                    // we allow function_as_value to return a value, rather than a fn-type
                    let fn_expr = self.read_function_expression(
                        expr_node,
                        &Vec::new(),
                        this_scope,
                        variables,
                    )?;

                    // fn_expr.get_type() will return a non-function type if no parameters are left to evaluate
                    Self::verify_value_type(&fn_expr.get_type(), target_type, sub_node)?;
                    ValueExpressionInner::FunctionAsValue(fn_expr)
                }
            },
            // only a function call and a zero-parameter explicit lamda can result into a value
            "explicit_par_lamda" => {
                let fn_expr = self.read_explicit_parameter_lamda(
                    sub_node,
                    variables,
                    this_scope,
                    &Vec::new(),
                )?;
                // fn_expr.get_type() will return a non-function type if no parameters are left to evaluate
                Self::verify_value_type(&fn_expr.get_type(), target_type, sub_node)?;
                ValueExpressionInner::FunctionAsValue(FunctionExpression {
                    inner: fn_expr,
                    char_idx: sub_node.first_char(),
                })
            },
            unknown => {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(unknown),
                }
                .while_parsing(node))
            },
        };

        Ok(ValueExpression {
            inner: expr,
            char_idx: sub_node.first_char(),
        })
    }

    // function_expression = function_call | implicit_par_lamda | explicit_par_lamda | mutator_cast | mutator_assign | operator_expression;
    fn read_function_expression(
        &self,
        node: &RuleNode,
        // what kind of arguments do we expect this function expression to accept?
        argument_types: &Vec<TypeRef>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<FunctionExpression> {
        debug_assert!(
            node.rule_name == "function_expression" || node.rule_name == "argument_expression"
        );

        let sub_node = node
            .sub_rules
            .first()
            .expect("function_expression must have subnodes");

        let char_idx = sub_node.first_char();

        let expr = match sub_node.rule_name {
            "function_call" => {
                // we first check whether this refers to a variable
                if sub_node.find_nodes("namespace_name").is_empty() {
                    let function_name_node = sub_node.expect_node("function_name")?;
                    let fn_variable =
                        variables.use_var_by_name(&function_name_node.tokens_as_string());

                    if let Some(found_variable) = fn_variable {
                        // the function_name is actually the variable name
                        let lamda = self.read_lamda_call(
                            sub_node,
                            found_variable,
                            this_scope,
                            variables,
                            argument_types,
                        )?;

                        return Ok(FunctionExpression {
                            inner: FunctionExpressionInner::FunctionCall(lamda),
                            char_idx,
                        });
                    }
                }

                let function_call = self.read_function_call(sub_node, this_scope, variables)?;
                let parameters = &function_call.value_type.parameters;
                Self::verify_parameters(argument_types, parameters, sub_node)?;

                FunctionExpressionInner::FunctionCall(function_call)
            },
            "implicit_par_lamda" => {
                if argument_types.len() != 1 {
                    return Err(SemanticError::InvalidNumerOfParameters {
                        num_target: argument_types.len(),
                        num_this: 1,
                    }
                    .while_parsing(sub_node));
                }
                self.read_implicit_parameter_lamda(
                    sub_node,
                    variables,
                    this_scope,
                    argument_types[0].clone(),
                )?
            },
            "explicit_par_lamda" => {
                self.read_explicit_parameter_lamda(sub_node, variables, this_scope, argument_types)?
            },
            "mutator_assign" => {
                if argument_types.len() != 1 {
                    return Err(SemanticError::InvalidNumerOfParameters {
                        num_target: argument_types.len(),
                        num_this: 1,
                    }
                    .while_parsing(sub_node));
                }

                self.read_assignment(sub_node, &argument_types[0], variables)?
            },
            "mutator_cast" => {
                let type_node = sub_node.expect_node("type_ref")?;
                let type_ref = TypeCollector::read_type_ref(type_node)?;
                FunctionExpressionInner::Cast(type_ref)
            },
            "operator_expression" => {
                if argument_types.len() != 1 {
                    return Err(SemanticError::InvalidNumerOfParameters {
                        num_target: argument_types.len(),
                        num_this: 1,
                    }
                    .while_parsing(sub_node));
                }
                self.read_operator(sub_node, &argument_types[0], this_scope, variables)?
            },
            unknown => {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(unknown),
                }
                .while_parsing(node))
            },
        };

        Ok(FunctionExpression {
            inner: expr,
            char_idx,
        })
    }

    // operator_expression = operator, function_expression;
    // operator = ? OPERATOR ?;
    fn read_operator(
        &self,
        sub_node: &RuleNode,
        argument_type: &TypeRef,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> Result<FunctionExpressionInner, SemanticError> {
        debug_assert_eq!(sub_node.rule_name, "operator_expression");

        let operator_symbol_node = sub_node.expect_node("operator")?;
        let operator_str = operator_symbol_node.as_identifier();
        let operator_fn_decl = self
            .root_namespace
            .functions
            .get(&operator_str)
            .ok_or_else(|| {
                SemanticError::SymbolNotFound {
                    kind: "operator",
                    symbol: operator_str,
                }
                .while_parsing(operator_symbol_node)
            })?;

        // operators must have exactly 2 parameters:
        // 0: the generic input value
        // 1: the function with which to transform the input
        debug_assert_eq!(operator_fn_decl.parameters.len(), 2);

        let first_par_type = operator_fn_decl.parameters[0].to_type();
        if let TypeRef::GenericName(_) = first_par_type {
            // always accepted, no special handling necessary
        } else if argument_type != first_par_type {
            return Err(SemanticError::TypeMismatchError {
                expected: argument_type.clone(),
                found: first_par_type.clone(),
            }
            .while_parsing(operator_symbol_node));
        }

        let inner_expression_node = sub_node.expect_node("function_expression")?;
        let operator_inner_type = operator_fn_decl.parameters[1].to_type();
        let inner_expression = if let TypeRef::Function(function_type) = operator_inner_type {
            self.read_function_expression(
                inner_expression_node,
                &function_type.parameters,
                this_scope,
                variables,
            )?
        } else {
            return Err(SemanticError::ExpectedFunctionGotValue {
                found_type: operator_inner_type.clone(),
            }
            .while_parsing(operator_symbol_node));
        };

        let found_inner_type = inner_expression.get_type();
        if &found_inner_type != operator_inner_type {
            return Err(SemanticError::TypeMismatchError {
                expected: operator_inner_type.clone(),
                found: found_inner_type,
            }
            .while_parsing(inner_expression_node));
        }

        Ok(FunctionExpressionInner::Operator(Operator {
            target: operator_fn_decl.id,
            inner_expr: Box::from(inner_expression),
            return_type: operator_fn_decl.return_type.clone(),
            inner_type: found_inner_type,
        }))
    }

    // mutator_assign = [ type_ref ], variable_name;
    fn read_assignment(
        &self,
        node: &RuleNode,
        value_type: &TypeRef,
        variables: &mut VarStorage,
    ) -> Result<FunctionExpressionInner, SemanticError> {
        let variable_name_node = node.expect_node("variable_name")?;
        let variable_name = variable_name_node.as_identifier();

        let type_ref_node = node.find_node("type_ref");
        if let Some(type_ref_node) = type_ref_node {
            let declared_type = TypeCollector::read_type_ref(type_ref_node)?;
            if &declared_type != value_type {
                return Err(SemanticError::TypeMismatchError {
                    expected: declared_type,
                    found: value_type.clone(),
                }
                .while_parsing(type_ref_node));
            }
        }

        let var_decl = if &variable_name[..] == "return" {
            variables.insert_return(value_type.clone())
        } else {
            variables.insert(variable_name, value_type.clone())?
        };

        Ok(FunctionExpressionInner::Assignment(var_decl))
    }

    // explicit_par_lamda     = [ untyped_parameter_list ], function_body;
    fn read_explicit_parameter_lamda(
        &self,
        node: &RuleNode,
        outer_variables: &mut VarStorage,
        this_scope: &Namespace,
        argument_types: &Vec<TypeRef>,
    ) -> SemanticResult<FunctionExpressionInner> {
        debug_assert!(node.rule_name == "explicit_par_lamda");

        let parameter_node = node.find_node("untyped_parameter_list");
        let function_node = node.expect_node("function_body")?;

        let untyped_parameter_list = match parameter_node {
            Some(p_node) => self
                .function_collector
                .read_untyped_parameter_list(p_node)?,
            None => Vec::new(),
        };

        if argument_types.len() != untyped_parameter_list.len() {
            return Err(SemanticError::InvalidNumerOfParameters {
                num_target: argument_types.len(),
                num_this: untyped_parameter_list.len(),
            }
            .while_parsing(parameter_node.unwrap_or(node)));
        }

        let mut inner_variables = VarStorage::from(outer_variables);

        for (t, n) in argument_types.iter().zip(untyped_parameter_list.iter()) {
            inner_variables.insert(n.to_owned(), t.to_owned())?;
        }

        let function_body =
            self.read_statements(&function_node.sub_rules, this_scope, &mut inner_variables)?;

        let mut capture = Vec::new();
        for var in inner_variables.get_used_vars() {
            if outer_variables.contains(&var) {
                capture.push(var)
            }
        }

        Ok(FunctionExpressionInner::Lamda(Lamda {
            parameters: argument_types.clone(),
            body: Rc::from(function_body),
            capture,
        }))
    }

    // the first statement of a implicit parameter lamdas starts with a function expression
    // the body is therefore not just a function body
    // implicit_par_lamda = function_expression, { function_expression }, { statement_separator, statement }, [ statement_separator ];
    fn read_implicit_parameter_lamda(
        &self,
        node: &RuleNode,
        outer_variables: &mut VarStorage,
        this_scope: &Namespace,
        argument_type: TypeRef,
    ) -> SemanticResult<FunctionExpressionInner> {
        debug_assert!(node.rule_name == "implicit_par_lamda");

        let mut inner_variables = VarStorage::from(outer_variables);

        let implicit_par_name = Identifier::from("__implicit");
        let implicit_par =
            inner_variables.insert(implicit_par_name.clone(), argument_type.clone())?;

        // handle the first statement separately
        let mut expression_type = argument_type.clone();
        let mut mutations = Vec::new();

        for mutator_node in node.find_nodes("function_expression") {
            let mutator = self.read_function_expression(
                mutator_node,
                &vec![expression_type],
                this_scope,
                &mut inner_variables,
            )?;

            expression_type = mutator.get_return_type();
            mutations.push(mutator);
        }

        let initial_statement = Statement {
            base_element: ValueExpression {
                inner: ValueExpressionInner::Variable(implicit_par.id),
                char_idx: node.first_char(),
            },
            mutations,
        };

        let num_elements_left = node
            .sub_rules
            .iter()
            .position(|r| r.rule_name == "statement");

        let function_body = if let Some(num_elements_left) = num_elements_left {
            let mut function_body = self.read_statements(
                &node.sub_rules[num_elements_left..],
                this_scope,
                &mut inner_variables,
            )?;

            // now add the first statement
            function_body.statements.insert(0, initial_statement);
            function_body
        } else {
            let parameters = inner_variables.get_var_ids();
            let return_var = inner_variables.get_return_var().ok_or_else(|| {
                SemanticError::SymbolNotFound {
                    kind: "implicit variable",
                    symbol: Identifier::from("return"),
                }
                .while_parsing(node)
            })?;

            FunctionBody {
                parameters,
                statements: vec![initial_statement],
                return_type: return_var.var_type.clone(),
            }
        };

        let mut capture = Vec::new();
        for var in inner_variables.get_used_vars() {
            if outer_variables.contains(&var) {
                capture.push(var)
            }
        }

        Ok(FunctionExpressionInner::Lamda(Lamda {
            parameters: vec![argument_type],
            body: Rc::from(function_body),
            capture,
        }))
    }

    // function_call = { namespace_name }, function_name, [ _argument_list ];
    // _argument_list = argument, { argument };
    fn read_function_call(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<FunctionCall> {
        assert_eq!(node.rule_name, "function_call");
        let scope: Vec<Identifier> = node
            .find_nodes("namespace_name")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        let function_name_node = node.expect_node("function_name")?;
        let function_name = function_name_node.as_identifier();

        let function_decl = type_resolver::resolve_function_name(
            function_name,
            &scope,
            &self.root_namespace,
            this_scope,
        )?;

        let argument_nodes = node.find_nodes("argument");
        let arguments_result =
            self.read_arguments(argument_nodes, &function_decl, this_scope, variables)?;

        let arguments = arguments_result.arguments;
        let parameters = arguments_result
            .remaining_parameters
            .into_iter()
            .filter(|p| !p.is_optional)
            .map(|p| p.par_type)
            .collect();

        let generic_arguments = Vec::new();

        Ok(FunctionCall {
            target: function_decl.id.into(),
            arguments,
            generic_arguments,
            value_type: FunctionType {
                parameters,
                return_type: Box::new(function_decl.return_type),
            },
        })
    }

    fn read_lamda_call(
        &self,
        node: &RuleNode,
        fn_var: Rc<VariableDeclaration>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
        argument_types: &Vec<TypeRef>,
    ) -> SemanticResult<FunctionCall> {
        let argument_nodes = node.find_nodes("argument");
        let mut arguments = Vec::new();
        for (arg_node, exp_type) in argument_nodes.iter().zip(argument_types) {
            let arg_impl_node = arg_node.expect_node("unnamed_argument")?;
            let arg_value =
                self.read_value_expression(arg_impl_node, this_scope, variables, Some(exp_type))?;
            arguments.push(Some(arg_value))
        }

        if let TypeRef::Function(fn_type) = &fn_var.var_type {
            Self::verify_parameters(argument_types, &fn_type.parameters, node)?;

            Ok(FunctionCall {
                target: LocalFunctionTarget::Local(fn_var.id),
                value_type: fn_type.clone(),
                generic_arguments: Vec::new(),
                arguments,
            })
        } else {
            Err(SemanticError::TypeMismatchError {
                expected: TypeRef::Function(FunctionType {
                    parameters: argument_types.clone(),
                    return_type: Box::new(TypeRef::Void),
                }),
                found: fn_var.var_type.clone(),
            }
            .while_parsing(node))
        }
    }

    // if successful, returns the arguments, where the indices map to function_decl.parameters
    fn read_arguments(
        &self,
        argument_nodes: Vec<&RuleNode>,
        function_decl: &FunctionDeclaration,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<ArgumentsResult> {
        if argument_nodes.is_empty() {
            return Ok(ArgumentsResult {
                arguments: Vec::new(),
                remaining_parameters: function_decl.parameters.clone(),
            });
        }

        let mut generic_replacements: HashMap<Identifier, TypeRef> = HashMap::new();

        let mut remaining_parameters: Vec<Parameter> = function_decl.parameters.clone();
        // maps the current parameters to the corresponding indices in the original parameters vec
        let mut indices: Vec<usize> = (0..function_decl.parameters.len()).collect();

        let mut arguments = Vec::new();
        arguments.resize(remaining_parameters.len(), None);

        for arg_node in argument_nodes.into_iter().rev() {
            let (par_idx, expr) = self.read_argument(
                &function_decl.name,
                arg_node,
                &remaining_parameters,
                this_scope,
                variables,
            )?;
            let target_par = remaining_parameters.remove(par_idx);
            let target_idx = indices.remove(par_idx);

            if let TypeRef::GenericName(name) = target_par.par_type {
                let new_type = expr.get_type(variables);

                if let Some(existing_type) = generic_replacements.get(&name) {
                    if existing_type != &new_type {
                        return Err(SemanticError::AmbiguousGenericType {
                            generic_name: name,
                            first_type: existing_type.clone(),
                            second_type: new_type,
                        });
                    }
                }

                generic_replacements.insert(name, new_type);
            }

            arguments[target_idx] = Some(expr);
        }
        Ok(ArgumentsResult {
            arguments,
            remaining_parameters,
        })
    }

    // argument                = named_argument | unnamed_argument | flag_argument;
    // unnamed_argument        = function_call | tuple_construction | _literal | function_as_value;
    // flag_argument           = ? IDENTIFIER ?;
    // named_argument          = identifier, "=", _expression;
    // returns (index of parameter in remaining_parameters where this arg binds to, Expression of the argument)
    fn read_argument(
        &self,
        function_name: &Identifier,
        node: &RuleNode,
        remaining_parameters: &Vec<Parameter>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<(usize, ValueExpression)> {
        debug_assert_eq!(node.rule_name, "argument");

        let sub_node = node
            .sub_rules
            .first()
            .expect("function_expression must have subnodes");

        match sub_node.rule_name {
            "named_argument" => {
                let arg_name_node = &sub_node.expect_node("identifier")?;
                let arg_name = arg_name_node.as_identifier();
                let target_parameter_idx = remaining_parameters
                    .iter()
                    .position(|p| p.matches(&arg_name))
                    .ok_or_else(|| {
                        SemanticError::ArgumentNotFound {
                            arg: arg_name,
                            function: function_name.clone(),
                        }
                        .while_parsing(sub_node)
                    })?;

                let expression_node = sub_node.expect_node("argument_expression")?;
                let expected_type = &remaining_parameters[target_parameter_idx].par_type;
                let arg_value = self.read_argument_expression(
                    expression_node,
                    this_scope,
                    variables,
                    expected_type,
                )?;

                Ok((target_parameter_idx, arg_value))
            },
            "unnamed_argument" => {
                let first_non_optional = remaining_parameters
                    .iter()
                    .enumerate()
                    .rev()
                    .find(|(_, p)| !p.is_optional)
                    .map(|(idx, _)| idx)
                    .ok_or_else(|| {
                        SemanticError::AmbiguousUnnamedArgument {
                            arg: sub_node.as_identifier(),
                            function: function_name.clone(),
                        }
                        .while_parsing(sub_node)
                    })?;

                let expression_node = sub_node.expect_node("argument_expression")?;
                let expected_type = &remaining_parameters[first_non_optional].par_type;
                let arg_value = self.read_argument_expression(
                    expression_node,
                    this_scope,
                    variables,
                    expected_type,
                )?;

                Ok((first_non_optional, arg_value))
            },
            "flag_argument" => {
                let flag_name = sub_node.expect_node("identifier")?.as_identifier();
                let target_parameter_idx = remaining_parameters
                    .iter()
                    .position(|p| p.matches(&flag_name));

                let Some(target_parameter_idx) = target_parameter_idx else {
                    return Err(SemanticError::ArgumentNotFound {
                        arg: flag_name,
                        function: function_name.clone(),
                    }
                    .while_parsing(sub_node));
                };

                let par_type = remaining_parameters[target_parameter_idx].to_type();

                if TypeRef::is_boolean(par_type) {
                    Ok((
                        target_parameter_idx,
                        ValueExpression {
                            inner: ValueExpressionInner::Literal(Literal::Boolean(true)),
                            char_idx: sub_node.first_char(),
                        },
                    ))
                } else {
                    Err(SemanticError::TypeMismatchError {
                        expected: TypeRef::boolean(),
                        found: par_type.clone(),
                    }
                    .while_parsing(sub_node))
                }
            },
            unknown => Err(SemanticError::UnexpectedNode {
                found: Identifier::from(unknown),
            }
            .while_parsing(node)),
        }
    }
}

fn extract_string(expression_node: &RuleNode) -> Rc<str> {
    let string = expression_node.tokens_as_string();
    // remove quotation marks
    Rc::from(&string[1..string.len() - 1])
}
