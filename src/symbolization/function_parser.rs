use std::{collections::HashMap, rc::Rc, vec};

use crate::parsing::rule_nodes::RuleNode;

use super::{
    ast::*,
    function_collector::FunctionCollector,
    semantic_result::{SemanticError, SemanticResult},
    type_resolver,
    variable_storage::VarStorage,
};

pub struct FunctionParser<'ns, 'fc> {
    pub root_namespace: &'ns Namespace,
    pub function_collector: &'fc mut FunctionCollector,
    pub functions: HashMap<FunctionId, FunctionDeclaration>,
}

impl FunctionParser<'_, '_> {
    pub fn new<'ns, 'fc>(
        root_namespace: &'ns Namespace,
        functions: HashMap<FunctionId, FunctionDeclaration>,
        function_collector: &'fc mut FunctionCollector,
    ) -> FunctionParser<'ns, 'fc> {
        FunctionParser {
            root_namespace,
            functions,
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
        if nodes.is_empty() {
            return Err(SemanticError::NodeNotFound {
                expected: "statment",
                parent_node: Identifier::from("function_body"),
            });
        }

        debug_assert_eq!(nodes.first().map(|n| n.rule_name), Some("statement"));

        let parameters = variables.get_var_ids();
        let mut statements = Vec::new();

        for ele in nodes.chunks(2) {
            let statement_node = &ele[0];
            if statement_node.rule_name != "statement" {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(statement_node.rule_name),
                    parent_node: "function_body",
                });
            };

            let mut statement = self
                .read_statement(statement_node, this_scope, &mut variables)
                .map_err(|e| SemanticError::WhileParsing {
                    rule_name: "statement",
                    char_idx: statement_node.first_char(),
                    cause: Box::from(e),
                })?;

            if ele.len() == 2 {
                if ele[1].rule_name != "statement_separator" {
                    return Err(SemanticError::UnexpectedNode {
                        found: Identifier::from(statement_node.rule_name),
                        parent_node: "function_body",
                    });
                };
            } else if let Some(last_mutation) = statement.mutations.last() {
                // this is an implicit return
                if let FunctionExpression::Assignment(_) = last_mutation {
                    return Err(SemanticError::BrokenControl(
                        "Function ended with an assignment, but no semicolon. This implied that we should return the result of the assignment, but that is always void",
                    ));
                }
                let last_expression_type = last_mutation.get_result_type(&self.functions);

                let return_var = match variables.use_var_by_name("return") {
                    Some(var) => {
                        if var.var_type != last_expression_type {
                            return Err(SemanticError::TypeMismatchError {
                                expected: var.var_type.clone(),
                                found: last_expression_type,
                            });
                        }
                        var
                    },
                    None => variables.insert(Identifier::from("return"), last_expression_type)?,
                };

                statement
                    .mutations
                    .push(FunctionExpression::Assignment(return_var))
            }

            statements.push(statement);
        }

        Ok(FunctionBody {
            statements,
            return_var: variables
                .use_var_by_name("return")
                .expect("no return var found"),
            parameters,
        })
    }

    pub fn read_function_body(
        &self,
        id: FunctionId,
        node: &RuleNode,
        this_scope: &Namespace,
    ) -> SemanticResult<FunctionBody> {
        debug_assert_eq!(node.rule_name, "function_body");
        let function_declaration = self.functions.get(&id).unwrap();

        let mut variables = VarStorage::new();

        for param in &function_declaration.parameters {
            variables.insert_from_param(param)?;
        }

        let function_body = self.read_statements(&node.sub_rules, this_scope, &mut variables)?;

        if &function_body.return_var.var_type != &function_declaration.return_type {
            return Err(SemanticError::TypeMismatchError {
                expected: function_body.return_var.var_type.clone(),
                found: function_declaration.return_type.clone(),
            });
        }

        for unused in variables.get_unused_vars() {
            println!(
                "WARNING: parameter {} of function {} not used",
                unused.name, function_declaration.name
            )
        }

        Ok(function_body)
    }

    // statement = value_expression, { { operator }, function_expression };
    fn read_statement(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<Statement> {
        debug_assert_eq!(node.rule_name, "statement");

        let mut node_iter: std::slice::Iter<RuleNode> = node.sub_rules.iter();

        let expression_node = node_iter.next();
        if expression_node.map(|n| n.rule_name) != Some("value_expression")
        {
            return Err(SemanticError::NodeNotFound { expected: "value_expression", parent_node: Identifier::from(node.rule_name) });
        }
        let expression_node = expression_node.unwrap();

        let expression = self
            .read_value_expression(expression_node, this_scope, variables)
            .map_err(|e| SemanticError::WhileParsing {
                rule_name: "value_expression",
                char_idx: expression_node.first_char(),
                cause: Box::from(e),
            })?;

        let mut expression_type = expression.get_type();
        let mut mutations = Vec::new();

        while let Some(mutator_node) = node_iter.next() {
            let mutator = self.read_mutator(
                mutator_node,
                &mut node_iter,
                expression_type,
                this_scope,
                variables,
            )?;
            expression_type = mutator.get_result_type(&self.functions);
            mutations.push(mutator);
        }

        Ok(Statement {
            base_element: expression,
            mutations,
        })
    }

    fn read_mutator(
        &self,
        mutator_node: &RuleNode,
        mutator_iter: &mut std::slice::Iter<RuleNode>,
        expression_type: TypeRef,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<FunctionExpression> {
        match mutator_node.rule_name {
            "operator" => {
                let operator_str = mutator_node.as_identifier();
                let function_id = self
                    .root_namespace
                    .functions
                    .get(&operator_str)
                    .ok_or_else(|| SemanticError::SymbolNotFound {
                        kind: "operator",
                        symbol: operator_str,
                    })?;

                let inner_type = self
                    .functions
                    .get(function_id)
                    .map(|f| TypeRef::from_fn_decl(&f))
                    .expect("Broken function call");

                if let Some(inner_mutator_node) = mutator_iter.next() {
                    let inner_expression = self.read_mutator(
                        inner_mutator_node,
                        mutator_iter,
                        inner_type,
                        this_scope,
                        variables,
                    )?;

                    Ok(FunctionExpression::Operator(Operator {
                        id: *function_id,
                        arg: Box::new(inner_expression),
                    }))
                } else {
                    Err(SemanticError::NodeNotFound {
                        expected: "function_expression or operator",
                        parent_node: Identifier::from(mutator_node.rule_name),
                    })
                }
            },
            "function_expression" => self.read_function_expression(
                mutator_node,
                &vec![expression_type],
                this_scope,
                variables,
            ),
            unknown => {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(unknown),
                    parent_node: "mutator",
                })
            },
        }
    }

    fn read_expression(
        &self,
        super_node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
        target_type: &TypeRef,
    ) -> SemanticResult<Expression> {
        let value_node = super_node.find_node("value_expression");
        if let Some(expr_node) = value_node {
            let value_expression = self
                .read_value_expression(expr_node, this_scope, variables)
                .map_err(|e| SemanticError::WhileParsing {
                    rule_name: "value_expression",
                    char_idx: expr_node.first_char(),
                    cause: Box::from(e),
                })?;

            if target_type != &value_expression.get_type() {
                return Err(SemanticError::TypeMismatchError {
                    expected: target_type.clone(),
                    found: value_expression.get_type(),
                });
            }

            return Ok(Expression::Value(value_expression));
        } else {
            let function_node = super_node.find_node("function_expression");
            if let Some(expr_node) = function_node {
                if let TypeRef::Function(fn_type) = target_type {
                    let expr = self
                        .read_function_expression(
                            expr_node,
                            &fn_type.parameters,
                            this_scope,
                            variables,
                        )
                        .map_err(|e| SemanticError::WhileParsing {
                            rule_name: "function_expression",
                            char_idx: expr_node.first_char(),
                            cause: Box::from(e),
                        })?;

                    let expr_result_type = expr.get_result_type(&self.functions);
                    if fn_type.return_type.as_ref() != &expr_result_type {
                        return Err(SemanticError::TypeMismatchError {
                            expected: target_type.clone(),
                            found: expr_result_type,
                        });
                    }
                    return Ok(Expression::Functional(expr));
                } else {
                    let expr = self
                        .read_function_expression(expr_node, &Vec::new(), this_scope, variables)
                        .map_err(|e| SemanticError::WhileParsing {
                            rule_name: "function_expression",
                            char_idx: expr_node.first_char(),
                            cause: Box::from(e),
                        })?;

                    let expr_result_type = expr.get_result_type(&self.functions);
                    if target_type != &expr_result_type {
                        return Err(SemanticError::TypeMismatchError {
                            expected: target_type.clone(),
                            found: expr_result_type,
                        });
                    }
                    return Ok(Expression::Functional(expr));
                };
            } else {
                return Err(SemanticError::NodeNotFound {
                    expected: "value_expression or function_expression",
                    parent_node: Identifier::from(super_node.rule_name),
                });
            }
        };
    }

    // value_expression = variable_name | tuple_construction | _literal;
    // _literal = string_literal | integer_literal | dollar_string_literal | raw_string_literal;
    fn read_value_expression(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<ValueExpression> {
        debug_assert!(node.rule_name == "value_expression");

        let sub_node = node
            .sub_rules
            .first()
            .expect("function_expression must have subnodes");

        match sub_node.rule_name {
            "variable_name" => {
                let var_name = sub_node.as_identifier();
                let variable = variables.use_var_by_name(&var_name).ok_or_else(|| {
                    SemanticError::SymbolNotFound {
                        kind: "variable",
                        symbol: var_name,
                    }
                })?;
                Ok(ValueExpression::Variable(variable))
            },
            "tuple_construction" => {
                let mut tuple_elements = Vec::new();
                for tuple_elt_node in &sub_node.sub_rules {
                    tuple_elements.push(self.read_value_expression(tuple_elt_node, this_scope, variables)?)
                }

                Ok(ValueExpression::Tuple(tuple_elements))
            },
            "string_literal" => {
                let string_value = extract_string(sub_node);
                Ok(ValueExpression::Literal(Literal::String(string_value)))
            },
            "integer_literal" => {
                let as_string = sub_node.tokens_as_string();
                let parse_result = as_string.parse::<i32>();

                match parse_result {
                    Ok(integer_value) => {
                        Ok(ValueExpression::Literal(Literal::Number(integer_value)))
                    },
                    Err(_) => Err(SemanticError::InternalError(
                        "Could not parse integer literal",
                    )),
                }
            },
            "dollar_string_literal" => unimplemented!("{}", node.rule_name),
            "raw_string_literal" => unimplemented!("{}", node.rule_name),
            unknown => {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(unknown),
                    parent_node: "value_expression",
                })
            },
        }
    }

    // function_expression = implicit_par_lamda | explicit_par_lamda | function_call | mutator_cast | mutator_assign;
    fn read_function_expression(
        &self,
        node: &RuleNode,
        // what kind of arguments do we expect this function expression to accept?
        argument_types: &Vec<TypeRef>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<FunctionExpression> {
        debug_assert!(node.rule_name == "function_expression");

        let sub_node = node
            .sub_rules
            .first()
            .expect("function_expression must have subnodes");

        match sub_node.rule_name {
            "function_call" => {
                let function_call = self
                    .read_function_call(sub_node, this_scope, variables)
                    .map_err(|e| SemanticError::WhileParsing {
                        rule_name: "function_call",
                        char_idx: sub_node.first_char(),
                        cause: Box::from(e),
                    })?;
                Ok(FunctionExpression::FunctionCall(function_call))
            },
            "implicit_par_lamda" => {
                if argument_types.len() != 1 {
                    return Err(SemanticError::InvalidNumerOfParameters {
                        what: "implicit_par_lamda",
                        num_found: argument_types.len(),
                        expected: String::from("1 argument"),
                    });
                }
                self.read_implicit_parameter_lamda(
                    sub_node,
                    variables,
                    this_scope,
                    argument_types.first().unwrap().clone(),
                )
                .map_err(|e| SemanticError::WhileParsing {
                    rule_name: "implicit_par_lamda",
                    char_idx: sub_node.first_char(),
                    cause: Box::from(e),
                })
            },
            "explicit_par_lamda" => self
                .read_explicit_parameter_lamda(sub_node, variables, this_scope, argument_types)
                .map_err(|e| SemanticError::WhileParsing {
                    rule_name: "explicit_par_lamda",
                    char_idx: sub_node.first_char(),
                    cause: Box::from(e),
                }),
            "mutator_cast" => unimplemented!("{}", node.rule_name),
            "mutator_assign" => unimplemented!("{}", node.rule_name),
            unknown => {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(unknown),
                    parent_node: "function_expression",
                })
            },
        }
    }

    // explicit_par_lamda     = [ untyped_parameter_list ], [ return_type ], function_body;
    fn read_explicit_parameter_lamda(
        &self,
        node: &RuleNode,
        outer_variables: &mut VarStorage,
        this_scope: &Namespace,
        argument_types: &Vec<TypeRef>,
    ) -> SemanticResult<FunctionExpression> {
        debug_assert!(node.rule_name == "explicit_par_lamda");

        let parameter_node = node.find_node("untyped_parameter_list");
        let return_type_node = node.find_node("return_type");
        let function_node = node.expect_node("function_body")?;

        let untyped_parameter_list = match parameter_node {
            Some(p_node) => self
                .function_collector
                .read_untyped_parameter_list(p_node)?,
            None => Vec::new(),
        };

        if argument_types.len() != untyped_parameter_list.len() {
            return Err(SemanticError::InvalidNumerOfParameters {
                what: "explicit_par_lamda",
                num_found: untyped_parameter_list.len(),
                expected: format!("{} arguments", argument_types.len()),
            });
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

        Ok(FunctionExpression::Lamda(Lamda {
            parameters: untyped_parameter_list,
            body: function_body,
            capture,
        }))
    }

    // the first statement of a implicit parameter lamdas starts with a function expression
    // the body is therefore not just a function body
    // implicit_par_lamda     = "{", { { operator }, function_expression }, { statement_separator, statement }, [ statement_separator ], "}";
    fn read_implicit_parameter_lamda(
        &self,
        node: &RuleNode,
        outer_variables: &mut VarStorage,
        this_scope: &Namespace,
        argument_type: TypeRef,
    ) -> SemanticResult<FunctionExpression> {
        debug_assert!(node.rule_name == "implicit_par_lamda");

        let mut inner_variables = VarStorage::from(outer_variables);

        let implicit_par_name = Identifier::from("__implicit");
        let implicit_par =
            inner_variables.insert(implicit_par_name.clone(), argument_type.clone())?;

        // handle the first statement separately
        let mut expression_type = argument_type.clone();
        let mut mutations = Vec::new();

        let mut mutator_iter: std::slice::Iter<RuleNode> = node.sub_rules.iter();
        while let Some(mutator_node) = mutator_iter.next() {
            if mutator_node.rule_name == "statement_separator" {
                // if there is another statement, then there must be a statement_separator
                // in between the mutators and this statement
                break;
            }

            let mutator = self.read_mutator(
                mutator_node,
                &mut mutator_iter,
                expression_type,
                this_scope,
                &mut inner_variables,
            )?;
            expression_type = mutator.get_result_type(&self.functions);
            mutations.push(mutator);
        }

        let initial_statement = Statement {
            base_element: ValueExpression::Variable(implicit_par),
            mutations,
        };

        let num_elements_left = node.sub_rules.len() - mutator_iter.count();
        let function_body = if num_elements_left > 0 {
            let mut function_body = self
                .read_statements(
                    &node.sub_rules[num_elements_left..],
                    this_scope,
                    &mut inner_variables,
                )
                .map_err(|e| SemanticError::WhileParsing {
                    rule_name: "implicit_par_lamda",
                    char_idx: node.first_char(),
                    cause: Box::from(e),
                })?;

            // now add the first statement
            function_body.statements.insert(0, initial_statement);
            function_body
        } else {
            let parameters = inner_variables.get_var_ids();
            let return_var = inner_variables.use_var_by_name("return").ok_or_else(|| {
                SemanticError::SymbolNotFound {
                    kind: "variable",
                    symbol: Identifier::from("return"),
                }
            })?;

            FunctionBody {
                parameters,
                statements: vec![initial_statement],
                return_var,
            }
        };

        let mut capture = Vec::new();
        for var in inner_variables.get_used_vars() {
            if outer_variables.contains(&var) {
                capture.push(var)
            }
        }

        Ok(FunctionExpression::Lamda(Lamda {
            parameters: vec![implicit_par_name],
            body: function_body,
            capture,
        }))
    }

    // function_call = function_name, [ _argument_list ];
    // _argument_list = argument, { argument };
    fn read_function_call(
        &self,
        node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<FunctionCall> {
        assert_eq!(node.rule_name, "function_call");
        let scope: Vec<Identifier> = node
            .find_nodes("scope_name")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        let function_name_node = node.expect_node("function_name")?;
        let function_name = function_name_node.as_identifier();

        // all function declarations are read now
        let function_id = type_resolver::resolve_function_name(
            function_name,
            &scope,
            &self.root_namespace,
            this_scope,
        )?;

        let function_decl = {
            let this = &self;
            this.functions.get(&function_id)
        }
        .expect("function id exist, but function is not found");

        let argument_nodes = node.find_nodes("argument");
        let arguments =
            self.read_arguments(argument_nodes, function_decl, this_scope, variables)?;

        Ok(FunctionCall {
            id: function_id,
            arguments,
        })
    }

    // if successful, returns the arguments, where the indices map to function_decl.parameters
    fn read_arguments(
        &self,
        argument_nodes: Vec<&RuleNode>,
        function_decl: &FunctionDeclaration,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SemanticResult<Vec<Option<Expression>>> {
        if argument_nodes.is_empty() {
            return Ok(Vec::new());
        }

        let mut remaining_parameters = function_decl.parameters.clone();
        // maps the current parameters to the corresponding indices in the original parameters vec
        let mut indices: Vec<usize> = (0..function_decl.parameters.len()).collect();

        let mut arguments = Vec::new();
        arguments.resize(remaining_parameters.len(), None);

        for arg_node in argument_nodes {
            let (par_idx, expr) = self.read_argument(
                &function_decl.name,
                arg_node,
                &remaining_parameters,
                this_scope,
                variables,
            )?;
            let target_par = remaining_parameters.remove(par_idx);
            let target_idx = indices.remove(par_idx);

            arguments[target_idx] = Some(expr);
        }
        Ok(arguments)
    }

    // argument                = named_argument | unnamed_argument | flag_argument;
    // unnamed_argument        = _expression;
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
    ) -> SemanticResult<(usize, Expression)> {
        debug_assert_eq!(node.rule_name, "argument");
        
        let sub_node = node
            .sub_rules
            .first()
            .expect("function_expression must have subnodes");

        match sub_node.rule_name {
            "named_argument" => {
                let arg_name_node = &sub_node.expect_node("identifier")?;
                let arg_name = arg_name_node.as_identifier();
                let some_arg_name = Some(arg_name.clone());
                let target_parameter_idx = remaining_parameters
                    .iter()
                    .position(|p| p.long_name == some_arg_name);

                let Some(target_parameter_idx) = target_parameter_idx else {
                    return Err(SemanticError::ArgumentInvalid {
                        arg: arg_name,
                        function: function_name.clone(),
                    });
                };

                let expected_type = &remaining_parameters[target_parameter_idx].par_type;
                let arg_value = self.read_expression(sub_node, this_scope, variables, expected_type)?;

                Ok((target_parameter_idx, arg_value))
            },
            "unnamed_argument" => {
                let first_non_optional = remaining_parameters
                    .iter()
                    .position(|p| !matches!(p.par_type, TypeRef::Flag | TypeRef::Optional(_)));

                let Some(first_non_optional) = first_non_optional else {
                    return Err(SemanticError::ArgumentInvalid {
                        arg: sub_node.as_identifier(),
                        function: function_name.clone(),
                    });
                };

                let expected_type = &remaining_parameters[first_non_optional].par_type;
                let arg_value = self.read_expression(sub_node, this_scope, variables, expected_type)?;

                Ok((first_non_optional, arg_value))
            },
            "flag_argument" => {
                let flag_name = sub_node.as_identifier();
                let target_parameter_idx = remaining_parameters
                    .iter()
                    .position(|p| p.short_name == Some(flag_name.clone()));

                let Some(target_parameter_idx) = target_parameter_idx else {
                    return Err(SemanticError::ArgumentInvalid {
                        arg: flag_name,
                        function: function_name.clone(),
                    });
                };

                let par_type = remaining_parameters[target_parameter_idx].to_type();
                if par_type != &TypeRef::BOOLEAN {
                    return Err(SemanticError::TypeMismatchError {
                        expected: TypeRef::BOOLEAN.clone(),
                        found: par_type.clone(),
                    });
                }

                Ok((
                    target_parameter_idx,
                    Expression::Value(ValueExpression::Literal(Literal::Boolean(true))),
                ))
            },
            unknown => {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(unknown),
                    parent_node: "argument",
                })
            },
        }
    }
}

fn extract_string(expression_node: &RuleNode) -> Rc<str> {
    let string = expression_node.tokens_as_string();
    // remove quotation marks
    Rc::from(&string[1..string.len() - 1])
}
