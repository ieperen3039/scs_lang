use std::{collections::HashMap, rc::Rc};

use simple_error::{SimpleError, SimpleResult};

use crate::parsing::rule_nodes::RuleNode;

use super::{
    ast::*, function_collector::FunctionCollector, type_collector::TypeCollector, type_resolver,
};

#[derive(Clone)]
struct VarStorage(Vec<Rc<VariableDeclaration>>);

pub struct FunctionParser<'ns, 'tc> {
    pub root_namespace: &'ns Namespace,
    pub function_collector: FunctionCollector<'tc>,
    pub type_collector: &'tc TypeCollector,
    pub functions: HashMap<NumericFunctionIdentifier, FunctionDeclaration>,
}

impl VarStorage {
    pub fn new() -> VarStorage {
        VarStorage(Vec::new())
    }

    pub fn get(&self, identifier: &str) -> Option<&Rc<VariableDeclaration>> {
        let VarStorage(vec) = self;

        for var in vec {
            if var.name.as_ref() == identifier {
                return Some(&var);
            }
        }

        None
    }

    pub fn insert(&mut self, var: Rc<VariableDeclaration>) -> SimpleResult<()> {
        if matches!(self.get(&var.name), None) {
            return Err(SimpleError::new(format!(
                "Variable {} already exists",
                var.name
            )));
        }

        let VarStorage(vec) = self;
        vec.push(var);

        Ok(())
    }

    pub fn expect(&self, identifier: &str) -> SimpleResult<Rc<VariableDeclaration>> {
        self.get(identifier)
            .map(Rc::clone)
            .ok_or_else(|| SimpleError::new(format!("expected identifier {identifier}")))
    }
}

impl FunctionParser<'_, '_> {
    pub fn new<'ns, 'tc>(
        root_namespace: &'ns Namespace,
        functions: HashMap<NumericFunctionIdentifier, FunctionDeclaration>,
        function_collector: FunctionCollector<'tc>,
        type_collector: &'tc TypeCollector,
    ) -> FunctionParser<'ns, 'tc> {
        FunctionParser {
            root_namespace,
            functions,
            type_collector,
            function_collector,
        }
    }

    // function_block          = statement, { statement_separator, [ statement ] }
    pub fn read_function_block(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        mut variables: VarStorage,
    ) -> SimpleResult<FunctionBody> {
        let mut statements = Vec::new();

        for ele in node.sub_rules.chunks(2) {
            let statement_node = ele[0].expect_node("statement")?;
            let mut statement = self.read_statement(statement_node, this_scope, &mut variables)?;

            if ele.len() == 2 {
                ele[1].expect_node("statement_separator")?;
            } else if let Some(last_mutation) = statement.mutations.last() {
                // this is an implicit return
                if let FunctionExpression::Assignment(_) = last_mutation {
                    return Err(SimpleError::new(
                        "Function ended with an assignment, but no semicolon. This implied that we should return the result of the assignment, but that is always void",
                    ));
                }

                let return_var = match variables.get("return") {
                    Some(var) => var.to_owned(),
                    None => {
                        let return_var = Rc::from(VariableDeclaration {
                            var_type: last_mutation.get_type(&self.functions),
                            name: Identifier::from("return"),
                        });
                        variables.insert(return_var.clone());
                        return_var
                    },
                };

                statement
                    .mutations
                    .push(FunctionExpression::Assignment(return_var))
            }

            statements.push(statement);
        }

        Ok(FunctionBody {
            statements,
            return_var: variables.expect("return")?,
        })
    }

    pub fn get_function_decl(&self, function_id: u32) -> Option<&FunctionDeclaration> {
        self
            .functions
            .get(&function_id)
    }

    pub fn read_function_body(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        parameters: &Vec<Parameter>,
        return_type: &TypeRef,
    ) -> SimpleResult<FunctionBody> {
        debug_assert_eq!(node.rule_name, "function_block");
        let mut variables = VarStorage::new();

        for param in parameters {
            variables.insert(Rc::from(VariableDeclaration {
                var_type: param.par_type.to_owned(),
                name: param.identifier().to_owned(),
            }));
        }

        let function_block = self.read_function_block(node, this_scope, variables)?;

        if &function_block.return_var.var_type != return_type {
            return Err(SimpleError::new(format!(
                "Expected type {:?} but found type {:?}",
                function_block.return_var.var_type, return_type
            )));
        }

        Ok(function_block)
    }

    // statement = value_expression, { { operator }, function_expression };
    fn read_statement(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<Statement> {
        debug_assert_eq!(node.rule_name, "statement");

        let expression_node = node
            .find_node("value_expression")
            .ok_or_else(|| SimpleError::new("empty statement"))?;

        let expression = self.read_value_expression(expression_node, this_scope, variables)?;

        let mut expression_type = expression.get_type();

        let mut mutations = Vec::new();
        for mutator_node in &node.sub_rules {
            let mutator =
                self.read_mutator(mutator_node, expression_type, this_scope, variables)?;
            expression_type = mutator.get_type(&self.functions);
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
        expression_type: TypeRef,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionExpression> {
        match mutator_node.rule_name {
            "operator" => {
                todo!();
            },
            "function_expression" => {
                let mutator = self.read_function_expression(
                    mutator_node,
                    &vec![expression_type],
                    this_scope,
                    variables,
                )?;
                Ok(mutator)
            },
            _ => todo!(),
        }
    }

    fn read_expression(
        &self,
        super_node: &RuleNode,
        this_scope: &Namespace,
        variables: &mut VarStorage,
        target_type: &TypeRef,
    ) -> SimpleResult<Expression> {
        let value_node = super_node.find_node("value_expression");
        if let Some(expr_node) = value_node {
            let value_expression = self.read_value_expression(expr_node, this_scope, variables)?;

            if target_type != &value_expression.get_type() {
                return Err(SimpleError::new(format!(
                    "Expected type {:?} but found type {:?}",
                    target_type,
                    value_expression.get_type()
                )));
            }

            return Ok(Expression::Value(value_expression));
        } else {
            let function_node = super_node.find_node("function_expression");
            if let Some(expr_node) = function_node {
                let expr = if let TypeRef::Function(fn_type) = target_type {
                    self.read_function_expression(
                        expr_node,
                        &fn_type.parameters,
                        this_scope,
                        variables,
                    )?
                } else {
                    self.read_function_expression(expr_node, &Vec::new(), this_scope, variables)?
                };
                return Ok(Expression::Functional(expr));
            } else {
                return Err(SimpleError::new(
                    "Expected value_expression or function_expression",
                ));
            }
        };
    }

    // value_expression = variable_name | tuple_construction | _literal;
    // _literal = string_literal | integer_literal | dollar_string_literal | raw_string_literal;
    fn read_value_expression(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<ValueExpression> {
        debug_assert!(node.rule_name == "value_expression");
        match node.rule_name {
            "variable_name" => {
                let variable = variables.expect(&node.tokens_as_string())?;
                Ok(ValueExpression::Variable(variable))
            },
            "tuple_construction" => {
                let mut tuple_elements = Vec::new();
                for subnode in &node.sub_rules {
                    tuple_elements.push(self.read_value_expression(subnode, this_scope, variables)?)
                }

                Ok(ValueExpression::Tuple(tuple_elements))
            },
            "string_literal" => {
                let string_value = extract_string(node);
                Ok(ValueExpression::Literal(Literal::String(string_value)))
            },
            "integer_literal" => {
                let as_string = node.tokens_as_string();
                let parse_result = as_string.parse::<i32>();

                match parse_result {
                    Ok(integer_value) => {
                        Ok(ValueExpression::Literal(Literal::Number(integer_value)))
                    },
                    Err(err) => Err(SimpleError::new(format!(
                        "Could not parse integer literal \"{as_string}\": {err}"
                    ))),
                }
            },
            "dollar_string_literal" => unimplemented!("{}", node.rule_name),
            "raw_string_literal" => unimplemented!("{}", node.rule_name),
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "node is not an _expression node: {unknown_rule}"
                )))
            },
        }
    }

    // function_expression = implicit_par_lambda | explicit_par_lambda | function_call | mutator_cast | mutator_assign;
    fn read_function_expression(
        &self,
        node: &RuleNode<'_, '_>,
        // what kind of arguments do we expect this function expression to accept?
        argument_types: &Vec<TypeRef>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionExpression> {
        debug_assert!(node.rule_name == "function_expression");

        let sub_node = node
            .sub_rules
            .first()
            .expect("function_expression must have subnodes");

        match sub_node.rule_name {
            "function_call" => {
                let function_call = self.read_function_call(node, this_scope, variables)?;
                Ok(FunctionExpression::FunctionCall(function_call))
            },
            "implicit_par_lambda" => {
                if argument_types.len() != 1 {
                    return Err(SimpleError::new(format!("implicit_par_lambda can only accept 1 argument, but was expected to handle {}", argument_types.len())));
                }
                self.read_implicit_parameter_lamda(
                    node,
                    variables,
                    this_scope,
                    argument_types.first().unwrap().clone(),
                )
            },
            "explicit_par_lambda" => {
                self.read_explicit_parameter_lamda(node, variables, this_scope, argument_types)
            },
            "mutator_cast" => unimplemented!("{}", node.rule_name),
            "mutator_assign" => unimplemented!("{}", node.rule_name),
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "unknown expression rule {unknown_rule}"
                )))
            },
        }
    }

    // explicit_par_lambda     = [ untyped_parameter_list ], [ return_type ], function_block;
    fn read_explicit_parameter_lamda(
        &self,
        node: &RuleNode,
        outer_variables: &mut VarStorage,
        this_scope: &Namespace,
        argument_types: &Vec<TypeRef>,
    ) -> SimpleResult<FunctionExpression> {
        debug_assert!(node.rule_name == "explicit_par_lambda");

        let parameter_node = node.find_node("untyped_parameter_list");
        let return_type_node = node.find_node("return_type");
        let function_node = node.expect_node("function_block")?;

        let untyped_parameter_list = match parameter_node {
            Some(p_node) => self
                .function_collector
                .read_untyped_parameter_list(p_node)?,
            None => Vec::new(),
        };

        if argument_types.len() != untyped_parameter_list.len() {
            return Err(SimpleError::new(format!(
                "Expected lamda with {} arguments, but found {}",
                argument_types.len(),
                untyped_parameter_list.len()
            )));
        }

        let mut parameters = VarStorage::new();

        for (t, n) in argument_types.iter().zip(untyped_parameter_list.iter()) {
            parameters.insert(Rc::from(VariableDeclaration {
                var_type: t.to_owned(),
                name: n.to_owned(),
            }))?;
        }

        let function_block = self.read_function_block(function_node, this_scope, parameters)?;

        // check return type if possible
        self.check_type_node(return_type_node, &function_block.return_var.var_type)?;

        Ok(FunctionExpression::Lamda(Lamda {
            parameters: untyped_parameter_list,
            body: function_block,
            capture: vec![], // TODO captures
        }))
    }

    // the first statement of a implicit parameter lamdas starts with a function expression
    // the body is therefore not just a function body
    // implicit_par_lambda     = "{", { { operator }, function_expression }, { statement_separator, statement }, [ statement_separator ], "}";
    fn read_implicit_parameter_lamda(
        &self,
        node: &RuleNode,
        outer_variables: &mut VarStorage,
        this_scope: &Namespace,
        argument_type: TypeRef,
    ) -> SimpleResult<FunctionExpression> {
        debug_assert!(node.rule_name == "implicit_par_lambda");

        let implicit_par_name = Identifier::from("__implicit");
        let implicit_par = Rc::from(VariableDeclaration {
            name: implicit_par_name.clone(),
            var_type: argument_type.clone(),
        });

        let mut variables = VarStorage::new();
        variables.insert(implicit_par.clone());

        // handle the first statement separately
        let mut expression_type = argument_type.clone();
        let mut mutations = Vec::new();
        for mutator_node in &node.sub_rules {
            let mutator =
                self.read_mutator(mutator_node, expression_type, this_scope, &mut variables)?;
            expression_type = mutator.get_type(&self.functions);
            mutations.push(mutator);
        }

        // this works because the first part contains no "statement" nodes
        let mut function_block = self.read_function_block(node, this_scope, variables)?;
        // now add the first statement
        function_block.statements.insert(
            0,
            Statement {
                base_element: ValueExpression::Variable(implicit_par),
                mutations,
            },
        );

        Ok(FunctionExpression::Lamda(Lamda {
            parameters: vec![implicit_par_name],
            body: function_block,
            capture: vec![], // TODO captures
        }))
    }

    fn check_type_node(
        &self,
        type_node: Option<&RuleNode>,
        actual_type: &TypeRef,
    ) -> SimpleResult<()> {
        if type_node.is_some() {
            let given_type = self.type_collector.read_type_ref(type_node.unwrap());
            if let Ok(given_type) = given_type {
                if *actual_type != given_type {
                    return Err(SimpleError::new(format!(
                        "Expected type {:?} but found type {:?}",
                        given_type, actual_type
                    )));
                }
            }
        }
        Ok(())
    }

    // function_call           = function_name, "(", [ _argument_list ], ")";
    fn read_function_call(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionCall> {
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

        let function_decl = self.get_function_decl(function_id)
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
    ) -> SimpleResult<Vec<Option<Expression>>> {
        let mut remaining_parameters = function_decl.parameters.clone();
        // maps the current parameters to the corresponding indices in the original parameters vec
        let mut indices: Vec<usize> = (0..function_decl.parameters.len()).collect();

        let mut arguments = Vec::new();
        arguments.resize(remaining_parameters.len(), None);

        for arg_node in argument_nodes {
            let (par_idx, expr) =
                self.read_argument(arg_node, &remaining_parameters, this_scope, variables)?;
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
        node: &RuleNode<'_, '_>,
        remaining_parameters: &Vec<Parameter>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<(usize, Expression)> {
        match node.rule_name {
            "named_argument" => {
                let arg_name = node.expect_node("identifier")?.as_identifier();
                let some_arg_name = Some(arg_name.clone());
                let target_parameter_idx = remaining_parameters
                    .iter()
                    .position(|p| p.long_name == some_arg_name);

                let Some(target_parameter_idx) = target_parameter_idx else {
                    return Err(SimpleError::new(format!(
                        "No parameter with name {arg_name} found"
                    )));
                };

                let expected_type = &remaining_parameters[target_parameter_idx].par_type;
                let arg_value = self.read_expression(node, this_scope, variables, expected_type)?;

                Ok((target_parameter_idx, arg_value))
            },
            "unnamed_argument" => {
                let first_non_optional = remaining_parameters
                    .iter()
                    .position(|p| !matches!(p.par_type, TypeRef::Flag | TypeRef::Optional(_)));

                let Some(first_non_optional) = first_non_optional else {
                    return Err(SimpleError::new(
                        "unnamed_argument given, but no required parameter can be bound to",
                    ));
                };

                let expected_type = &remaining_parameters[first_non_optional].par_type;
                let arg_value = self.read_expression(node, this_scope, variables, expected_type)?;

                Ok((first_non_optional, arg_value))
            },
            "flag_argument" => {
                let flag_name = node.as_identifier();
                let target_parameter_idx = remaining_parameters
                    .iter()
                    .position(|p| p.short_name == Some(flag_name.clone()));

                let Some(target_parameter_idx) = target_parameter_idx else {
                    return Err(SimpleError::new(format!("flag {flag_name} not found")));
                };

                let par_type = remaining_parameters[target_parameter_idx].to_type();
                if par_type != &TypeRef::BOOLEAN {
                    return Err(SimpleError::new(format!(
                        "parameter {flag_name} is not a boolean, but {:?} and cannot be used as a flag",
                        par_type
                    )));
                }

                Ok((
                    target_parameter_idx,
                    Expression::Value(ValueExpression::Literal(Literal::Boolean(true))),
                ))
            },
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "unknown expression rule {unknown_rule}"
                )))
            },
        }
    }
}

fn extract_string(expression_node: &RuleNode<'_, '_>) -> Rc<str> {
    let string = expression_node.tokens_as_string();
    // remove quotation marks
    Rc::from(&string[1..string.len() - 1])
}
