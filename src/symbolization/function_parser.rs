use std::{collections::HashMap, rc::Rc};

use simple_error::{SimpleError, SimpleResult};

use crate::{built_in::primitives, parsing::rule_nodes::RuleNode};

use super::{
    ast::*, function_collector::FunctionCollector, type_collector::TypeCollector, type_resolver,
};

type VarStorage = HashMap<Identifier, Rc<VariableDeclaration>>;

pub struct FunctionParser<'ns, 'tc> {
    pub root_namespace: &'ns Namespace,
    pub function_collector: FunctionCollector<'tc>,
    pub type_collector: &'tc TypeCollector,
    pub functions: HashMap<NumericFunctionIdentifier, FunctionDeclaration>,
    function_definitions: HashMap<u32, FunctionBody>,
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
            function_definitions: HashMap::new(),
        }
    }

    // function_block          = statement, { statement_separator, [ statement ] }
    pub fn read_function_block(
        &mut self,
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
                        variables.insert(return_var.name.clone(), return_var.clone());
                        return_var
                    }
                };

                statement
                    .mutations
                    .push(FunctionExpression::Assignment(return_var))
            }

            statements.push(statement);
        }

        Ok(FunctionBody {
            statements,
            return_var: expect_variable(&variables, "return")?,
        })
    }

    pub fn read_function_body(
        &mut self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        parameters: &Vec<Parameter>,
        return_var: Rc<VariableDeclaration>,
    ) -> SimpleResult<FunctionBody> {
        debug_assert_eq!(node.rule_name, "function_block");
        let mut variables: VarStorage = HashMap::new();

        // the return statement is implemented as a variable
        // this variable will hold the return value when a function returns
        variables.insert(return_var.name.clone(), return_var);

        for param in parameters {
            variables.insert(
                param.identifier().clone(),
                Rc::from(VariableDeclaration {
                    var_type: param.par_type.to_owned(),
                    name: param.identifier().to_owned(),
                }),
            );
        }

        self.read_function_block(node, this_scope, variables)
    }

    // statement = value_expression, { { operator }, function_expression };
    pub fn read_statement(
        &mut self,
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
            match mutator_node.rule_name {
                "operator" => {
                    todo!();
                }
                "function_expression" => {
                    let mutator = self.read_function_expression(
                        mutator_node,
                        vec![expression_type],
                        this_scope,
                        variables,
                    )?;

                    expression_type = mutator.get_type(&self.functions);
                    mutations.push(mutator);
                }
            };
        }

        Ok(Statement {
            base_element: expression,
            mutations,
        })
    }

    // value_expression = variable_name | tuple_construction | _literal;
    // _literal = string_literal | integer_literal | dollar_string_literal | raw_string_literal;
    fn read_value_expression(
        &mut self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<ValueExpression> {
        debug_assert!(node.rule_name == "value_expression");
        match node.rule_name {
            "variable_name" => {
                let variable = expect_variable(variables, &node.tokens_as_string())?;
                Ok(ValueExpression::Variable(variable))
            }
            "tuple_construction" => {
                let mut tuple_elements = Vec::new();
                for subnode in &node.sub_rules {
                    tuple_elements.push(self.read_value_expression(subnode, this_scope, variables)?)
                }

                Ok(ValueExpression::Tuple(tuple_elements))
            }
            "string_literal" => {
                let string_value = extract_string(node);
                Ok(ValueExpression::Literal(Literal::String(string_value)))
            }
            "integer_literal" => {
                let as_string = node.tokens_as_string();
                let parse_result = as_string.parse::<i32>();

                match parse_result {
                    Ok(integer_value) => {
                        Ok(ValueExpression::Literal(Literal::Number(integer_value)))
                    }
                    Err(err) => Err(SimpleError::new(format!(
                        "Could not parse integer literal \"{as_string}\": {err}"
                    ))),
                }
            }
            "dollar_string_literal" => unimplemented!("{}", node.rule_name),
            "raw_string_literal" => unimplemented!("{}", node.rule_name),
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "node is not an _expression node: {unknown_rule}"
                )))
            }
        }
    }

    // function_expression = implicit_par_lambda | explicit_par_lambda | function_call | mutator_cast | mutator_assign;
    fn read_function_expression(
        &mut self,
        node: &RuleNode<'_, '_>,
        argument_types: Vec<TypeRef>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionExpression> {
        debug_assert!(node.rule_name == "function_expression");
        match node.rule_name {
            "function_call" => {
                let function_call = self.read_function_call(node, this_scope, variables)?;
                Ok(FunctionExpression::FunctionCall(function_call))
            }
            "implicit_par_lambda" => {
                // (* the first statement of a implicit parameter lamdas starts with a function expression *)
                // implicit_par_lambda     = "{", { { operator }, function_expression }, { statement_separator, statement }, [ statement_separator ], "}";
                Ok(FunctionExpression::FunctionCall(todo!()))
            }
            "explicit_par_lambda" => {
                // explicit_par_lambda     = "(", [ untyped_parameter_list ], ")", [ ":", return_type ], function_block;
                let function_node = node.expect_node("function_block")?;
                let parameter_node = node.find_node("untyped_parameter_list");
                let return_type_node = node.find_node("return_type");

                let scope_variables = variables.clone();
                let function_block =
                    self.read_function_block(function_node, this_scope, scope_variables)?;
                let return_type = function_block.return_var.var_type;

                // check return type if possible
                if return_type_node.is_some() {
                    let given_type = self.type_collector.read_type_ref(return_type_node.unwrap());
                    if let Ok(given_type) = given_type {
                        if return_type != given_type {
                            return Err(SimpleError::new(format!(
                                "Lamda return type given is {:?} but actual type is {:?}",
                                given_type, return_type
                            )));
                        }
                    }
                }

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

                let parameters = argument_types
                    .into_iter()
                    .zip(untyped_parameter_list.into_iter())
                    .map(|(t, n)| Parameter {
                        long_name: Some(n),
                        par_type: t,
                        short_name: None,
                    })
                    .collect();

                let function_decl = self
                    .function_collector
                    .create_lamda(parameters, return_type);

                let id = function_decl.id;
                self.functions.insert(id, function_decl);

                Ok(FunctionExpression::FunctionCall(FunctionCall {
                    id,
                    arguments: todo!(),
                }))
            }
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "unknown expression rule {unknown_rule}"
                )))
            }
        }
    }

    // function_call           = function_name, "(", [ _argument_list ], ")";
    fn read_function_call(
        &mut self,
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

        let function_decl = self
            .functions
            .get(&function_id)
            .expect("function id exist, but function is not found");

        let argument_nodes = node.find_nodes("argument");
        let mut arguments = HashMap::new();
        for arg_node in argument_nodes {
            let (name, args) =
                self.read_argument(arg_node, function_decl, this_scope, variables)?;
            arguments.insert(name, args);
        }

        // TODO: explicit generic arguments
        Ok(FunctionCall {
            id: function_id,
            arguments,
        })
    }

    // argument                = named_argument | single_argument | flag_argument;
    // single_argument         = _expression;
    // flag_argument           = ? IDENTIFIER ?;
    // named_argument          = identifier, "=", _expression;
    fn read_argument(
        &mut self,
        node: &RuleNode<'_, '_>,
        of_function: &FunctionDeclaration,
        this_scope: &Namespace,
        variables: &mut HashMap<Rc<str>, Rc<VariableDeclaration>>,
    ) -> SimpleResult<(Rc<str>, Expression)> {
        match node.rule_name {
            "named_argument" => {
                let arg_name = node.expect_node("identifier")?.as_identifier();
                let target_parameter = of_function
                    .parameters
                    .iter()
                    .find(|p| p.long_name == Some(arg_name));
                let Some(target_parameter) = target_parameter else {
                    return Err(SimpleError::new(format!(
                        "No parameter with name {arg_name} found"
                    )));
                };

                let expected_type = target_parameter.par_type;

                let value_node = node.find_node("value_expression");
                let arg_value = if let Some(expr_node) = value_node {
                    Expression::Value(self.read_value_expression(expr_node, this_scope, variables)?)
                } else {
                    let function_node = node.find_node("function_expression");
                    if let Some(expr_node) = function_node {
                        if let TypeRef::Function(fn_type) = expected_type {
                            Expression::Functional(self.read_function_expression(
                                expr_node,
                                fn_type.parameters,
                                this_scope,
                                variables,
                            )?)
                        } else {
                            Expression::Functional(self.read_function_expression(
                                expr_node,
                                Vec::new(),
                                this_scope,
                                variables,
                            )?)
                        }
                    } else {
                        return Err(SimpleError::new(
                            "named_argument without value_expression and function_expression",
                        ));
                    }
                };

                let arg_name_node = node.expect_node("identifier")?;
                Ok((arg_name_node.as_identifier(), arg_value))
            }
            "single_argument" => {
                let value_node = node.expect_node("value_expression")?;
                let arg_value = self.read_value_expression(value_node, this_scope, variables)?;
                Ok((Identifier::from(""), Expression::Value(arg_value)))
            }
            "flag_argument" => Ok((
                node.as_identifier(),
                Expression::Value(ValueExpression::Literal(Literal::Boolean(true))),
            )),
        }
    }
}

fn extract_string(expression_node: &RuleNode<'_, '_>) -> Rc<str> {
    let string = expression_node.tokens_as_string();
    // remove quotation marks
    Rc::from(&string[1..string.len() - 1])
}

fn expect_variable(
    variables: &VarStorage,
    identifier: &str,
) -> SimpleResult<Rc<VariableDeclaration>> {
    variables
        .get(identifier)
        .map(Rc::clone)
        .ok_or_else(|| SimpleError::new(format!("expected identifier {identifier}")))
}
