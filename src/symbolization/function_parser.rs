use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::{ast::*, type_resolver};

type VarStorage = HashMap<Identifier, Rc<VariableDeclaration>>;

pub struct FunctionParser<'s> {
    pub root_scope: &'s Scope
}

impl FunctionParser<'_> {
    // function_block          = statement, { statement_separator, [ statement ] }
    pub fn read_function_block(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Scope,
        mut variables: VarStorage,
    ) -> Result<FunctionBody, SimpleError> {
        let mut statements = Vec::new();

        for ele in node.sub_rules.chunks(2) {
            let statement_node = ele[0].expect_node("statement")?;
            let mut statement = self.read_statement(statement_node, this_scope, &mut variables)?;

            if ele.len() == 2 {
                ele[1].expect_node("statement_separator")?;
            } else if let Some(last_mutation) = statement.mutations.last() {
                // this is an implicit return
                if let Mutator::Assignment(_) = last_mutation {
                    return Err(SimpleError::new(
                        "Function ended with an assignment, but no semicolon. This implied that we should return the result of the assignment, but that is always void",
                    ));
                }

                let return_var = match variables.get("return") {
                    Some(var) => var.to_owned(),
                    None => {
                        let return_var = Rc::from(VariableDeclaration {
                            var_type: last_mutation.get_type(),
                            name: Identifier::from("return"),
                        });
                        variables.insert(return_var.name.clone(), return_var.clone());
                        return_var
                    }
                };

                statement.mutations.push(Mutator::Assignment(return_var))
            }

            statements.push(statement);
        }

        Ok(FunctionBody {
            statements,
            return_var: expect_variable(&variables, "return")?,
        })
    }

    pub fn read_function_body(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Scope,
        parameters: &HashMap<Identifier, TypeRef>,
        return_var: Rc<VariableDeclaration>,
    ) -> Result<FunctionBody, SimpleError> {
        debug_assert_eq!(node.rule_name, "function_block");
        let mut variables: VarStorage = HashMap::new();

        // the return statement is implemented as a variable
        // this variable will hold the return value when a function returns
        variables.insert(return_var.name.clone(), return_var);

        for (identifier, type_name) in parameters {
            variables.insert(
                identifier.clone(),
                Rc::from(VariableDeclaration {
                    var_type: type_name.to_owned(),
                    name: identifier.to_owned(),
                }),
            );
        }

        self.read_function_block(node, this_scope, variables)
    }

    // statement               = _expression, { _mutator };
    pub fn read_statement(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Scope,
        variables: &mut VarStorage,
    ) -> Result<Statement, SimpleError> {
        debug_assert_eq!(node.rule_name, "statement");

        let expression_node = node
            .sub_rules
            .first()
            .ok_or_else(|| SimpleError::new("empty statement"))?;

        let expression = self.read_expression(expression_node, this_scope, variables)?;

        let mut expression_type = expression.get_type();
        for mutator_node in &node.sub_rules[1..] {
            let mutator =
                self.read_mutator(mutator_node, &expression_type, this_scope, variables)?;
            expression_type = mutator.get_type();
        }

        todo!()
    }

    // _expression             = identifier | static_function_call | lambda | array_initialisation | string_literal | integer_literal;
    fn read_expression(
        &self,
        expression_node: &RuleNode<'_, '_>,
        this_scope: &Scope,
        variables: &mut VarStorage,
    ) -> Result<Expression, SimpleError> {
        match expression_node.rule_name {
            "identifier" => {
                let variable = expect_variable(variables, &expression_node.tokens_as_string())?;
                Ok(Expression::Variable(variable))
            }
            "static_function_call" => {
                let function_call =
                    self.read_static_function_call(expression_node, this_scope, variables)?;
                Ok(Expression::StaticFunctionCall(function_call))
            }
            "lambda" => {
                let scope_variables = variables.clone();
                // TODO add parameters to scope_variables
                let function_block =
                    self.read_function_block(expression_node, this_scope, scope_variables)?;
                Ok(Expression::FunctionBody(function_block))
            }
            "array_initialisation" => {
                let array = self.read_array_initialisation(expression_node, variables)?;
                Ok(Expression::Buffer(array))
            }
            "string_literal" => {
                let string_value = extract_string(expression_node);
                Ok(Expression::Literal(Literal::String(string_value)))
            }
            "integer_literal" => {
                let as_string = expression_node.tokens_as_string();
                let parse_result = as_string.parse::<i32>();

                match parse_result {
                    Ok(integer_value) => Ok(Expression::Literal(Literal::Number(integer_value))),
                    Err(err) => Err(SimpleError::new(format!(
                        "Could not parse integer literal \"{as_string}\": {err}"
                    ))),
                }
            }
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "node is not an _expression node: {unknown_rule}"
                )))
            }
        }
    }

    // _mutator = method_call | static_function_call | operator;
    fn read_mutator(
        &self,
        mutator_node: &RuleNode<'_, '_>,
        expression_type: &TypeRef,
        this_scope: &Scope,
        variables: &mut VarStorage,
    ) -> Result<Mutator, SimpleError> {
        match mutator_node.rule_name {
            "method_call" => Ok(Mutator::FunctionCall(self.read_method_call(
                expression_type,
                mutator_node,
                variables,
            )?)),
            "static_function_call" => Ok(Mutator::FunctionCall(self.read_static_function_call(
                mutator_node,
                this_scope,
                variables,
            )?)),
            "operator" => {
                todo!();
            }
            unknown_rule => {
                return Err(SimpleError::new(format!(
                    "unknown expression rule {unknown_rule}"
                )))
            }
        }
    }

    // static_function_call    = [ _scope_reference ], function_name, [ _argument_list ];
    // _scope_reference        = { scope_name };
    // _argument_list          = single_argument | { named_argument } ;
    fn read_static_function_call(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Scope,
        variables: &mut VarStorage,
    ) -> Result<FunctionCall, SimpleError> {
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
            &self.root_scope,
            this_scope,
        )?;

        let maybe_single_argument = node.find_node("single_argument");

        let mut arguments = HashMap::new();
        if let Some(single_argument) = maybe_single_argument {
            let arg_value = self.read_expression(single_argument, this_scope, variables)?;
            arguments.insert(Identifier::from(""), arg_value);
        } else {
            let named_arguments = node.find_nodes("named_argument");
            // named_argument = identifier, _expression;
            let parameter_name_node = node.expect_node("identifier")?;

            let expression_node = node
                .sub_rules
                .last()
                .ok_or_else(|| SimpleError::new("empty statement"))?;
            let arg_value = self.read_expression(expression_node, this_scope, variables)?;
            arguments.insert(parameter_name_node.as_identifier(), arg_value);
        }

        // TODO: explicit generic arguments
        Ok(FunctionCall {
            id: function_id,
            generic_arguments: HashMap::new(),
            arguments,
        })
    }

    // method_call             = [ type_ref ], function_name, [ _argument_list ];
    fn read_method_call(
        &self,
        expression_type: &TypeRef,
        node: &RuleNode<'_, '_>,
        variables: &mut VarStorage,
    ) -> Result<FunctionCall, SimpleError> {
        let type_ref_node = node.expect_node("type_ref");

        todo!()
    }

    fn read_array_initialisation(
        &self,
        node: &RuleNode<'_, '_>,
        variables: &mut VarStorage,
    ) -> Result<ArrayInitialisation, SimpleError> {
        todo!()
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
) -> Result<Rc<VariableDeclaration>, SimpleError> {
    variables
        .get(identifier)
        .map(Rc::clone)
        .ok_or_else(|| SimpleError::new(format!("expected identifier {identifier}")))
}
