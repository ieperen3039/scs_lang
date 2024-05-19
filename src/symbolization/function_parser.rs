use std::{collections::HashMap, rc::Rc};

use simple_error::{SimpleError, SimpleResult};

use crate::parsing::rule_nodes::RuleNode;

use super::{ast::*, type_resolver, type_collector::TypeCollector};

type VarStorage = HashMap<Identifier, Rc<VariableDeclaration>>;

pub struct FunctionParser<'s, 'tc> {
    pub root_scope: &'s Namespace,
    pub functions: HashMap<NumericFunctionIdentifier, FunctionDeclaration>,
    pub type_collector: &'tc TypeCollector,
}

impl FunctionParser<'_, '_> {
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
                        variables.insert(return_var.name.clone(), return_var.clone());
                        return_var
                    }
                };

                statement.mutations.push(FunctionExpression::Assignment(return_var))
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

    // statement               = _expression, { _mutator };
    pub fn read_statement(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<Statement> {
        debug_assert_eq!(node.rule_name, "statement");

        let expression_node = node
            .sub_rules
            .first()
            .ok_or_else(|| SimpleError::new("empty statement"))?;

        let expression = self.read_expression(expression_node, this_scope, variables)?;

        let mut expression_type = expression.get_type(&self.functions);

        let mut mutations = Vec::new();
        for mutator_node in &node.sub_rules[1..] {
            let mutator =
                self.read_mutator(mutator_node, &expression_type, this_scope, variables)?;
            expression_type = mutator.get_type(&self.functions);
            mutations.push(mutator);
        }

        Ok(Statement {
            base_element: expression,
            mutations,
        })
    }

    // _expression             = identifier | static_function_call | lambda | array_initialisation | string_literal | integer_literal;
    fn read_expression(
        &self,
        expression_node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<ValueExpression> {
        match expression_node.rule_name {
            "identifier" => {
                let variable = expect_variable(variables, &expression_node.tokens_as_string())?;
                Ok(ValueExpression::Variable(variable))
            }
            "lambda" => {
                let scope_variables = variables.clone();
                // TODO add parameters to scope_variables
                let function_block =
                    self.read_function_block(expression_node, this_scope, scope_variables)?;
                Ok(ValueExpression::FunctionBody(function_block))
            }
            "tuple_initialisation" => {
                let tuple = self.read_tuple_initialisation(expression_node, variables)?;
                Ok(ValueExpression::Tuple(tuple))
            }
            "string_literal" => {
                let string_value = extract_string(expression_node);
                Ok(ValueExpression::Literal(Literal::String(string_value)))
            }
            "integer_literal" => {
                let as_string = expression_node.tokens_as_string();
                let parse_result = as_string.parse::<i32>();

                match parse_result {
                    Ok(integer_value) => Ok(ValueExpression::Literal(Literal::Number(integer_value))),
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
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionExpression> {
        match mutator_node.rule_name {
            "method_call" => Ok(FunctionExpression::FunctionCall(self.read_method_call(
                expression_type,
                mutator_node,
                this_scope,
                variables,
            )?)),
            "static_function_call" => Ok(FunctionExpression::FunctionCall(self.read_static_function_call(
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
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionCall> {
        assert_eq!(node.rule_name, "static_function_call");
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

        let arguments = self.read_arguments(node, this_scope, variables)?;

        // TODO: explicit generic arguments
        Ok(FunctionCall {
            id: function_id,
            arguments,
        })
    }

    fn read_arguments(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut HashMap<Rc<str>, Rc<VariableDeclaration>>,
    ) -> SimpleResult<HashMap<Rc<str>, ValueExpression>> {
        let mut arguments = HashMap::new();
        if let Some(single_argument) = node.find_node("single_argument") {
            let arg_value = self.read_expression(single_argument, this_scope, variables)?;
            arguments.insert(Identifier::from(""), arg_value);
        } else {
            let named_arguments = node.find_nodes("named_argument");
            for arg in named_arguments {
                // named_argument = identifier, _expression;
                let parameter_name_node = arg.expect_node("identifier")?;

                let expression_node = arg
                    .sub_rules
                    .last()
                    .ok_or_else(|| SimpleError::new("empty statement"))?;
                let arg_value = self.read_expression(expression_node, this_scope, variables)?;
                arguments.insert(parameter_name_node.as_identifier(), arg_value);
            }
        }
        Ok(arguments)
    }

    // method_call             = [ type_ref ], function_name, [ _argument_list ];
    // _argument_list          = single_argument | ( named_argument, { _, ",", _, named_argument } );
    fn read_method_call(
        &self,
        expression_type: &TypeRef,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
        variables: &mut VarStorage,
    ) -> SimpleResult<FunctionCall> {

        let obj_type = if let Some(type_ref_node) = node.find_node("type_ref") {
            self.type_collector.read_type_ref(type_ref_node)?
        } else {
            expression_type.clone()
        };

        let function_name_node = node.expect_node("function_name")?;
        let function_id = this_scope.functions.get(&function_name_node.as_identifier())
            .ok_or_else(|| SimpleError::new(format!("Unknown function {}", function_name_node.as_identifier())))?;

        let arguments = self.read_arguments(node, this_scope, variables)?;
        
        Ok(FunctionCall { id: *function_id, arguments })
    }

    fn read_tuple_initialisation(
        &self,
        node: &RuleNode<'_, '_>,
        variables: &mut VarStorage,
    ) -> SimpleResult<TupleInitialisation> {
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
) -> SimpleResult<Rc<VariableDeclaration>> {
    variables
        .get(identifier)
        .map(Rc::clone)
        .ok_or_else(|| SimpleError::new(format!("expected identifier {identifier}")))
}
