use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::ast::*;

type VarStorage = HashMap<Identifier, Rc<VariableDeclaration>>;

// function_block          = statement, { statement_separator, [ statement ] }
pub fn read_function_block(
    node: &RuleNode<'_, '_>,
    mut variables: VarStorage,
) -> Result<FunctionBlock, SimpleError> {
    let mut statements = Vec::new();

    for ele in node.sub_rules.chunks(2) {
        let statement_node = ele[0].expect_node("statement")?;
        let mut statement = read_statement(statement_node, &mut variables)?;

        if ele.len() == 2 {
            ele[1].expect_node("statement_separator")?;
        } else {
            // this is an implicit return
            let return_var = match statement.mutations.last().unwrap() {
                Mutation::FunctionCall(f) => f.function.body.return_var,
                Mutation::Assignment(_) => {
                    return Err(SimpleError::new(
                        "Result of an assignment is always void (did you forget a semicolon?",
                    ))
                }
            };

            variables.insert(return_var.name, return_var);

            statement.mutations.push(Mutation::Assignment(return_var))
        }

        statements.push(statement);
    }

    Ok(FunctionBlock {
        statements,
        return_var: expect_variable(&variables, "return")?,
    })
}

pub fn read_function_body(
    node: &RuleNode<'_, '_>,
    parameters: HashMap<Identifier, TypeRef>,
    return_var: Rc<VariableDeclaration>,
) -> Result<FunctionBlock, SimpleError> {
    debug_assert_eq!(node.rule_name, "function_block");
    let mut variables: VarStorage = HashMap::new();

    // the return statement is implemented as a variable
    // this variable will hold the return value when a function returns
    variables.insert(return_var.name, return_var);

    for (identifier, type_name) in parameters {
        variables.insert(
            identifier,
            Rc::from(VariableDeclaration {
                type_name,
                name: identifier,
            }),
        );
    }

    read_function_block(node, variables)
}

// statement               = _expression, { _mutator };
// _expression             = identifier | static_function_call | lambda | array_initialisation | string_literal | integer_literal;
// _mutator                = method_call | method_to_function_call | operator;
pub fn read_statement(
    node: &RuleNode<'_, '_>,
    variables: &mut VarStorage,
) -> Result<Statement, SimpleError> {
    debug_assert_eq!(node.rule_name, "statement");

    let expression_node = node
        .sub_rules
        .first()
        .ok_or_else(|| SimpleError::new("empty statement"))?;

    let expression = read_expression(expression_node, variables)?;

    for mutator_node in &node.sub_rules[1..] {}

    todo!()
}

fn read_expression(
    expression_node: &RuleNode<'_, '_>,
    variables: &mut VarStorage,
) -> Result<Expression, SimpleError> {
    match expression_node.rule_name {
        "identifier" => {
            let variable = expect_variable(variables, expression_node.tokens)?;
            Ok(Expression::Variable(variable))
        }
        "static_function_call" => {
            let function_call = read_function_call(expression_node, variables)?;
            Ok(Expression::StaticFunctionCall(function_call))
        }
        "lambda" => {
            let scope_variables = variables.clone();
            // TODO add parameters to scope_variables
            let function_block = read_function_block(expression_node, scope_variables)?;
            Ok(Expression::FunctionBlock(function_block))
        }
        "array_initialisation" => {
            let array = read_array_initialisation(expression_node, variables)?;
            Ok(Expression::Array(array))
        }
        "string_literal" => {
            let string_value = extract_string(expression_node);
            Ok(Expression::Literal(Literal::String(string_value)))
        }
        "integer_literal" => {
            let parse_result = expression_node.tokens.parse::<i32>();

            match parse_result {
                Ok(integer_value) => Ok(Expression::Literal(Literal::Number(integer_value))),
                Err(err) => Err(SimpleError::new(format!(
                    "Could not parse integer literal \"{}\": {}",
                    expression_node.tokens, err
                ))),
            }
        }
        unknown_rule => return Err(SimpleError::new(format!("unknown expression rule {unknown_rule}"))),
    }
}

fn extract_string(expression_node: &RuleNode<'_, '_>) -> Rc<str> {
    let slice = expression_node.tokens;
    // remove quotation marks
    Rc::from(&slice[1..slice.len() - 1])
}

fn read_array_initialisation(
    node: &RuleNode<'_, '_>,
    variables: &mut VarStorage,
) -> Result<Vec<Expression>, SimpleError> {
    todo!()
}

fn read_function_call(
    node: &RuleNode<'_, '_>,
    variables: &mut VarStorage,
) -> Result<FunctionCall, SimpleError> {
    todo!()
}

fn expect_variable(
    variables: &VarStorage,
    identifier: &str,
) -> Result<Rc<VariableDeclaration>, SimpleError> {
    variables
        .get(identifier)
        .map(Rc::to_owned)
        .ok_or_else(|| SimpleError::new(format!("expected identifier {identifier}")))
}
