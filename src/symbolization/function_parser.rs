use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::ast::*;

type VarStorage = HashMap<Identifier, Rc<VariableDeclaration>>;

// function_block          = statement, { statement_separator, [ statement ] }
pub fn read_function_block(
    node: &RuleNode<'_, '_>,
    parameters: HashMap<Identifier, TypeRef>,
    return_var: Rc<VariableDeclaration>,
) -> Result<FunctionBlock, SimpleError> {
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

    let mut statements = Vec::new();

    for ele in node.sub_rules.chunks(2) {
        let statement_node = ele[0].expect_node("statement")?;
        let mut statement = read_statement(statement_node, &mut variables)?;

        if ele.len() == 2 {
            ele[1].expect_node("statement_separator")?;
        } else {
            // this is an implicit return
            statement.mutations.push(Mutation::Assignment(return_var))
        }

        statements.push(statement);
    }

    Ok(FunctionBlock { statements })
}

// statement               = _expression, { _mutator };
// _expression             = identifier | static_function_call | lambda | array_initialisation | _literal;
// _mutator                = method_call | method_to_function_call | operator;
pub fn read_statement(
    node: &RuleNode<'_, '_>,
    variables: &mut VarStorage,
) -> Result<Statement, SimpleError> {
    todo!()
}
