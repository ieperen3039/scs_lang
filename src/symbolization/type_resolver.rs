use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::{parsing::rule_nodes::RuleNode, symbolization::ast::*};

use super::proto_ast::ProtoScope;

pub struct TypeResolver {
    pub aliasses: HashMap<Identifier, Identifier>,
    pub proto_scope: ProtoScope,
}

impl TypeResolver {
    pub fn resolve_scope(&self, scope : ProtoScope) -> Result<Scope, SimpleError> {
        todo!()
    }
}
