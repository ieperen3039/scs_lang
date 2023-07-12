use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::{parsing::rule_nodes::RuleNode, symbolization::ast::*};

use super::proto_ast::ProtoScope;

pub struct TypeResolver {
    pub proto_scope: ProtoScope,
}

impl TypeResolver {
    pub fn resolve_scope(&self, scope : ProtoScope) -> Result<Scope, SimpleError> {
        let new_scope = Scope::new();

        for (identifier, type_def) in scope.types {
            new_scope.types.insert(identifier, self.resolve_type(type_def))
        }

        todo!()
    }
    
    fn resolve_scope_reference<'a>(&'a self, scope : Vec<Identifier>, current : ProtoScope) -> Result<&'a ProtoScope, SimpleError> {
        let derived_scope = node.find_nodes("scope_name");
        let mut search_scope = &self.scope;
        for ele in derived_scope {
            debug_assert_eq!(ele.rule_name, "scope_name");
            search_scope = search_scope
                .scopes
                .get(ele.tokens)
                .ok_or_else(|| SimpleError::new(format!("unknown scope {}", ele.tokens)))?;
        }
        Ok(search_scope)
    }
}
