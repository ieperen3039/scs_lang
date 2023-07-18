use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::proto_ast::ProtoScope;
use super::{ast, proto_ast};

pub struct TypeResolver {
    pub proto_scope: ProtoScope,
}

impl TypeResolver {
    pub fn resolve_scope(&self, scope: &ProtoScope) -> Result<Scope, SimpleError> {
        let new_scope = Scope::new();

        for (identifier, type_def) in scope.types {
            new_scope
                .types
                .insert(identifier, self.resolve_type(type_def, scope))
        }

        todo!() // todo subscopes
    }

    fn resolve_scope_reference<'a>(
        &'a self,
        scope: Vec<Identifier>,
        current: &'a ProtoScope,
    ) -> Result<&'a ProtoScope, SimpleError> {
        match scope.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(top_scope) = self.proto_scope.scopes.get(first) {
                    Ok(top_scope)
                }
                if let Some(local_scope) = current.scopes.get(first) {
                    self.resolve_local_scope_reference(&scope[1..], local_scope)
                }
                Err(SimpleError::new(format!(
                    "Could not find {} in scope {}, nor in the global scope",
                    first, current.full_name
                )))
            }
        }
    }

    fn resolve_local_scope_reference<'a>(
        &'a self,
        scope: &[Identifier],
        current: &'a ProtoScope,
    ) -> Result<&'a ProtoScope, SimpleError> {
        match scope.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(child_scope) = current.scopes.get(first) {
                    return self.resolve_local_scope_reference(&scope[1..], child_scope);
                };
                Err(SimpleError::new(format!(
                    "Could not find {} in scope {}",
                    first, current.full_name
                )))
            }
        }
    }

    fn resolve_type_name(&self, source: proto_ast::TypeName, local_scope: &ProtoScope) -> Result<ast::TypeRef, SimpleError> {
        Ok(match source {
            proto_ast::TypeName::Defined(defined) => ast::TypeRef::Defined(self.resolve_defined_name(defined)?),
            proto_ast::TypeName::UnamedTuple(type_names) => {
                let mut type_refs = Vec::new();
                for ele in type_names {
                    let ele_ref = self.resolve_type_name(ele, local_scope)?;
                    type_refs.push(ele_ref);
                }
                ast::TypeRef::UnamedTuple(type_refs)
            },
            proto_ast::TypeName::Array(type_name) => ast::TypeRef::Array(self.resolve_type_name(type_name, local_scope)?),
            proto_ast::TypeName::FunctionType(name) => ast::TypeRef::Function(self.resolve_function_name(name)?),
            proto_ast::TypeName::Void => ast::TypeRef::Void,
        })
    }

    fn resolve_type(&self, source: Rc<proto_ast::TypeDefinition>, local_scope: &ProtoScope) -> Result<Rc<ast::TypeDefinition>, SimpleError> {
        Ok(ast::TypeDefinition {
            name: source.name,
            generic_parameters: source.generic_parameters,
            sub_type: source.sub_type,
        })
    }

    fn resolve_subtype(&self, source: proto_ast::TypeSubType, local_scope: &ProtoScope) -> Result<ast::TypeSubType, SimpleError> {
        Ok(match source {
            proto_ast::TypeSubType::Base { derived } => ast::TypeSubType::Base {
                derived: Box::new(self.resolve_type_name(derive, local_scoped)?),
            },
            proto_ast::TypeSubType::Enum { values } => ast::TypeSubType::Enum { values },
            proto_ast::TypeSubType::Variant { variants } => ast::TypeSubType::Variant { variants },
            proto_ast::TypeSubType::Tuple { elements } => {
                let mut type_refs = Vec::new();
                for ele in elements {
                    let ele_ref = self.resolve_type_name(ele, local_scope)?;
                    type_refs.push(ele_ref);
                }
                ast::TypeSubType::Tuple {
                    elements: type_refs
                }
            },
        })
    }

    fn resolve_defined_name(&self, defined: proto_ast::DefinedName) -> Result<ast::DefinedTypeRef, SimpleError> {
        todo!()
    }

    fn resolve_function_name(&self, name: proto_ast::FunctionName) -> Result<ast::FunctionRef, SimpleError> {
        todo!()
    }
}
