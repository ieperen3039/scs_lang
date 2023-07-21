use std::{rc::Rc, borrow::BorrowMut};

use simple_error::SimpleError;

use super::ast::*;

pub struct TypeResolver {
    pub root_scope: Scope,
}

impl TypeResolver {
    pub fn resolve_scope(&self, scope: &mut Scope) -> Result<(), SimpleError> {
        for (_type_name, type_def) in &mut scope.types {
            self.resolve_type(type_def, scope)?;
        }

        for (_subscope_name, subscope) in &mut scope.scopes {
            self.resolve_scope(subscope)?;
        }

        Ok(())
    }

    fn resolve_scope_reference<'a>(
        &'a self,
        scope_to_resolve: &[Identifier],
        current: &'a Scope,
    ) -> Result<&'a Scope, SimpleError> {
        match scope_to_resolve.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(top_scope) = self.root_scope.scopes.get(first) {
                    Ok(top_scope)
                } else if let Some(local_scope) = current.scopes.get(first) {
                    self.resolve_local_scope_reference(&scope_to_resolve[1..], local_scope)
                } else {
                    Err(SimpleError::new(format!(
                        "Could not find '{}' in scope {:?}, nor in the global scope",
                        first, current.full_name
                    )))
                }
            }
        }
    }

    fn resolve_local_scope_reference<'a>(
        &'a self,
        scope: &[Identifier],
        current: &'a Scope,
    ) -> Result<&'a Scope, SimpleError> {
        match scope.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(child_scope) = current.scopes.get(first) {
                    self.resolve_local_scope_reference(&scope[1..], child_scope)
                }
                else
                {
                    Err(SimpleError::new(format!(
                        "Could not find '{first}' in scope {:?}",
                        current.full_name
                    )))
                }
            }
        }
    }

    fn resolve_type(
        &self,
        type_to_resolve: &mut TypeDefinition,
        local_scope: &Scope,
    ) -> Result<(), SimpleError> {
        match &mut type_to_resolve.sub_type {
            TypeSubType::Base { derived: None } => {},
            TypeSubType::Enum { .. } => {},
            TypeSubType::Base { derived: Some(to_resolve), } => {
                self.resolve_type_ref(
                    to_resolve.borrow_mut(),
                    &type_to_resolve.generic_parameters,
                    local_scope,
                )?;
            }
            TypeSubType::Variant { variants } => {
                for variant in variants {
                    self.resolve_type_ref(
                        &mut variant.value_type,
                        &type_to_resolve.generic_parameters,
                        local_scope,
                    )?;
                }
            }
            TypeSubType::Tuple { elements } => {
                for elt in elements {
                    self.resolve_type_ref(
                        elt,
                        &type_to_resolve.generic_parameters,
                        local_scope,
                    )?;
                }
            }
        }

        Ok(())
    }

    fn resolve_type_ref(
        &self,
        type_to_resolve: &mut TypeRef,
        generics: &[Rc<GenericParameter>],
        local_scope: &Scope,
    ) -> Result<(), SimpleError> {
        match type_to_resolve {
            TypeRef::UnresolvedName(name_to_resolve) => {
                let defined_type_ref =
                    self.resolve_defined_name(name_to_resolve, generics, local_scope)?;
                *type_to_resolve = TypeRef::Defined(defined_type_ref);
            }
            _ => {},
        }
        Ok(())
    }

    fn resolve_defined_name(
        &self,
        type_to_resolve: &UnresolvedName,
        generics: &[Rc<GenericParameter>],
        local_scope: &Scope,
    ) -> Result<DefinedTypeRef, SimpleError> {
        let mut generic_parameters = Vec::new();
        for ele in &type_to_resolve.generic_parameters {
            let mut ele_ref = ele.clone();
            self.resolve_type_ref(&mut ele_ref, generics, local_scope)?;
            generic_parameters.push(ele_ref);
        }

        let resolved_scope = self.resolve_scope_reference(&type_to_resolve.scope, local_scope)?;
        let resolved_type = resolved_scope
            .types
            .get(&type_to_resolve.name)
            .ok_or_else(|| {
                SimpleError::new(format!(
                    "Could not find '{}' in scope '{:?}'",
                    type_to_resolve.name, resolved_scope.full_name
                ))
            })?;

        Ok(DefinedTypeRef {
            id: resolved_type.id,
            generic_parameters,
        })
    }

    fn resolve_function_name(
        &self,
        name: FunctionCall,
        generics: &[Rc<GenericParameter>],
        local_scope: &Scope,
    ) -> Result<Rc<FunctionDefinition>, SimpleError> {
        todo!()
    }
}
