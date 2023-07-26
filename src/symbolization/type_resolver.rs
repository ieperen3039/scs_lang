use std::{borrow::BorrowMut, collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use super::ast::*;

pub struct TypeResolver<'inc> {
    pub external_scope: &'inc Scope,
    pub root_scope: Scope,
}

pub fn resolve_scope(
    external_scope: &Scope,
    scope_to_resolve: Scope,
) -> Result<Scope, SimpleError> {
    let resolver = TypeResolver {
        external_scope,
        root_scope: scope_to_resolve,
    };

    resolver.resolve_scope(&resolver.root_scope)
}

impl<'inc> TypeResolver<'inc> {
    fn resolve_scope(&self, scope: &Scope) -> Result<Scope, SimpleError> {
        let mut new_scope = Scope {
            full_name: scope.full_name.clone(),
            scopes: HashMap::new(),
            types: HashMap::new(),
            functions: scope.functions.clone(),
        };

        for (_type_name, type_def) in &scope.types {
            let mut new_type_def: TypeDefinition = type_def.clone();
            // we remove this type from the scope, in order to allow passing the scope to resolve_type
            self.resolve_type(&mut new_type_def, &scope)?;
            new_scope.add_type(new_type_def);
        }

        for (_subscope_name, subscope) in &scope.scopes {
            let new_sub_scope = self.resolve_scope(subscope)?;
            new_scope.add_sub_scope(new_sub_scope);
        }

        Ok(new_scope)
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
                } else if current.full_name.contains(first) {
                    let local_scope = self.resolve_partial_scope_name(current, first);
                    self.resolve_local_scope_reference(&scope_to_resolve[1..], local_scope)
                } else if let Some(local_scope) = current.scopes.get(first) {
                    self.resolve_local_scope_reference(&scope_to_resolve[1..], local_scope)
                } else if let Some(top_scope) = self.external_scope.scopes.get(first) {
                    Ok(top_scope)
                } else {
                    Err(SimpleError::new(format!(
                        "Could not find '{}' in scope {:?}, nor in the global scope",
                        first, current.full_name
                    )))
                }
            }
        }
    }

    fn resolve_partial_scope_name(&self, scope: &Scope, first_of_partial: &Rc<str>) -> &Scope {
        let mut local_scope = self.external_scope;

        for name in &scope.full_name {
            if name == first_of_partial {
                break;
            }

            local_scope = local_scope.scopes.get(name).unwrap();
        }

        local_scope
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
                } else {
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
            TypeSubType::Base { derived: None } => {}
            TypeSubType::Enum { .. } => {}
            TypeSubType::Base {
                derived: Some(to_resolve),
            } => {
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
                    self.resolve_type_ref(elt, &type_to_resolve.generic_parameters, local_scope)?;
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
            _ => {}
        }
        Ok(())
    }

    fn resolve_defined_name(
        &self,
        type_to_resolve: &UnresolvedName,
        generics: &[Rc<GenericParameter>],
        local_scope: &Scope,
    ) -> Result<DefinedRef, SimpleError> {
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

        Ok(DefinedRef {
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
