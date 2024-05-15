use std::{borrow::BorrowMut, rc::Rc};

use simple_error::{SimpleError, SimpleResult};

use super::ast::*;

pub struct TypeResolver<'ext, 'int> {
    pub external_scope: &'ext Scope,
    pub root_scope: &'int Scope,
}

pub fn resolve_type_definitions(
    types_to_resolve: Vec<TypeDefinition>,
    external_scope: &Scope,
    internal_scope: &Scope,
) -> SimpleResult<Vec<TypeDefinition>> {
    let resolver = TypeResolver {
        external_scope,
        root_scope: internal_scope,
    };

    resolver.resolve_types(types_to_resolve)
}

pub fn resolve_type_name(
    type_to_resolve: &UnresolvedName,
    root_scope: &Scope,
    local_scope: &Scope,
) -> SimpleResult<NumericTypeIdentifier> {
    let resolver = TypeResolver {
        external_scope : root_scope,
        root_scope,
    };

    let resolved_scope = resolver.resolve_scope_reference(&type_to_resolve.scope, local_scope)?;
    
    resolved_scope
        .types
        .get(&type_to_resolve.name)
        .map(u32::clone)
        .ok_or_else(|| {
            SimpleError::new(format!(
                "Could not find '{}' in scope '{:?}'",
                type_to_resolve.name, resolved_scope.full_name
            ))
        })
}

pub fn resolve_function_name<'s>(
    name: Identifier,
    scope: &[Identifier],
    root_scope: &Scope,
    local_scope: &'s Scope,
) -> SimpleResult<NumericFunctionIdentifier> {
    let resolver = TypeResolver {
        external_scope : root_scope,
        root_scope,
    };
    
    let resolved_scope = resolver.resolve_scope_reference(scope, local_scope)?;
    
    resolved_scope
        .functions
        .get(&name)
        .map(u32::clone)
        .ok_or_else(|| {
            SimpleError::new(format!(
                "Could not find '{}' in scope '{:?}'",
                name, resolved_scope.full_name
            ))
        })
}

impl<'ext, 'int> TypeResolver<'ext, 'int> {
    fn resolve_types(&self, types_to_resolve: Vec<TypeDefinition>) -> SimpleResult<Vec<TypeDefinition>> {
        let mut new_types = Vec::new();

        for mut type_def in types_to_resolve {
            // we remove this type from the scope, in order to allow passing the scope to resolve_type
            self.resolve_type(&mut type_def, self.root_scope)?;
            new_types.push(type_def);
        }

        Ok(new_types)
    }

    fn resolve_scope_reference<'a>(
        &'a self,
        scope_to_resolve: &[Identifier],
        current: &'a Scope,
    ) -> SimpleResult<&'a Scope> {
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
    ) -> SimpleResult<&'a Scope> {
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
    ) -> SimpleResult<()> {
        match &mut type_to_resolve.type_class {
            TypeClass::Base { derived: None } => {}
            TypeClass::Enum { .. } => {}
            TypeClass::Base {
                derived: Some(to_resolve),
            } => {
                self.resolve_type_ref(
                    to_resolve.borrow_mut(),
                    local_scope,
                )?;
            }
            TypeClass::Variant { variants } => {
                for variant in variants {
                    self.resolve_type_ref(
                        &mut variant.value_type,
                        local_scope,
                    )?;
                }
            }
            TypeClass::Tuple { elements } => {
                for elt in elements {
                    self.resolve_type_ref(elt, local_scope)?;
                }
            }
        }

        Ok(())
    }

    fn resolve_type_ref(
        &self,
        type_to_resolve: &mut TypeRef,
        local_scope: &Scope,
    ) -> SimpleResult<()> {
        match type_to_resolve {
            TypeRef::UnresolvedName(name_to_resolve) => {
                let defined_type_ref =
                    self.resolve_defined_name(name_to_resolve, local_scope)?;
                *type_to_resolve = TypeRef::Defined(defined_type_ref);
            }
            _ => {}
        }
        Ok(())
    }

    fn resolve_defined_name(
        &self,
        type_to_resolve: &UnresolvedName,
        local_scope: &Scope,
    ) -> SimpleResult<DefinedRef> {
        let resolved_scope = self.resolve_scope_reference(&type_to_resolve.scope, local_scope)?;
        let &resolved_type = resolved_scope
            .types
            .get(&type_to_resolve.name)
            .ok_or_else(|| {
                SimpleError::new(format!(
                    "Could not find '{}' in scope '{:?}'",
                    type_to_resolve.name, resolved_scope.full_name
                ))
            })?;

        Ok(DefinedRef {
            id: resolved_type,
        })
    }
}
