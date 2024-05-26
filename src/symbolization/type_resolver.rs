use std::{borrow::BorrowMut, rc::Rc};

use super::{ast::*, semantic_result::{SemanticError, SemanticResult}};

pub struct TypeResolver<'ext, 'int> {
    pub external_scope: &'ext Namespace,
    pub root_scope: &'int Namespace,
}

pub fn resolve_type_definitions(
    types_to_resolve: Vec<TypeDefinition>,
    external_scope: &Namespace,
    internal_scope: &Namespace,
) -> SemanticResult<Vec<TypeDefinition>> {
    let resolver = TypeResolver {
        external_scope,
        root_scope: internal_scope,
    };

    resolver.resolve_types(types_to_resolve)
}

pub fn resolve_type_name(
    type_to_resolve: &UnresolvedName,
    root_scope: &Namespace,
    local_scope: &Namespace,
) -> SemanticResult<TypeId> {
    let resolver = TypeResolver {
        external_scope: root_scope,
        root_scope,
    };

    let resolved_scope = resolver.resolve_scope_reference(&type_to_resolve.scope, local_scope)?;

    resolved_scope
        .types
        .get(&type_to_resolve.name)
        .map(u32::clone)
        .ok_or_else(|| {
            SemanticError::SymbolNotFoundInScope { kind: "type", symbol: type_to_resolve.name.clone(), scope: resolved_scope.full_name.clone() }
        })
}

pub fn resolve_function_name<'s>(
    name: Identifier,
    scope: &[Identifier],
    root_scope: &Namespace,
    local_scope: &'s Namespace,
) -> SemanticResult<FunctionId> {
    let resolver = TypeResolver {
        external_scope: root_scope,
        root_scope,
    };

    let resolved_scope = resolver.resolve_scope_reference(scope, local_scope)?;

    resolved_scope
        .functions
        .get(&name)
        .map(u32::clone)
        .ok_or_else(|| {
            SemanticError::SymbolNotFoundInScope { kind: "function", symbol: name, scope: resolved_scope.full_name.clone() }
        })
}

impl<'ext, 'int> TypeResolver<'ext, 'int> {
    fn resolve_types(
        &self,
        types_to_resolve: Vec<TypeDefinition>,
    ) -> SemanticResult<Vec<TypeDefinition>> {
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
        current: &'a Namespace,
    ) -> SemanticResult<&'a Namespace> {
        match scope_to_resolve.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(top_scope) = self.root_scope.namespaces.get(first) {
                    Ok(top_scope)
                } else if current.full_name.contains(first) {
                    let local_scope = self.resolve_partial_scope_name(current, first);
                    self.resolve_local_scope_reference(&scope_to_resolve[1..], local_scope)
                } else if let Some(local_scope) = current.namespaces.get(first) {
                    self.resolve_local_scope_reference(&scope_to_resolve[1..], local_scope)
                } else if let Some(top_scope) = self.external_scope.namespaces.get(first) {
                    Ok(top_scope)
                } else {
                    Err(SemanticError::SymbolNotFoundInScope { kind: "namespace", symbol: first.clone(), scope: current.full_name.clone() })
                }
            },
        }
    }

    fn resolve_partial_scope_name(
        &self,
        scope: &Namespace,
        first_of_partial: &Rc<str>,
    ) -> &Namespace {
        let mut local_scope = self.external_scope;

        for name in &scope.full_name {
            if name == first_of_partial {
                break;
            }

            local_scope = local_scope.namespaces.get(name).unwrap();
        }

        local_scope
    }

    fn resolve_local_scope_reference<'a>(
        &'a self,
        scope: &[Identifier],
        current: &'a Namespace,
    ) -> SemanticResult<&'a Namespace> {
        match scope.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(child_scope) = current.namespaces.get(first) {
                    self.resolve_local_scope_reference(&scope[1..], child_scope)
                } else {
                    Err(SemanticError::SymbolNotFoundInScope { kind: "namespace", symbol: first.clone(), scope: current.full_name.clone() })
                }
            },
        }
    }

    fn resolve_type(
        &self,
        type_to_resolve: &mut TypeDefinition,
        local_scope: &Namespace,
    ) -> SemanticResult<()> {
        match &mut type_to_resolve.type_class {
            TypeClass::Base { derived: None } => {},
            TypeClass::Enum { .. } => {},
            TypeClass::Base {
                derived: Some(to_resolve),
            } => {
                self.resolve_type_ref(to_resolve.borrow_mut(), local_scope)?;
            },
            TypeClass::Variant { variants } => {
                for variant in variants {
                    self.resolve_type_ref(&mut variant.value_type, local_scope)?;
                }
            },
            TypeClass::Tuple { elements } => {
                for elt in elements {
                    self.resolve_type_ref(elt, local_scope)?;
                }
            },
        }

        Ok(())
    }

    fn resolve_type_ref(
        &self,
        type_to_resolve: &mut TypeRef,
        local_scope: &Namespace,
    ) -> SemanticResult<()> {
        match type_to_resolve {
            TypeRef::UnresolvedName(name_to_resolve) => {
                let defined_type_ref = self.resolve_defined_name(name_to_resolve, local_scope)?;
                *type_to_resolve = TypeRef::Defined(defined_type_ref);
            },
            _ => {},
        }
        Ok(())
    }

    fn resolve_defined_name(
        &self,
        type_to_resolve: &UnresolvedName,
        local_scope: &Namespace,
    ) -> SemanticResult<DefinedRef> {
        let resolved_scope = self.resolve_scope_reference(&type_to_resolve.scope, local_scope)?;
        let &resolved_type = resolved_scope
            .types
            .get(&type_to_resolve.name)
            .ok_or_else(|| {
                SemanticError::SymbolNotFoundInScope { kind: "type", symbol: type_to_resolve.name.clone(), scope: resolved_scope.full_name.clone() }
            })?;

        Ok(DefinedRef { id: resolved_type })
    }
}
