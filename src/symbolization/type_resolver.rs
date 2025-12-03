use std::{borrow::BorrowMut, rc::Rc};

use super::{
    ast::*,
    semantic_result::{SemanticError, SemanticResult},
};

pub struct TypeResolver<'rns> {
    pub root_namespace: &'rns Namespace,
}

pub fn resolve_type_definitions(
    types_to_resolve: Vec<TypeDefinition>,
    root_namespace: &Namespace,
) -> SemanticResult<Vec<TypeDefinition>> {
    let resolver = TypeResolver { root_namespace };

    resolver.resolve_types(types_to_resolve)
}

pub fn resolve_type_name(
    mut type_to_resolve: TypeRef,
    root_namespace: &Namespace,
    local_namespace: &Namespace,
) -> SemanticResult<TypeRef> {
    let resolver = TypeResolver { root_namespace };

    resolver.resolve_type_ref(&mut type_to_resolve, local_namespace)?;

    Ok(type_to_resolve)
}

pub fn resolve_function_name(
    name: Identifier,
    declared_namespace: &[Identifier],
    root_namespace: &Namespace,
    local_namespace: &Namespace,
) -> SemanticResult<FunctionDeclaration> {
    let resolver = TypeResolver { root_namespace };

    let resolved_namespace =
        resolver.resolve_namespace_reference(declared_namespace, local_namespace)?;

    resolved_namespace
        .functions
        .get(&name)
        .map(FunctionDeclaration::clone)
        .ok_or_else(|| SemanticError::SymbolNotFoundInScope {
            kind: "function",
            symbol: name,
            scope: resolved_namespace.full_name.clone(),
        })
}

impl<'rns> TypeResolver<'rns> {
    fn resolve_types(
        &self,
        types_to_resolve: Vec<TypeDefinition>,
    ) -> SemanticResult<Vec<TypeDefinition>> {
        let mut new_types = Vec::new();

        for mut type_def in types_to_resolve {
            // we remove this type from the namespace, in order to allow passing the namespace to resolve_type
            self.resolve_type(&mut type_def, self.root_namespace)?;
            new_types.push(type_def);
        }

        Ok(new_types)
    }

    fn resolve_namespace_reference(
        &'rns self,
        namespace_to_resolve: &[Identifier],
        current: &'rns Namespace,
    ) -> SemanticResult<&'rns Namespace> {
        match namespace_to_resolve.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(top_namespace) = self.root_namespace.namespaces.get(first) {
                    Ok(top_namespace)
                } else if current.full_name.contains(first) {
                    let local_namespace = self.resolve_partial_namespace_name(current, first);
                    local_namespace.get_namespace(&namespace_to_resolve[1..])
                } else if let Some(local_namespace) = current.namespaces.get(first) {
                    local_namespace.get_namespace(&namespace_to_resolve[1..])
                } else {
                    Err(SemanticError::SymbolNotFoundInScope {
                        kind: "namespace",
                        symbol: first.clone(),
                        scope: current.full_name.clone(),
                    })
                }
            },
        }
    }

    fn resolve_partial_namespace_name(
        &self,
        namespace: &Namespace,
        first_of_partial: &Rc<str>,
    ) -> &Namespace {
        let mut local_namespace = self.root_namespace;

        for name in &namespace.full_name {
            if name == first_of_partial {
                break;
            }

            local_namespace = local_namespace.namespaces.get(name).unwrap();
        }

        local_namespace
    }

    fn resolve_type(
        &self,
        type_to_resolve: &mut TypeDefinition,
        local_namespace: &Namespace,
    ) -> SemanticResult<()> {
        match &mut type_to_resolve.type_class {
            TypeClass::Base { derived: None } => {},
            TypeClass::Enum { .. } => {},
            TypeClass::Base {
                derived: Some(to_resolve),
            } => {
                self.resolve_type_ref(to_resolve.borrow_mut(), local_namespace)?;
            },
            TypeClass::Variant { variants } => {
                for variant in variants {
                    self.resolve_type_ref(&mut variant.value_type, local_namespace)?;
                }
            },
            TypeClass::Tuple { elements } => {
                for elt in elements {
                    self.resolve_type_ref(elt, local_namespace)?;
                }
            },
        }

        Ok(())
    }

    fn resolve_type_ref(
        &self,
        type_to_resolve: &mut TypeRef,
        local_namespace: &Namespace,
    ) -> SemanticResult<()> {
        match type_to_resolve {
            TypeRef::UnresolvedName(name_to_resolve) => {
                let defined_type_ref =
                    self.resolve_defined_name(name_to_resolve, local_namespace)?;
                *type_to_resolve = TypeRef::Defined(defined_type_ref);
            },
            _ => {},
        }
        Ok(())
    }

    fn resolve_defined_name(
        &self,
        type_to_resolve: &UnresolvedName,
        local_namespace: &Namespace,
    ) -> SemanticResult<DefinedRef> {
        let resolved_namespace =
            self.resolve_namespace_reference(&type_to_resolve.scope, local_namespace)?;
        let &resolved_type = resolved_namespace
            .types
            .get(&type_to_resolve.name)
            .ok_or_else(|| SemanticError::SymbolNotFoundInScope {
                kind: "type",
                symbol: type_to_resolve.name.clone(),
                scope: resolved_namespace.full_name.clone(),
            })?;

        Ok(DefinedRef {
            id: resolved_type,
            generics: Vec::new(),
        })
    }
}
