use std::rc::Rc;

use simple_error::SimpleError;

use super::ast::*;

pub struct TypeResolver {
    pub root_scope: Scope,
}

impl TypeResolver {
    pub fn resolve_scope(&self, scope: &mut Scope) -> Result<Scope, SimpleError> {
        for (name, type_def) in &scope.types {
            **type_def = self.resolve_type(**type_def, scope)?;
        }

        for (subscope_name, subscope) in &mut scope.scopes {
            *subscope = self.resolve_scope(subscope)?;
        }

        todo!()
    }

    fn resolve_scope_reference<'a>(
        &'a self,
        scope_to_resolve: Vec<Identifier>,
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
                    return self.resolve_local_scope_reference(&scope[1..], child_scope);
                };
                Err(SimpleError::new(format!(
                    "Could not find '{first}' in scope {:?}",
                    current.full_name
                )))
            }
        }
    }

    fn resolve_type(
        &self,
        type_to_resolve: TypeDefinition,
        local_scope: &Scope,
    ) -> Result<TypeDefinition, SimpleError> {
        match type_to_resolve.sub_type {
            TypeSubType::Base { derived: None } => Ok(type_to_resolve),
            TypeSubType::Enum { .. } => Ok(type_to_resolve),
            TypeSubType::Base {
                derived: Some(derived),
            } => {
                let resolved_type_ref = self.resolve_type_ref(
                    *derived,
                    &type_to_resolve.generic_parameters,
                    local_scope,
                )?;

                Ok(TypeDefinition {
                    name: type_to_resolve.name,
                    generic_parameters: type_to_resolve.generic_parameters,
                    sub_type: TypeSubType::Base {
                        derived: Some(Box::from(resolved_type_ref)),
                    },
                })
            }
            TypeSubType::Variant { mut variants } => {
                for variant in &mut variants {
                    let resolved_value_type = self.resolve_type_ref(
                        variant.value_type,
                        &type_to_resolve.generic_parameters,
                        local_scope,
                    )?;
                    variant.value_type = resolved_value_type;
                }

                Ok(TypeDefinition {
                    name: type_to_resolve.name,
                    generic_parameters: type_to_resolve.generic_parameters,
                    sub_type: TypeSubType::Variant { variants },
                })
            }
            TypeSubType::Tuple { mut elements } => {
                for elt in &mut elements {
                    let resolved_value_type = self.resolve_type_ref(
                        *elt,
                        &type_to_resolve.generic_parameters,
                        local_scope,
                    )?;
                    *elt = resolved_value_type;
                }

                Ok(TypeDefinition {
                    name: type_to_resolve.name,
                    generic_parameters: type_to_resolve.generic_parameters,
                    sub_type: TypeSubType::Tuple { elements },
                })
            }
        }
    }

    fn resolve_type_ref(
        &self,
        type_to_resolve: TypeRef,
        generics: &[Rc<GenericParameter>],
        local_scope: &Scope,
    ) -> Result<TypeRef, SimpleError> {
        match type_to_resolve {
            TypeRef::UnresolvedName(name_to_resolve) => {
                let defined_type_ref =
                    self.resolve_defined_name(name_to_resolve, generics, local_scope)?;
                Ok(TypeRef::Defined(defined_type_ref))
            }
            _ => Ok(type_to_resolve),
        }
    }

    fn resolve_defined_name(
        &self,
        type_to_resolve: UnresolvedName,
        generics: &[Rc<GenericParameter>],
        local_scope: &Scope,
    ) -> Result<DefinedTypeRef, SimpleError> {
        let mut generic_parameters = Vec::new();
        for ele in type_to_resolve.generic_parameters {
            let ele_ref = self.resolve_type_ref(ele, generics, local_scope)?;
            generic_parameters.push(ele_ref);
        }

        let resolved_scope = self.resolve_scope_reference(type_to_resolve.scope, local_scope)?;
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
            definition: resolved_type.clone(),
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
