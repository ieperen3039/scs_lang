use std::rc::Rc;

use simple_error::SimpleError;



use super::ast::GenericParameter;
use super::proto_ast::ProtoScope;
use super::{ast, proto_ast};

pub struct TypeResolver {
    pub proto_scope: ProtoScope,
}

impl TypeResolver {
    pub fn resolve_scope(&self, scope: &ProtoScope) -> Result<ast::Scope, SimpleError> {
        let mut new_scope = ast::Scope::new();

        for (identifier, type_def) in scope.types {
            let new_type_def = self.resolve_type(type_def, scope)?;
            new_scope.types.insert(identifier, Rc::from(new_type_def));
        }

        todo!() // todo subscopes
    }

    fn resolve_scope_reference<'a>(
        &'a self,
        scope: Vec<proto_ast::Identifier>,
        current: &'a ProtoScope,
    ) -> Result<&'a ProtoScope, SimpleError> {
        match scope.first() {
            None => Ok(current),
            Some(first) => {
                if let Some(top_scope) = self.proto_scope.scopes.get(first) {
                    Ok(top_scope)
                } else if let Some(local_scope) = current.scopes.get(first) {
                    self.resolve_local_scope_reference(&scope[1..], local_scope)
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
        scope: &[proto_ast::Identifier],
        current: &'a ProtoScope,
    ) -> Result<&'a ProtoScope, SimpleError> {
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

    fn resolve_type_name(
        &self,
        source: proto_ast::TypeName,
        generics: &[Rc<GenericParameter>],
        local_scope: &ProtoScope,
    ) -> Result<ast::TypeRef, SimpleError> {
        match source {
            proto_ast::TypeName::Defined(defined) => {
                Ok(ast::TypeRef::Defined(self.resolve_defined_name(defined, generics, local_scope)?))
            }
            proto_ast::TypeName::UnamedTuple(type_names) => {
                let mut type_refs = Vec::new();
                for ele in type_names {
                    let ele_ref = self.resolve_type_name(ele, generics, local_scope)?;
                    type_refs.push(ele_ref);
                }
                Ok(ast::TypeRef::UnamedTuple(type_refs))
            }
            proto_ast::TypeName::Array(type_name) => Ok(ast::TypeRef::Array(Box::from(
                self.resolve_type_name(*type_name, generics, local_scope)?,
            ))),
            proto_ast::TypeName::FunctionType(name) => {
                Ok(ast::TypeRef::Function(self.resolve_function_name(name, generics, local_scope)?))
            }
            proto_ast::TypeName::Void => Ok(ast::TypeRef::Void),
            proto_ast::TypeName::Generic(name) => {
                let found = generics.iter().find(|g| g.name == name).ok_or_else(|| {
                    SimpleError::new(format!(
                        "Can't find generic '{name}' among types {:?}",
                        generics
                    ))
                })?;

                Ok(ast::TypeRef::Generic(found.clone()))
            }
        }
    }

    fn resolve_type(
        &self,
        source: Rc<proto_ast::TypeDefinition>,
        local_scope: &ProtoScope,
    ) -> Result<ast::TypeDefinition, SimpleError> {
        let generic_parameters: Vec<Rc<GenericParameter>> = source
            .generic_parameters
            .into_iter()
            .map(|name| Rc::from(GenericParameter { name }))
            .collect();

        let sub_type = self.resolve_subtype(source.sub_type, &generic_parameters, local_scope)?;

        Ok(ast::TypeDefinition {
            name: source.name,
            generic_parameters,
            sub_type,
        })
    }

    fn resolve_subtype(
        &self,
        source: proto_ast::TypeSubType,
        generics: &[Rc<GenericParameter>],
        local_scope: &ProtoScope,
    ) -> Result<ast::TypeSubType, SimpleError> {
        match source {
            proto_ast::TypeSubType::Base {
                derived: optional_derived,
            } => {
                if let Some(derived) = optional_derived {
                    let type_ref = self.resolve_type_name(*derived, generics, local_scope)?;
                    Ok(ast::TypeSubType::Base {
                        derived: Some(Box::from(type_ref)),
                    })
                } else {
                    Ok(ast::TypeSubType::Base { derived: None })
                }
            }
            proto_ast::TypeSubType::Enum { values } => Ok(ast::TypeSubType::Enum { values }),
            proto_ast::TypeSubType::Variant { variants } => {
                let mut type_refs = Vec::new();
                for ele in variants {
                    let ele_ref = self.resolve_type_name(ele.type_name, generics, local_scope)?;
                    type_refs.push(ast::VariantValue {
                        name: ele.name,
                        value_type: ele_ref,
                    });
                }
                Ok(ast::TypeSubType::Variant {
                    variants: type_refs,
                })
            }
            proto_ast::TypeSubType::Tuple { elements } => {
                let mut type_refs = Vec::new();
                for ele in elements {
                    let ele_ref = self.resolve_type_name(ele, generics, local_scope)?;
                    type_refs.push(ele_ref);
                }
                Ok(ast::TypeSubType::Tuple {
                    elements: type_refs,
                })
            }
        }
    }

    fn resolve_defined_name(
        &self,
        source: proto_ast::DefinedName,
        generics: &[Rc<GenericParameter>],
        local_scope: &ProtoScope,
    ) -> Result<ast::DefinedTypeRef, SimpleError> {
        let mut generic_parameters = Vec::new();
        for ele in source.generic_parameters {
            let ele_ref = self.resolve_type_name(ele, generics, local_scope)?;
            generic_parameters.push(ele_ref);
        }

        let resolved_scope = self.resolve_scope_reference(source.scope, local_scope)?;
        let resolved_type = resolved_scope.types.get(&source.name)
            .ok_or_else(|| SimpleError::new(format!("Could not find '{}' in scope '{:?}'", source.name, resolved_scope.full_name)))?;

        Ok(ast::DefinedTypeRef {
            definition: resolved_type.clone(),
            generic_parameters,
        })
    }

    fn resolve_function_name(
        &self,
        name: proto_ast::FunctionName,
        generics: &[Rc<GenericParameter>],
        local_scope: &ProtoScope,
    ) -> Result<ast::FunctionRef, SimpleError> {
        todo!()
    }
}
