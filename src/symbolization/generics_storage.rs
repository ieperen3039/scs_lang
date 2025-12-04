use crate::symbolization::ast::*;
use crate::symbolization::semantic_result::{SemanticError, SemanticResult};
use std::collections::HashMap;

pub struct GenStorage {
    rigid_parameters: Vec<Identifier>,
    arguments: HashMap<Identifier, Option<TypeRef>>,
}

impl GenStorage {
    pub fn new() -> Self {
        Self {
            rigid_parameters: Vec::new(),
            arguments: HashMap::new(),
        }
    }

    pub fn from(other: &GenStorage) -> Self {
        Self {
            rigid_parameters: other.rigid_parameters.clone(),
            // generic arguments never transfer scopes
            arguments: HashMap::new(),
        }
    }

    pub fn set_substitution(&mut self, name: &Identifier, substitution: TypeRef) {
        // we avoid needing to clone `name` again; the key must already be here anyway
        let entry = self.arguments.get_mut(name).expect(
            "generics may only be resolved if they have been added with set_arguments first",
        );
        *entry = Some(substitution);
    }

    pub fn set_arguments<I: Iterator<Item = Identifier>>(&mut self, names: I) {
        self.arguments.clear();
        for name in names {
            self.arguments.insert(name, None);
        }
    }

    pub fn set_parameters<I: Iterator<Item = Identifier>>(&mut self, names: I) {
        self.rigid_parameters.clear();
        for name in names {
            self.rigid_parameters.push(name);
        }
    }

    pub fn get_resolved_type(&self, name: &Identifier) -> Option<&TypeRef> {
        self.arguments.get(name).and_then(|t| t.as_ref())
    }

    pub fn is_resolved(&self, name: &Identifier) -> bool {
        self.arguments
            .get(name)
            .expect("given name must have been added first")
            .is_some()
    }

    pub fn get_all_unresolved(&self) -> Vec<&Identifier> {
        self.arguments
            .iter()
            .filter(|(_, t)| t.is_none())
            .map(|(k, _)| k)
            .collect()
    }

    pub fn resolve(&self, type_to_resolve: &TypeRef) -> TypeRef {
        match type_to_resolve {
            TypeRef::GenericName(id) => self
                .get_resolved_type(id)
                .unwrap_or(type_to_resolve)
                .clone(),
            TypeRef::Result(a, b) => {
                TypeRef::Result(Box::from(self.resolve(a)), Box::from(self.resolve(b)))
            },
            TypeRef::UnnamedTuple(tuple_elements) => {
                TypeRef::UnnamedTuple(tuple_elements.iter().map(|t| self.resolve(t)).collect())
            },
            TypeRef::Stream(box_t) => TypeRef::Stream(Box::from(self.resolve(box_t))),
            TypeRef::Function(f) => TypeRef::Function(FunctionType {
                parameters: f.parameters.iter().map(|p| self.resolve(p)).collect(),
                return_type: Box::new(self.resolve(&f.return_type)),
            }),
            TypeRef::UnresolvedName(_) => panic!("unresolved types should not exist at this stage"),
            _ => type_to_resolve.clone(),
        }
    }
    pub fn verify_equal(
        &mut self,
        actual_type: &TypeRef,
        expected_type: &TypeRef,
    ) -> SemanticResult<()> {
        self.verify_equal_inner(actual_type, expected_type)
            .map_err(|e| SemanticError::WhileComparingType {
                expected: expected_type.clone(),
                found: actual_type.clone(),
                cause: Box::new(e),
            })
    }

    pub fn verify_equal_inner(
        &mut self,
        actual_type: &TypeRef,
        expected_type: &TypeRef,
    ) -> SemanticResult<()> {
        // If the expected type is an unresolved generic, then we can use resolve that generic to the actual type.
        // If the expected type is a resolved generic, then we must check that the actual type matches this resolved type.
        // If the actual type is a generic, then it must be a generic parameter type of the surrounding function,
        // and we treat it as a regular type, even resolving the expected generic to the generic actual type
        let is_comparable = match (actual_type, expected_type) {
            (TypeRef::GenericName(actual_id), TypeRef::GenericName(expected_id)) => {
                let resolved_actual = self.get_resolved_type(actual_id);
                let resolved_expected = self.get_resolved_type(expected_id);
                match (resolved_actual, resolved_expected) {
                    (Some(resolved_actual), Some(resolved_expected)) => {
                        if resolved_actual != resolved_expected {
                            // this is a tricky error: both expected and actual are generic types,
                            // but both have been resolved to different types.
                            return Err(SemanticError::AmbiguousGenericType {
                                generic_name: expected_id.clone(),
                                first_type: resolved_expected.clone(),
                                second_type: resolved_actual.clone(),
                            });
                        } else {
                            true
                        }
                    },
                    (Some(resolved_actual), None) => {
                        self.set_substitution(expected_id, resolved_actual.clone());
                        true
                    }
                    (None, Some(resolved_expected)) => {
                        self.set_substitution(actual_id, resolved_expected.clone());
                        true
                    }
                    (None, None) => panic!("Tried matching generic {actual_id} with {expected_id} but neither has been resolved yet"),
                }
            },
            (actual_type, TypeRef::GenericName(id)) => {
                let resolution = self.get_resolved_type(id);
                if let Some(resolution) = resolution {
                    if resolution != actual_type {
                        return Err(SemanticError::AmbiguousGenericType {
                            generic_name: id.clone(),
                            first_type: resolution.clone(),
                            second_type: actual_type.clone(),
                        });
                    }
                } else {
                    // resolve the generic expected type to the actual type
                    self.set_substitution(id, actual_type.clone());
                }
                true
            },
            (TypeRef::GenericName(id), expected) => {
                // expected_type is not a generic, actual type is a generic parameter type of the surrounding function.
                // if the generic is not already resolved, then we don't accept it
                let resolution = self.get_resolved_type(id);

                if resolution == None {
                    false
                } else if resolution != Some(expected) {
                    return Err(SemanticError::AmbiguousGenericType {
                        generic_name: id.clone(),
                        first_type: resolution.unwrap_or(actual_type).clone(),
                        second_type: expected.clone(),
                    });
                } else {
                    true
                }
            },
            (TypeRef::Defined(a), TypeRef::Defined(b)) => {
                if (a.id == b.id) && (a.generics.len() == b.generics.len()) {
                    for (a2, b2) in a.generics.iter().zip(&b.generics) {
                        self.verify_equal_inner(a2, b2)?
                    }
                }
                true
            },
            (TypeRef::Result(a1, a2), TypeRef::Result(b1, b2)) => {
                self.verify_equal_inner(a1, b1)?;
                self.verify_equal_inner(a2, b2)?;
                true
            },
            (TypeRef::UnnamedTuple(a), TypeRef::UnnamedTuple(b)) => {
                if a.len() != b.len() {
                    false
                } else {
                    for (a2, b2) in a.iter().zip(b) {
                        self.verify_equal_inner(a2, b2)?;
                    }
                    true
                }
            },
            (TypeRef::Stream(a), TypeRef::Stream(b)) => return self.verify_equal_inner(a, b),
            (TypeRef::Function(a), TypeRef::Function(b)) => {
                if a.parameters.len() != b.parameters.len() {
                    false
                } else {
                    for (a2, b2) in a.parameters.iter().zip(&b.parameters) {
                        self.verify_equal_inner(a2, b2)?;
                    }
                    return self.verify_equal_inner(&b.return_type, &a.return_type);
                }
            },
            (TypeRef::Void, TypeRef::Void) => true,
            (TypeRef::Break, TypeRef::Break) => true,
            _ => false,
        };

        if is_comparable {
            Ok(())
        } else {
            Err(SemanticError::TypeMismatchError {
                expected: expected_type.clone(),
                found: actual_type.clone(),
            })
        }
    }
}
