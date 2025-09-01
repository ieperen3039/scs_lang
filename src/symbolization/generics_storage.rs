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
        let mut combined = Vec::new();
        combined.extend(other.rigid_parameters.iter().cloned());
        // TODO is this correct?
        combined.extend(other.arguments.keys().cloned());

        Self {
            rigid_parameters: combined,
            arguments: HashMap::new(),
        }
    }

    pub fn set_substitution(&mut self, name: &Identifier, substitution: TypeRef) {
        // we avoid needing to clone name again; the key must already be here anyway
        let entry = self
            .arguments
            .get_mut(name)
            .expect("given name must have been added first");
        *entry = Some(substitution);
    }

    pub fn set_arguments<I: Iterator<Item = Identifier>>(&mut self, names: I) {
        for name in names {
            self.arguments.insert(name, None);
        }
    }

    pub fn set_parameters<I: Iterator<Item = Identifier>>(&mut self, names: I) {
        for name in names {
            self.arguments.insert(name, None);
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
            TypeRef::GenericName(id) => self.get_resolved_type(id).unwrap_or(type_to_resolve).clone(),
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

    pub fn verify_equal(&mut self, argument: &TypeRef, parameter: &TypeRef) -> SemanticResult<()> {
        let is_comparable = match (argument, parameter) {
            (TypeRef::GenericName(id1), TypeRef::GenericName(id2)) => {
                // TODO what are we comparing? generic arguments, or generic parameters? a mix mayhaps?
                // check if either of them has been resolved
                let resolution1 = self.get_resolved_type(id1);
                let resolution2 = self.get_resolved_type(id2);
                match (resolution1, resolution2) {
                    (Some(r1), Some(r2)) => r1 == r2,
                    (Some(r1), None) => {
                        self.set_substitution(id2, r1.clone());
                        true
                    },
                    (None, Some(r2)) => {
                        self.set_substitution(id1, r2.clone());
                        true
                    },
                    (None, None) => true,
                }
            },
            (TypeRef::GenericName(id), other) | (other, TypeRef::GenericName(id)) => {
                let resolution = self.get_resolved_type(id);
                if let Some(resolution) = resolution {
                    if resolution != other {
                        return Err(SemanticError::AmbiguousGenericType {
                            generic_name: id.clone(),
                            first_type: resolution.clone(),
                            second_type: other.clone(),
                        });
                    } else {
                        true
                    }
                } else {
                    self.set_substitution(id, other.clone());
                    true
                }
            },
            (TypeRef::Defined(a), TypeRef::Defined(b)) => {
                if (a.id == b.id) && (a.generics.len() == b.generics.len()) {
                    for (a2, b2) in a.generics.iter().zip(&b.generics) {
                        self.verify_equal(a2, b2)?
                    }
                }
                true
            },
            (TypeRef::Result(a1, a2), TypeRef::Result(b1, b2)) => {
                self.verify_equal(b1, a1)?;
                self.verify_equal(b2, a2)?;
                true
            },
            (TypeRef::UnnamedTuple(a), TypeRef::UnnamedTuple(b)) => {
                if a.len() != b.len() {
                    return Err(SemanticError::TypeMismatchError {
                        expected: parameter.clone(),
                        found: argument.clone(),
                    });
                }
                for (a2, b2) in a.iter().zip(b) {
                    self.verify_equal(b2, a2)?;
                }
                true
            },
            (TypeRef::Stream(a), TypeRef::Stream(b)) => return self.verify_equal(b, a),
            (TypeRef::Function(a), TypeRef::Function(b)) => {
                if a.parameters.len() != b.parameters.len() {
                    return Err(SemanticError::TypeMismatchError {
                        expected: parameter.clone(),
                        found: argument.clone(),
                    });
                }
                for (a2, b2) in a.parameters.iter().zip(&b.parameters) {
                    self.verify_equal(b2, a2)?;
                }
                return self.verify_equal(&b.return_type, &a.return_type);
            },
            (TypeRef::Void, TypeRef::Void) => true,
            (TypeRef::Break, TypeRef::Break) => true,
            _ => false,
        };

        if is_comparable {
            Ok(())
        } else {
            Err(SemanticError::TypeMismatchError {
                expected: parameter.clone(),
                found: argument.clone(),
            })
        }
    }
}
