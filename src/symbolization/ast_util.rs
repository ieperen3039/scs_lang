use std::{collections::HashMap, rc::Rc};

use super::ast::*;

impl TypeDefinition {
    pub const STRING_TYPE: Rc<TypeDefinition> = Rc::from(TypeDefinition::build_native("String"));

    pub const INT_TYPE: Rc<TypeDefinition> = Rc::from(TypeDefinition::build_native("int"));

    pub const RESULT_TYPE: Rc<TypeDefinition> = {
        let generic_pos = Rc::from(GenericParameter{ name: Rc::from("P")});
        let generic_neg = Rc::from(GenericParameter{ name: Rc::from("N")});
        Rc::from(TypeDefinition {
            name: Rc::from("Result"),
            generic_parameters: vec![generic_pos, generic_neg],
            sub_type: TypeSubType::Variant {
                variants: vec![
                    VariantValue {
                        name: Rc::from("pos"),
                        value_type: TypeRef::Generic(generic_pos),
                    },
                    VariantValue {
                        name: Rc::from("neg"),
                        value_type: TypeRef::Generic(generic_neg),
                    },
                ],
            },
        })
    };

    pub const OPTIONAL_TYPE: Rc<TypeDefinition> = {
        let generic_type = Rc::from(GenericParameter{ name: Rc::from("T") });
        Rc::from(TypeDefinition {
            name: Rc::from("Optional"),
            generic_parameters: vec![generic_type],
            sub_type: TypeSubType::Base {
                derived: Some(Box::from(TypeRef::Defined(DefinedTypeRef {
                    definition: TypeDefinition::RESULT_TYPE,
                    generic_parameters: vec![TypeRef::Generic(generic_type), TypeRef::Void],
                }))),
            },
        })
    };

    pub const BOOLEAN_TYPE: Rc<TypeDefinition> = Rc::from(TypeDefinition {
        name: Rc::from("boolean"),
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Base {
            derived: Some(Box::from(TypeRef::Defined(DefinedTypeRef {
                definition: TypeDefinition::RESULT_TYPE,
                generic_parameters: vec![TypeRef::Void, TypeRef::Void],
            }))),
        },
    });

    pub fn get_name(&self) -> Identifier {
        self.name
    }

    pub fn build_plain(name: &str, derived_from: Option<TypeRef>) -> TypeDefinition {
        TypeDefinition {
            name: Rc::from(name),
            generic_parameters: Vec::new(),
            sub_type: TypeSubType::Base {
                derived: derived_from.map(Box::from),
            },
        }
    }

    pub fn build_native(name: &str) -> TypeDefinition {
        TypeDefinition {
            name: Rc::from(name),
            generic_parameters: Vec::new(),
            sub_type: TypeSubType::Base { derived: None },
        }
    }
}

impl Expression {
    pub fn get_type(&self) -> TypeRef {
        match self {
            Expression::StaticFunctionCall(fun) => fun.function.body.return_var.var_type,
            Expression::FunctionBlock(block) => block.return_var.var_type,
            Expression::Array(array) => array.element_type,
            Expression::Literal(Literal::String(_)) => TypeRef::Defined(DefinedTypeRef {
                definition: TypeDefinition::STRING_TYPE,
                generic_parameters: Vec::new(),
            }),
            Expression::Literal(Literal::Number(_)) => TypeRef::Defined(DefinedTypeRef {
                definition: TypeDefinition::INT_TYPE,
                generic_parameters: Vec::new(),
            }),
            Expression::Literal(Literal::Boolean(_)) => TypeRef::Defined(DefinedTypeRef {
                definition: TypeDefinition::BOOLEAN_TYPE,
                generic_parameters: Vec::new(),
            }),
            Expression::Variable(var) => var.var_type,
        }
    }
}

impl Scope {
    pub fn new(name: &str, parent: Option<&Scope>) -> Scope {
        let mut full_name = parent.map(|sc| sc.full_name.clone()).unwrap_or(Vec::new());
        full_name.push(Rc::from(name));

        Scope {
            full_name,
            scopes: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn get_name(&self) -> Identifier {
        self.full_name.last().unwrap().to_owned()
    }

    pub fn extend(&mut self, other: Scope) {
        self.scopes.extend(other.scopes);
        self.types.extend(other.types);
    }

    pub fn combined_with(mut self, other: Scope) -> Scope {
        self.extend(other);
        self
    }

    pub fn get(&self, name: &str) -> Option<Rc<TypeDefinition>> {
        self.types.get(name).map(Rc::clone)
    }
}
