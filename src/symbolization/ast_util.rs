use std::{collections::HashMap, rc::Rc};

use super::ast::*;

impl TypeDefinition {
    pub const STRING: Rc<TypeDefinition> = Rc::from(TypeDefinition::build_native("String"));

    pub const NUMBER: Rc<TypeDefinition> = Rc::from(TypeDefinition::build_native("int"));

    pub const RESULT: Rc<TypeDefinition> = {
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

    pub const OPTIONAL: Rc<TypeDefinition> = {
        let generic_type = Rc::from(GenericParameter{ name: Rc::from("T") });
        Rc::from(TypeDefinition {
            name: Rc::from("Optional"),
            generic_parameters: vec![generic_type],
            sub_type: TypeSubType::Base {
                derived: Some(Box::from(TypeRef::Defined(DefinedTypeRef {
                    definition: TypeDefinition::RESULT,
                    generic_parameters: vec![TypeRef::Generic(generic_type), TypeRef::Void],
                }))),
            },
        })
    };

    pub const BOOLEAN: Rc<TypeDefinition> = Rc::from(TypeDefinition {
        name: Rc::from("boolean"),
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Base {
            derived: Some(Box::from(TypeRef::Defined(DefinedTypeRef {
                definition: TypeDefinition::RESULT,
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

impl TypeRef {
    pub const STRING : TypeRef = TypeRef::Defined(DefinedTypeRef {
        definition: TypeDefinition::STRING,
        generic_parameters: Vec::new(),
    });

    pub const NUMBER : TypeRef = TypeRef::Defined(DefinedTypeRef {
        definition: TypeDefinition::NUMBER,
        generic_parameters: Vec::new(),
    });

    pub const BOOLEAN : TypeRef = TypeRef::Defined(DefinedTypeRef {
        definition: TypeDefinition::BOOLEAN,
        generic_parameters: Vec::new(),
    });
}

impl Expression {
    pub fn get_type(&self) -> &TypeRef {
        match &self {
            Expression::StaticFunctionCall(fun) => &fun.function.body.return_var.var_type,
            Expression::FunctionBlock(block) => &block.return_var.var_type,
            Expression::Array(array) => &array.element_type,
            Expression::Literal(Literal::String(_)) => &TypeRef::STRING,
            Expression::Literal(Literal::Number(_)) => &TypeRef::NUMBER,
            Expression::Literal(Literal::Boolean(_)) => &TypeRef::BOOLEAN,
            Expression::Variable(var) => &var.var_type,
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
