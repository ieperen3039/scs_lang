use std::{collections::HashMap, rc::Rc};

use super::ast::*;

impl StructDefinition {
    pub fn build_plain(name: &str, fields: Vec<StructField>) -> StructDefinition {
        StructDefinition {
            name: Rc::from(name),
            generic_parameters: Vec::new(),
            derived_from: None,
            fields,
        }
    }

    pub fn build_native(name: &str) -> StructDefinition {
        StructDefinition {
            name: Rc::from(name),
            generic_parameters: Vec::new(),
            derived_from: None,
            fields: Vec::new(),
        }
    }
}

impl TypeDefinition {
    pub const STRING_TYPE: Rc<TypeDefinition> = Rc::from(TypeDefinition::Tuple(
        StructDefinition::build_native("String"),
    ));
    
    pub const INT_TYPE: Rc<TypeDefinition> = Rc::from(TypeDefinition::Tuple(
        StructDefinition::build_native("int"),
    ));

    pub const RESULT_TYPE: Rc<TypeDefinition> =
        Rc::from(TypeDefinition::Variant(VariantDefinition {
            name: Rc::from("Result"),
            values: vec![Rc::from("pos"), Rc::from("neg")],
            derived_from: None,
        }));

    pub const OPTIONAL_TYPE: Rc<TypeDefinition> =
        Rc::from(TypeDefinition::Variant(VariantDefinition {
            name: Rc::from("Optional"),
            values: vec![Rc::from("some"), Rc::from("none")],
            derived_from: Some(DefinedTypeRef {
                definition: TypeDefinition::RESULT_TYPE,
                generic_parameters: ,
            }),
        }));

    pub const BOOLEAN_TYPE: Rc<TypeDefinition> =
    Rc::from(TypeDefinition::Variant(VariantDefinition {
        name: Rc::from("boolean"),
        values: vec![Rc::from("true"), Rc::from("false")],
        derived_from: Some(DefinedTypeRef {
            definition: TypeDefinition::RESULT_TYPE,
            generic_parameters: ,
        }),
    }));

    pub fn get_name(&self) -> Identifier {
        match self {
            TypeDefinition::Tuple(StructDefinition { name, .. }) => name.clone(),
            TypeDefinition::Enum(EnumDefinition { name, .. }) => name.clone(),
            TypeDefinition::Variant(VariantDefinition { name, .. }) => name.clone(),
            TypeDefinition::Base(BaseType { name, .. }) => name.clone(),
        }
    }
}

impl Expression {
    pub fn get_type(&self) -> TypeRef {
        match self {
            Expression::StaticFunctionCall(fun) => fun.function.body.return_var.var_type,
            Expression::FunctionBlock(block) => block.return_var.var_type,
            Expression::Array(array) => array.element_type,
            Expression::Literal(Literal::String(_)) => TypeRef::Defined(DefinedTypeRef { definition: TypeDefinition::STRING_TYPE, generic_parameters: Vec::new() }), 
            Expression::Literal(Literal::Number(_)) => TypeRef::Defined(DefinedTypeRef { definition: TypeDefinition::INT_TYPE, generic_parameters: Vec::new() }),
            Expression::Literal(Literal::Boolean(_)) => TypeRef::Defined(DefinedTypeRef { definition: TypeDefinition::BOOLEAN_TYPE, generic_parameters: Vec::new() }),
            Expression::Variable(var) => var.var_type,
        }
    }
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            scopes: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
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
