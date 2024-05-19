use crate::symbolization::ast::*;

pub const TYPE_ID_INT: u32 = 0;
pub const TYPE_ID_FLOAT: u32 = 1;
pub const TYPE_ID_CHARACTER: u32 = 2;
pub const TYPE_ID_STRING: u32 = 3;
pub const TYPE_ID_BOOLEAN: u32 = 4;
pub const FIRST_CUSTOM_TYPE_ID: u32 = 5;

pub fn get_primitives() -> Vec<TypeDefinition> {
    vec![
        build_primitive("int", TYPE_ID_INT),
        build_primitive("float", TYPE_ID_FLOAT),
        build_primitive("character", TYPE_ID_CHARACTER),
        build_primitive("String", TYPE_ID_STRING),
        build_variant(
            "boolean",
            TYPE_ID_BOOLEAN,
            vec![("Pos", &TypeRef::Void), ("Neg", &TypeRef::Void)],
        ),
    ]
}

fn build_primitive(name: &str, id: u32) -> TypeDefinition {
    TypeDefinition {
        name: Identifier::from(name),
        id,
        type_class: TypeClass::Base { derived: None },
        full_scope: Vec::new(), // root scope
    }
}

fn build_derived(name: &str, id: u32, base: &TypeRef) -> TypeDefinition {
    TypeDefinition {
        name: Identifier::from(name),
        id,
        type_class: TypeClass::Base {
            derived: Some(Box::from(base.clone())),
        },
        full_scope: Vec::new(), // root scope
    }
}

fn build_variant(name: &str, id: u32, values: Vec<(&str, &TypeRef)>) -> TypeDefinition {
    TypeDefinition {
        name: Identifier::from(name),
        id,
        type_class: TypeClass::Variant {
            variants: values
                .into_iter()
                .map(|(value_1, type_1)| VariantValue {
                    name: Identifier::from(value_1),
                    value_type: type_1.clone(),
                })
                .collect(),
        },
        full_scope: Vec::new(), // root scope
    }
}
