use super::ast::*;

pub const TYPE_ID_INT: u32 = 0;
pub const TYPE_ID_FLOAT: u32 = 1;
pub const TYPE_ID_CHARACTER: u32 = 2;
pub const TYPE_ID_STRING: u32 = 3;
pub const FIRST_CUSTOM_TYPE_ID: u32 = 4;

pub fn get_implicit() -> Scope {
    let mut scope = Scope::new("", None);

    scope.add_type(&build_native("int", TYPE_ID_INT));
    scope.add_type(&build_native("float", TYPE_ID_FLOAT));
    scope.add_type(&build_native("character", TYPE_ID_CHARACTER));
    scope.add_type(&build_native("String", TYPE_ID_STRING));

    scope
}

fn build_native(name: &str, id: u32) -> TypeDefinition {
    TypeDefinition {
        name: Identifier::from(name),
        id,
        type_class: TypeClass::Base { derived: None },
        full_scope: Vec::new(), // root scope
    }
}
