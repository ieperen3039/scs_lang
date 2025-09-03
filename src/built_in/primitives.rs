use crate::symbolization::ast;
use crate::symbolization::ast::*;
use crate::symbolization::ast::Literal::Boolean;

pub const TYPE_ID_INT: u32 = 0;
pub const TYPE_ID_FLOAT: u32 = 1;
pub const TYPE_ID_CHARACTER: u32 = 2;
pub const TYPE_ID_STRING: u32 = 3;
// pub const TYPE_ID_RESULT: u32 = 4;
pub const FIRST_CUSTOM_TYPE_ID: u32 = 5;

pub fn build_primitives() -> Vec<TypeDefinition> {
    vec![
        build_primitive("int", TYPE_ID_INT),
        build_primitive("float", TYPE_ID_FLOAT),
        build_primitive("character", TYPE_ID_CHARACTER),
        build_primitive("String", TYPE_ID_STRING),
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

pub fn build_constants() -> Namespace {
    let mut namespace = Namespace::new_root();
    namespace.add_constant_literal(ast::Identifier::from("true"), Boolean(true));
    namespace.add_constant_literal(ast::Identifier::from("false"), Boolean(false));
    namespace
}