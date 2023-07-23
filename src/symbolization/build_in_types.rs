use std::rc::Rc;

use simple_error::SimpleError;

use super::ast::*;

pub const TYPE_ID_INT : u32 = 0;
pub const TYPE_ID_STRING : u32 = 1;
pub const TYPE_ID_RESULT : u32 = 2;
pub const TYPE_ID_OPTIONAL : u32 = 3;
pub const TYPE_ID_BOOLEAN : u32 = 4;
pub const FIRST_CUSTOM_TYPE_ID : u32 = 5;

pub fn get_implicit_scope(name: &str) -> Result<Scope, SimpleError>
{
    let mut scope = Scope::new(name, None);

    let type_int = build_native("int", TYPE_ID_INT);
    let type_string = build_native("String", TYPE_ID_STRING);

    let type_result = {
        let generic_pos = Rc::from(GenericParameter {
            name: Rc::from("P"),
        });
        let generic_neg = Rc::from(GenericParameter {
            name: Rc::from("N"),
        });
        TypeDefinition {
            name: Rc::from("Result"),
            id: TYPE_ID_RESULT,
            generic_parameters: vec![generic_pos.clone(), generic_neg.clone()],
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
        }
    };

    let type_optional = {
        let generic_type = Rc::from(GenericParameter {
            name: Rc::from("T"),
        });
        TypeDefinition {
            name: Rc::from("Optional"),
            id: TYPE_ID_OPTIONAL,
            generic_parameters: vec![generic_type.clone()],
            sub_type: TypeSubType::Base {
                derived: Some(Box::from(TypeRef::Defined(DefinedRef {
                    id: type_result.id,
                    generic_parameters: vec![TypeRef::Generic(generic_type), TypeRef::Void],
                }))),
            },
        }
    };

    let type_boolean = TypeDefinition {
        name: Rc::from("boolean"),
        id: TYPE_ID_BOOLEAN,
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Base {
            derived: Some(Box::from(TypeRef::Defined(DefinedRef {
                id: type_result.id,
                generic_parameters: vec![TypeRef::Void, TypeRef::Void],
            }))),
        },
    };

    scope.add_type(type_int);
    scope.add_type(type_string);
    scope.add_type(type_result);
    scope.add_type(type_optional);
    scope.add_type(type_boolean);

    Ok(scope)
}

fn build_plain(name: &str, id: u32, derived_from: Option<TypeRef>) -> TypeDefinition {
    TypeDefinition {
        name: Rc::from(name),
        id,
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Base {
            derived: derived_from.map(Box::from),
        },
    }
}

fn build_native(name: &str, id: u32) -> TypeDefinition {
    TypeDefinition {
        name: Rc::from(name),
        id,
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Base { derived: None },
    }
}