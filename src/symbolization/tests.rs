use std::rc::Rc;

use crate::parsing::{ebnf_parser, parser};

use super::{symbolizer, ast::*, built_in_types::FIRST_CUSTOM_TYPE_ID};


#[test]
fn collect_types_1() {
    // based on the observation that an implicit operator in the example could not be parsed
    let definition = include_str!("../../doc/definition.ebnf");
    let program = r#"
        type FilePath : String
    "#;

    let grammar = ebnf_parser::parse_ebnf(definition).unwrap();
    let parser = parser::Parser::new(grammar, None).unwrap();
    let ast = parser.parse_program(&program).unwrap();
}

fn parse_implicit() {
    let definition = include_str!("../../doc/definition.ebnf");
    let program = include_str!("../../doc/implicit_impl.faux");

    let type_result = {
        let generic_pos = Rc::from(GenericParameter {
            name: Rc::from("P"),
        });
        let generic_neg = Rc::from(GenericParameter {
            name: Rc::from("N"),
        });
        TypeDefinition {
            name: Rc::from("Result"),
            id: FIRST_CUSTOM_TYPE_ID,
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
            id: FIRST_CUSTOM_TYPE_ID + 1,
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
        id: FIRST_CUSTOM_TYPE_ID + 2,
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Base {
            derived: Some(Box::from(TypeRef::Defined(DefinedRef {
                id: type_result.id,
                generic_parameters: vec![TypeRef::Void, TypeRef::Void],
            }))),
        },
    };
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