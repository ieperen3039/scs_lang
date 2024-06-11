use crate::symbolization::{ast::*, type_collector::TypeCollector};

pub type InternalTypes = Vec<TypeDefinition>;

pub fn build_types(_tc: &mut TypeCollector) -> InternalTypes {
    vec![]
}

pub fn get_types(types: &Vec<TypeDefinition>) -> Namespace {
    let mut namespace = Namespace::new_root();
    for type_def in types {
        namespace.add_type(type_def);
    }
    namespace
}

pub fn build_derived(mut full_name: Vec<&str>, id: TypeId, base: &TypeRef) -> TypeDefinition {
    let name = Identifier::from(full_name.pop().unwrap());
    let full_scope = full_name.into_iter().map(Identifier::from).collect();

    TypeDefinition {
        name,
        id,
        type_class: TypeClass::Base {
            derived: Some(Box::from(base.clone())),
        },
        full_scope,
    }
}

pub fn build_result(id: TypeId, type_1: &TypeRef, type_2: &TypeRef) -> TypeDefinition {
    build_variant(
        &format!("{:?}!{:?}", type_1, type_2),
        id,
        vec![("Pos", type_1), ("Neg", type_2)],
    )
}

pub fn build_optional(id: TypeId, opt_type: &TypeRef) -> TypeDefinition {
    build_variant(
        &format!("{:?}?", opt_type),
        id,
        vec![("Some", opt_type), ("None", &TypeRef::Void)],
    )
}

pub fn build_variant(name: &str, id: TypeId, values: Vec<(&str, &TypeRef)>) -> TypeDefinition {
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
        full_scope: Vec::new(),
    }
}
