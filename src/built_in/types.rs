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
