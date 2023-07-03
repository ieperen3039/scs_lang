use std::{collections::HashMap, rc::Rc};

use super::proto_ast::*;

impl ProtoScope {
    pub fn new(name: &str) -> ProtoScope {
        ProtoScope {
            name : Rc::from(name),
            scopes: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

impl TypeName {
    pub fn build_fn(return_type: TypeName) -> TypeName {
        TypeName::FunctionType(FunctionName {
            parameters: Vec::new(),
            return_type: Box::from(return_type),
        })
    }

    pub fn build_result(pos_type: TypeName, neg_type: TypeName) -> TypeName {
        TypeName::Struct(StructName {
            name: Rc::from("Result"),
            scope: Vec::new(),
            generic_parameters: vec![pos_type, neg_type],
        })
    }

    pub fn build_optional(some_type: TypeName) -> TypeName {
        TypeName::Struct(StructName {
            name: Rc::from("Optional"),
            scope: Vec::new(),
            generic_parameters: vec![some_type],
        })
    }
}

impl StructName {
    pub fn build_plain(name: &str) -> StructName {
        StructName {
            name: Rc::from(name),
            scope: Vec::new(),
            generic_parameters: Vec::new(),
        }
    }
}
