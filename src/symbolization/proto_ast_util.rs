use std::{collections::HashMap, rc::Rc};

use super::proto_ast::*;

impl ProtoScope {
    pub fn new(name: &str, parent: Option<&ProtoScope>) -> ProtoScope {
        let mut full_name = parent.map(|sc| sc.full_name.clone()).unwrap_or(Vec::new());
        full_name.push(Rc::from(name));

        ProtoScope {
            full_name,
            scopes: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn get_name(&self) -> Identifier {
        self.full_name.last().unwrap().to_owned()
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
        TypeName::Defined(DefinedName {
            name: Rc::from("Result"),
            scope: Vec::new(),
            generic_parameters: vec![pos_type, neg_type],
        })
    }

    pub fn build_optional(some_type: TypeName) -> TypeName {
        TypeName::Defined(DefinedName {
            name: Rc::from("Optional"),
            scope: Vec::new(),
            generic_parameters: vec![some_type],
        })
    }
}

impl DefinedName {
    pub fn build_plain(name: &str) -> DefinedName {
        DefinedName {
            name: Rc::from(name),
            scope: Vec::new(),
            generic_parameters: Vec::new(),
        }
    }
}
