use crate::symbolization::{ast::*, function_collector::FunctionCollector};

pub fn get_functions(collector: &mut FunctionCollector) -> Vec<FunctionDeclaration> {
    vec![collector.create_external(
        "echo",
        vec![
            req_par(0, "in", None, &TypeRef::STRING),
            flag(1, Some("error"), Some("e")),
        ],
        TypeRef::STRING.clone(),
    )]
}

fn flag(id: VariableId, long_name: Option<&str>, short_name: Option<&str>) -> Parameter {
    Parameter {
        id,
        par_type: TypeRef::Flag,
        long_name: long_name.map(Identifier::from),
        short_name: short_name.map(Identifier::from),
    }
}

fn opt_par(id: VariableId, long_name: &str, short_name: Option<&str>, t: &TypeRef) -> Parameter {
    Parameter {
        id,
        par_type: TypeRef::Optional(Box::from(t.clone())),
        long_name: Some(Identifier::from(long_name)),
        short_name: short_name.map(Identifier::from),
    }
}

fn req_par(id: VariableId, long_name: &str, short_name: Option<&str>, t: &TypeRef) -> Parameter {
    Parameter {
        id,
        par_type: t.clone(),
        long_name: Some(Identifier::from(long_name)),
        short_name: short_name.map(Identifier::from),
    }
}
