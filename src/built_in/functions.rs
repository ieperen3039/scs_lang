use crate::{
    interpretation::{execution_state::Variable, meta_structures::*},
    symbolization::{ast::*, function_collector::FunctionCollector},
};

pub fn flag(long_name: Option<&str>, short_name: Option<&str>) -> Parameter {
    Parameter {
        par_type: TypeRef::Flag,
        long_name: long_name.map(Identifier::from),
        short_name: short_name.map(Identifier::from),
    }
}

pub fn opt_par(long_name: &str, short_name: Option<&str>, t: &TypeRef) -> Parameter {
    Parameter {
        par_type: TypeRef::Optional(Box::from(t.clone())),
        long_name: Some(Identifier::from(long_name)),
        short_name: short_name.map(Identifier::from),
    }
}

pub fn req_par(long_name: &str, short_name: Option<&str>, t: &TypeRef) -> Parameter {
    Parameter {
        par_type: t.clone(),
        long_name: Some(Identifier::from(long_name)),
        short_name: short_name.map(Identifier::from),
    }
}

pub fn get_built_in_functions(collector: &mut FunctionCollector) -> Vec<FunctionDeclaration> {
    vec![collector.create_external(
        "echo",
        vec![
            req_par("in", None, &TypeRef::STRING),
            flag(Some("error"), Some("e")),
        ],
        TypeRef::STRING.clone(),
    )]
}

pub fn create_function_variable(
    decl: FunctionDeclaration,
    implementation: NativeFunction,
) -> Variable {
    let parameters = decl
        .parameters
        .iter()
        .map(Parameter::to_type)
        .cloned()
        .collect();

    Variable {
        name: decl.name,
        var_type: TypeRef::Function(FunctionType {
            parameters,
            return_type: Box::from(decl.return_type),
        }),
        value: Value::InternalFunction(implementation),
    }
}
