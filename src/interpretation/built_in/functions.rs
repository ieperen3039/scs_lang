use std::rc::Rc;

use crate::{
    interpretation::{
        built_in::implementations,
        execution_state::Variable, meta_structures::*,
    },
    symbolization::ast::*,
};

pub fn flag(long_name: Option<&str>, short_name: Option<String>) -> Parameter {
    Parameter {
        par_type: ParameterType::Flag,
        long_name: long_name.map(str::to_string),
        short_name,
    }
}

pub fn opt_par(long_name: &str, short_name: Option<String>, t: &TypeRef) -> Parameter {
    Parameter {
        par_type: ParameterType::Optional(t.clone()),
        long_name: Some(String::from(long_name)),
        short_name,
    }
}

pub fn req_par(long_name: &str, short_name: Option<String>, t: &TypeRef) -> Parameter {
    Parameter {
        par_type: ParameterType::Required(t.clone()),
        long_name: Some(String::from(long_name)),
        short_name,
    }
}

pub fn get_built_in_functions() -> Vec<Variable> {
    vec![create_function(
        "echo",
        vec![req_par("in", None, &TypeRef::STRING)],
        &TypeRef::STRING,
        implementations::echo,
    )]
}

pub fn create_function(
    name: &str,
    parameters: Vec<Parameter>,
    return_type: &TypeRef,
    implementation: FauxFunction,
) -> Variable {
    let parameter_types = parameters
        .iter()
        .map(Parameter::to_type)
        .collect();

    Variable {
        name: Rc::from(name),
        var_type: TypeRef::Function(FunctionType {
            parameters: parameter_types,
            return_type: Box::from(return_type.clone()),
        }),
        value: Value::InternalFunction(InternalFunction{ func: implementation, parameters }),
    }
}
