use crate::{
    interpretation::{
        interperation_result::{InterpResult, InterpretationError},
        stack_frame::StackFrame,
        value::Value,
    },
    symbolization::ast::*,
};

pub struct FunctionBuilder {
    var_id: VariableId,
}

impl FunctionBuilder {
    pub fn new() -> FunctionBuilder {
        FunctionBuilder { var_id: 0 }
    }

    pub fn new_var_id(&mut self) -> VariableId {
        let id = self.var_id;
        self.var_id = id + 1;
        id
    }

    pub fn flag(&mut self, long_name: Option<&str>, short_name: Option<&str>) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: TypeRef::BOOLEAN.clone(),
            is_optional: true,
            long_name: long_name.map(Identifier::from),
            short_name: short_name.map(Identifier::from),
        }
    }

    pub fn opt_par(&mut self, long_name: &str, t: &TypeRef) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: t.clone(),
            is_optional: true,
            long_name: Some(Identifier::from(long_name)),
            short_name: None,
        }
    }

    pub fn req_par(&mut self, long_name: &str, t: &TypeRef) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: t.clone(),
            is_optional: false,
            long_name: Some(Identifier::from(long_name)),
            short_name: None,
        }
    }

    pub fn opt_par_s(&mut self, long_name: &str, short_name: &str, t: &TypeRef) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: t.clone(),
            is_optional: true,
            long_name: Some(Identifier::from(long_name)),
            short_name: Some(Identifier::from(short_name)),
        }
    }

    pub fn req_par_s(&mut self, long_name: &str, short_name: &str, t: &TypeRef) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: t.clone(),
            is_optional: false,
            long_name: Some(Identifier::from(long_name)),
            short_name: Some(Identifier::from(short_name)),
        }
    }

    pub fn sorted(parameters: &[&Parameter]) -> Vec<Parameter> {
        let mut result: Vec<Parameter> = parameters.into_iter().map(|&p| p.to_owned()).collect();
        result.sort_unstable_by_key(|p| p.id);
        result
    }

    pub fn get_value(arguments: &mut Vec<Value>, par: &Parameter) -> Value {
        std::mem::replace(&mut arguments[par.id], Value::Nothing)
    }

    pub fn get_string(arguments: &mut Vec<Value>, par: &Parameter) -> String {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::String(value) = arg {
            value
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }

    pub fn get_boolean(arguments: &mut Vec<Value>, par: &Parameter) -> bool {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Boolean(value) = arg {
            value
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }

    pub fn get_int(arguments: &mut Vec<Value>, par: &Parameter) -> i32 {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Int(value) = arg {
            value
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }

    pub fn get_fn(
        arguments: &mut Vec<Value>,
        par: &Parameter,
    ) -> (GlobalFunctionTarget, StackFrame) {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::FunctionLamda(value, arguments) = arg {
            (value, arguments)
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }

    pub fn get_tuple(arguments: &mut Vec<Value>, par: &Parameter) -> Vec<Value> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Tuple(value) = arg {
            value
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }
}
