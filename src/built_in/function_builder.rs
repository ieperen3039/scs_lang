use crate::{
    interpretation::{stack_frame::StackFrame, value::Value},
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

    pub fn flag(&mut self, long_name: &str) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: TypeRef::boolean(),
            is_optional: true,
            long_name: Some(Identifier::from(long_name)),
            short_name: None,
        }
    }

    pub fn flag_s(&mut self, long_name: &str, short_name: &str) -> Parameter {
        Parameter {
            id: self.new_var_id(),
            par_type: TypeRef::boolean(),
            is_optional: true,
            long_name: Some(Identifier::from(long_name)),
            short_name: Some(Identifier::from(short_name)),
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

    pub fn get_value(arguments: &mut Vec<Value>, par: &Parameter) -> Value {
        std::mem::replace(&mut arguments[par.id], Value::Nothing)
    }

    pub fn get_string(arguments: &mut Vec<Value>, par: &Parameter) -> String {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::String(value) = arg {
            return value;
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

        if let Value::Variant(variant_id, value) = arg {
            if matches!(*value, Value::Nothing) {
                return variant_id != 0;
            } else {
                panic!(
                    "booleans must be a variant with an empty value, but found {:?}",
                    *value
                );
            }
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
            return value;
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
    ) -> (LocalFunctionTarget, StackFrame) {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::FunctionLamda(value, arguments) = arg {
            return (value, arguments);
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }

    pub fn get_variant(arguments: &mut Vec<Value>, par: &Parameter) -> (u32, Value) {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Variant(id, value) = arg {
            return (id, *value);
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arg,
                par.name()
            );
        }
    }

    // this function does not consume the argument if it is not the requested type.
    // it is less efficient than calling get_variant, unless the original variant
    // should be preseved when no match is made
    pub fn get_variant_if_present(
        arguments: &mut Vec<Value>,
        par: &Parameter,
        id: u32,
    ) -> Option<Value> {
        if let Value::Variant(v_id, _) = &arguments[par.id] {
            if *v_id != id {
                None
            } else {
                let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);
                if let Value::Variant(_, value) = arg {
                    Some(*value)
                } else {
                    unreachable!("We already checked")
                }
            }
        } else {
            panic!(
                "Expected type \"{:?}\", but found value \"{:?}\" for argument {:?}",
                par.par_type,
                arguments[par.id],
                par.name()
            );
        }
    }

    pub fn get_tuple(arguments: &mut Vec<Value>, par: &Parameter) -> Vec<Value> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Tuple(value) = arg {
            return value;
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
