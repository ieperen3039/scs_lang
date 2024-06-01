use crate::{
    interpretation::meta_structures::{InterpResult, InterpretationError, Value},
    symbolization::{ast::*, function_collector::FunctionCollector},
};

pub mod echo;

pub trait InternalFunction {
    fn new(collector: &mut FunctionCollector) -> Self;

    fn get_declaration(&self) -> FunctionDeclaration;

    fn call(&self, arguments: Vec<Value>) -> InterpResult<Value>;
}

pub struct FunctionBuilder {
    var_id: VariableId,
}

impl FunctionBuilder {
    pub fn new() -> FunctionBuilder {
        FunctionBuilder { var_id: 0 }
    }

    pub fn new_id(&mut self) -> VariableId {
        let id = self.var_id;
        self.var_id = id + 1;
        id
    }

    pub fn flag(&mut self, long_name: Option<&str>, short_name: Option<&str>) -> Parameter {
        Parameter {
            id: self.new_id(),
            par_type: TypeRef::Flag,
            long_name: long_name.map(Identifier::from),
            short_name: short_name.map(Identifier::from),
        }
    }

    pub fn opt_par(&mut self, long_name: &str, short_name: Option<&str>, t: &TypeRef) -> Parameter {
        Parameter {
            id: self.new_id(),
            par_type: TypeRef::Optional(Box::from(t.clone())),
            long_name: Some(Identifier::from(long_name)),
            short_name: short_name.map(Identifier::from),
        }
    }

    pub fn req_par(&mut self, long_name: &str, short_name: Option<&str>, t: &TypeRef) -> Parameter {
        Parameter {
            id: self.new_id(),
            par_type: t.clone(),
            long_name: Some(Identifier::from(long_name)),
            short_name: short_name.map(Identifier::from),
        }
    }

    pub fn get_string(
        arguments: &mut Vec<Value>,
        par: &Parameter,
    ) -> InterpResult<std::rc::Rc<str>> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::String(value) = arg {
            Ok(value)
        } else {
            Err(InterpretationError::ArgumentTypeMismatch {
                par: par.clone(),
                args: arguments.clone(),
            })
        }
    }

    pub fn get_boolean(arguments: &mut Vec<Value>, par: &Parameter) -> InterpResult<bool> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Boolean(value) = arg {
            Ok(value)
        } else {
            Err(InterpretationError::ArgumentTypeMismatch {
                par: par.clone(),
                args: arguments.clone(),
            })
        }
    }

    pub fn get_int(arguments: &mut Vec<Value>, par: &Parameter) -> InterpResult<i32> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Int(value) = arg {
            Ok(value)
        } else {
            Err(InterpretationError::ArgumentTypeMismatch {
                par: par.clone(),
                args: arguments.clone(),
            })
        }
    }

    pub fn get_fn(arguments: &mut Vec<Value>, par: &Parameter) -> InterpResult<FunctionBody> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Function(value) = arg {
            Ok(value)
        } else {
            Err(InterpretationError::ArgumentTypeMismatch {
                par: par.clone(),
                args: arguments.clone(),
            })
        }
    }

    pub fn get_tuple(arguments: &mut Vec<Value>, par: &Parameter) -> InterpResult<Vec<Value>> {
        let arg = std::mem::replace(&mut arguments[par.id], Value::Nothing);

        if let Value::Tuple(value) = arg {
            Ok(value)
        } else {
            Err(InterpretationError::ArgumentTypeMismatch {
                par: par.clone(),
                args: arguments.clone(),
            })
        }
    }
}
