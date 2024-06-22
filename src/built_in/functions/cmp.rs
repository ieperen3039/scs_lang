use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{interperation_result::InterpResult, value::*},
    symbolization::ast::*,
};

pub struct FnLessThan {
    function_id: NativeFunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnGreaterThan {
    function_id: NativeFunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

impl InternalFunction for FnLessThan {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnLessThan {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "less_than",
            &[&self.par_left, &self.par_right],
            &TypeRef::BOOLEAN,
        )
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left);
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right);
        return Ok(Value::Boolean(left < right));
    }
}

impl InternalFunction for FnGreaterThan {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnGreaterThan {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "greater_than",
            &[&self.par_left, &self.par_right],
            &TypeRef::BOOLEAN,
        )
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left);
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right);
        return Ok(Value::Boolean(left > right));
    }
}