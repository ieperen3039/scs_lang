use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{interperation_result::InterpResult, interpreter::Interpreter, value::*},
    symbolization::ast::*,
};

pub struct FnAdd {
    function_id: NativeFunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnSub {
    function_id: NativeFunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnMul {
    function_id: NativeFunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnDiv {
    function_id: NativeFunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnSqrt {
    function_id: NativeFunctionId,
    par_value: Parameter,
}

impl InternalFunction for FnAdd {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnAdd {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "add",
            Vec::new(),
            vec![&self.par_left, &self.par_right],
            &TypeRef::INT,
        )
    }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left);
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right);
        return Ok(Value::Int(left + right));
    }
}

impl InternalFunction for FnSub {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnSub {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "sub",
            Vec::new(),
            vec![&self.par_left, &self.par_right],
            &TypeRef::INT,
        )
    }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left);
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right);
        return Ok(Value::Int(left - right));
    }
}

impl InternalFunction for FnMul {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnMul {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "mul",
            Vec::new(),
            vec![&self.par_left, &self.par_right],
            &TypeRef::INT,
        )
    }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left);
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right);
        return Ok(Value::Int(left * right));
    }
}

impl InternalFunction for FnDiv {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnDiv {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "div",
            Vec::new(),
            vec![&self.par_left, &self.par_right],
            &TypeRef::INT,
        )
    }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left);
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right);
        return Ok(Value::Int(left / right));
    }
}

impl InternalFunction for FnSqrt {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnSqrt {
            function_id,
            par_value: builder.req_par_s("value", "v", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "sqrt",
            Vec::new(),
            vec![&self.par_value],
            &TypeRef::INT,
        )
    }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_value);
        return Ok(Value::Int(f32::sqrt(left as f32) as i32));
    }
}
