use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{interperation_result::InterpResult, interpreter::Interpreter, value::*},
    symbolization::ast::*,
};

pub struct FnAdd {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnSub {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnMul {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnDiv {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnSqrt {
    decl: FunctionDeclaration,
    par_value: Parameter,
}

impl InternalFunction for FnAdd {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        let par_left = builder.req_par_s("left", "l", &TypeRef::INT);
        let par_right = builder.req_par_s("right", "r", &TypeRef::INT);
        FnAdd {
            decl: FunctionDeclaration::new_native(
                function_id,
                "add",
                Vec::new(),
                vec![&par_left, &par_right],
                &TypeRef::INT,
            ),
            par_left,
            par_right,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
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

        let par_left = builder.req_par_s("left", "l", &TypeRef::INT);
        let par_right = builder.req_par_s("right", "r", &TypeRef::INT);
        FnSub {
            decl: FunctionDeclaration::new_native(
                function_id,
                "sub",
                Vec::new(),
                vec![&par_left, &par_right],
                &TypeRef::INT,
            ),
            par_left,
            par_right,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
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

        let par_left = builder.req_par_s("left", "l", &TypeRef::INT);
        let par_right = builder.req_par_s("right", "r", &TypeRef::INT);
        FnMul {
            decl: FunctionDeclaration::new_native(
                function_id,
                "mul",
                Vec::new(),
                vec![&par_left, &par_right],
                &TypeRef::INT,
            ),
            par_left,
            par_right,
        }
    }
    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
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

        let par_left = builder.req_par_s("left", "l", &TypeRef::INT);
        let par_right = builder.req_par_s("right", "r", &TypeRef::INT);
        FnDiv {
            decl: FunctionDeclaration::new_native(
                function_id,
                "div",
                Vec::new(),
                vec![&par_left, &par_right],
                &TypeRef::INT,
            ),
            par_left,
            par_right,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
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

        let par_value = builder.req_par_s("value", "v", &TypeRef::INT);
        FnSqrt {
            decl: FunctionDeclaration::new_native(
                function_id,
                "sqrt",
                Vec::new(),
                vec![&par_value],
                &TypeRef::INT,
            ),
            par_value,
        }
    }
    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
    }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_value);
        return Ok(Value::Int(f32::sqrt(left as f32) as i32));
    }
}
