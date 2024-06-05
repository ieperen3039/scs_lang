use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::meta_structures::*,
    symbolization::ast::*,
};

pub struct FnAdd {
    function_id: FunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnSub {
    function_id: FunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnMul {
    function_id: FunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnDiv {
    function_id: FunctionId,
    par_left: Parameter,
    par_right: Parameter,
}

impl InternalFunction for FnAdd {
    fn new(function_id: FunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnAdd {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            id: self.function_id,
            name: Identifier::from("add"),
            parameters: FunctionBuilder::sorted(&[&self.par_left, &self.par_right]),
            return_type: TypeRef::INT.clone(),
            is_native: true,
        }
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left)?;
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right)?;
        return Ok(Value::Int(left + right));
    }
}

impl InternalFunction for FnSub {
    fn new(function_id: FunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnSub {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            id: self.function_id,
            name: Identifier::from("sub"),
            parameters: FunctionBuilder::sorted(&[&self.par_left, &self.par_right]),
            return_type: TypeRef::INT.clone(),
            is_native: true,
        }
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left)?;
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right)?;
        return Ok(Value::Int(left - right));
    }
}

impl InternalFunction for FnMul {
    fn new(function_id: FunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnMul {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            id: self.function_id,
            name: Identifier::from("mul"),
            parameters: FunctionBuilder::sorted(&[&self.par_left, &self.par_right]),
            return_type: TypeRef::INT.clone(),
            is_native: true,
        }
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left)?;
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right)?;
        return Ok(Value::Int(left * right));
    }
}

impl InternalFunction for FnDiv {
    fn new(function_id: FunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnDiv {
            function_id,
            par_left: builder.req_par_s("left", "l", &TypeRef::INT),
            par_right: builder.req_par_s("right", "r", &TypeRef::INT),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            id: self.function_id,
            name: Identifier::from("div"),
            parameters: FunctionBuilder::sorted(&[&self.par_left, &self.par_right]),
            return_type: TypeRef::INT.clone(),
            is_native: true,
        }
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let left = FunctionBuilder::get_int(&mut arguments, &self.par_left)?;
        let right = FunctionBuilder::get_int(&mut arguments, &self.par_right)?;
        return Ok(Value::Int(left / right));
    }
}
