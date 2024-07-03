use std::collections::HashMap;

use crate::{
    interpretation::{interperation_result::InterpResult, interpreter::Interpreter, value::Value},
    symbolization::ast::{self, Identifier},
};

pub mod cmp;
pub mod echo;
pub mod math;
pub mod result;

pub type InternalFunctions = HashMap<ast::NativeFunctionId, Box<dyn InternalFunction>>;

pub fn build_functions() -> InternalFunctions {
    let mut id_gen = NativeFunctionBuilder::new();

    HashMap::from([
        id_gen.build_function::<math::FnAdd>(),
        id_gen.build_function::<math::FnSub>(),
        id_gen.build_function::<math::FnMul>(),
        id_gen.build_function::<math::FnDiv>(),
        id_gen.build_function::<math::FnSqrt>(),
        id_gen.build_function::<cmp::FnLessThan>(),
        id_gen.build_function::<cmp::FnGreaterThan>(),
    ])
}

pub struct NativeFunctionBuilder {
    next_id: ast::NativeFunctionId,
}

impl NativeFunctionBuilder {
    pub fn new() -> NativeFunctionBuilder {
        NativeFunctionBuilder { next_id: 0 }
    }

    pub fn new_id(&mut self) -> ast::NativeFunctionId {
        let id = self.next_id;
        self.next_id = id + 1;
        id
    }

    pub fn build_function<FnType: InternalFunction + 'static>(
        &mut self,
    ) -> (ast::NativeFunctionId, Box<dyn InternalFunction>) {
        let id = self.new_id();
        (id, Box::new(FnType::new(id)))
    }
}

pub trait InternalFunction {
    fn new(function_id: ast::NativeFunctionId) -> Self
    where
        Self: Sized;

    fn get_declaration(&self) -> ast::FunctionDeclaration;

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value>;

    fn as_operator(&self) -> Option<Identifier> { None }
}

pub fn get_functions(functions: &InternalFunctions) -> ast::Namespace {
    let mut namespace = ast::Namespace::new_root();
    for fn_def in functions.values() {
        namespace.add_function(fn_def.get_declaration());
    }
    namespace
}
