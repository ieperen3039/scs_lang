use std::collections::HashMap;

use crate::{
    interpretation::meta_structures::{InterpResult, Value},
    symbolization::{ast, function_collector::FunctionCollector},
};

pub mod echo;
pub mod math;

pub type InternalFunctions = HashMap<ast::FunctionId, Box<dyn InternalFunction>>;

pub fn build_functions(fc: &mut FunctionCollector) -> InternalFunctions {
    HashMap::from([
        build_function::<math::FnAdd>(fc),
        build_function::<math::FnSub>(fc),
        build_function::<math::FnMul>(fc),
        build_function::<math::FnDiv>(fc),
    ])
}

pub fn build_function<FnType: InternalFunction + 'static>(
    fc: &mut FunctionCollector,
) -> (ast::FunctionId, Box<dyn InternalFunction>) {
    let id = fc.new_id();
    (id, Box::new(FnType::new(id)))
}

pub trait InternalFunction {
    fn new(function_id: ast::FunctionId) -> Self
    where
        Self: Sized;

    fn get_declaration(&self) -> ast::FunctionDeclaration;

    fn call(&self, arguments: Vec<Value>) -> InterpResult<Value>;
}

pub fn get_functions(functions: &InternalFunctions) -> ast::Namespace {
    let mut namespace = ast::Namespace::new_root();
    for fn_def in functions.values() {
        namespace.add_function(fn_def.get_declaration());
    }
    namespace
}
