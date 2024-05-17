use simple_error::SimpleError;

use crate::symbolization::ast::*;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Int(i32),
    Function(FunctionBody),
    InternalFunction(InternalFunction),
    Result(Box<(Value, Value)>)
}

#[derive(Debug, Clone)]
pub struct InternalFunction {
    pub func : FauxFunction,
    pub parameters: Vec<Parameter>
}

pub type FauxFunction = fn(Vec<(Parameter, Value)>) -> FauxResult<Value>;

pub type FauxResult<T> = std::result::Result<T, InterpretationError>;

#[derive(Debug, Clone)]
pub struct Parameter {
    pub par_type: ParameterType,
    pub long_name: Option<String>,
    pub short_name: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ParameterType {
    Required(TypeRef),
    Optional(TypeRef),
    Flag,
}

impl Parameter {
    pub fn to_type(&self) -> TypeRef {
        match &self.par_type {
            ParameterType::Required(t) | ParameterType::Optional(t) => t.clone(),
            ParameterType::Flag => TypeRef::BOOLEAN,
        }
    }
}

pub enum InterpretationError {
    TypeError { expected: TypeRef, found: TypeRef },
    ArgumentRequiredError(Parameter),
    InternalError(SimpleError),
}