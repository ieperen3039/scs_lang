use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{interperation_result::{InterpResult, InterpretationError}, interpreter::Interpreter, value::*},
    symbolization::ast::*,
};

pub struct FnEqual {
    function_id: NativeFunctionId,
    generic_type: Identifier,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnLessThan {
    function_id: NativeFunctionId,
    generic_type: Identifier,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnGreaterThan {
    function_id: NativeFunctionId,
    generic_type: Identifier,
    par_left: Parameter,
    par_right: Parameter,
}

impl InternalFunction for FnEqual {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_type: Identifier = Identifier::from("T");

        FnEqual {
            function_id,
            generic_type: generic_type.clone(),
            par_left: builder.req_par_s("left", "l", &TypeRef::GenericName(generic_type.clone())),
            par_right: builder.req_par_s("right", "r", &TypeRef::GenericName(generic_type.clone())),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "equals",
            vec![self.generic_type.clone()],
            vec![&self.par_left, &self.par_right],
            &TypeRef::BOOLEAN,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        return compare_equal(&arguments[self.par_left.id], &arguments[self.par_right.id]);
    }
}

impl InternalFunction for FnLessThan {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_type: Identifier = Identifier::from("T");

        FnLessThan {
            function_id,
            generic_type: generic_type.clone(),
            par_left: builder.req_par_s("left", "l", &TypeRef::GenericName(generic_type.clone())),
            par_right: builder.req_par_s("right", "r", &TypeRef::GenericName(generic_type.clone())),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "less_than",
            vec![self.generic_type.clone()],
            vec![&self.par_left, &self.par_right],
            &TypeRef::BOOLEAN,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        return compare_less(&arguments[self.par_left.id], &arguments[self.par_right.id]);
    }
}

impl InternalFunction for FnGreaterThan {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_type: Identifier = Identifier::from("T");

        FnGreaterThan {
            function_id,
            generic_type: generic_type.clone(),
            par_left: builder.req_par_s("left", "l", &TypeRef::GenericName(generic_type.clone())),
            par_right: builder.req_par_s("right", "r", &TypeRef::GenericName(generic_type.clone())),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "greater_than",
            vec![self.generic_type.clone()],
            vec![&self.par_left, &self.par_right],
            &TypeRef::BOOLEAN,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        return compare_greater(&arguments[self.par_left.id], &arguments[self.par_right.id]);
    }
}

fn compare_less(left: &Value, right: &Value) -> InterpResult<Value> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => {
            Ok(Value::Boolean(left < right))
        },
        (Value::Float(left), Value::Float(right)) => {
            Ok(Value::Boolean(left < right))
        },
        (Value::Tuple(left), Value::Tuple(right)) => {
            assert_eq!(left.len(), right.len());
            for pair in left.into_iter().zip(right.into_iter()) {
                if let Ok(Value::Boolean(true)) = compare_equal(pair.0, pair.1) {
                    continue;
                }

                return compare_less(pair.0, pair.1);
            }
            unreachable!("Empty tuples should not exist")
        },
        _ => {
            Err(InterpretationError::InternalError(String::from("")))
        }
    }
}

fn compare_greater(left: &Value, right: &Value) -> InterpResult<Value> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => {
            Ok(Value::Boolean(left > right))
        },
        (Value::Float(left), Value::Float(right)) => {
            Ok(Value::Boolean(left > right))
        },
        (Value::Tuple(left), Value::Tuple(right)) => {
            assert_eq!(left.len(), right.len());
            for pair in left.into_iter().zip(right.into_iter()) {
                if let Ok(Value::Boolean(true)) = compare_equal(pair.0, pair.1) {
                    continue;
                }

                return compare_greater(pair.0, pair.1);
            }
            unreachable!("Empty tuples should not exist")
        },
        _ => {
            Err(InterpretationError::InternalError(String::from("")))
        }
    }
}

fn compare_equal(left: &Value, right: &Value) -> InterpResult<Value> {
    match (left, right) {
        (Value::String(left), Value::String(right)) => {
            Ok(Value::Boolean(left == right))
        }
        (Value::Int(left), Value::Int(right)) => {
            Ok(Value::Boolean(left == right))
        },
        (Value::Float(left), Value::Float(right)) => {
            Ok(Value::Boolean(left == right))
        },
        (Value::Tuple(left), Value::Tuple(right)) => {
            assert_eq!(left.len(), right.len());
            for pair in left.into_iter().zip(right.into_iter()) {
                if let Ok(Value::Boolean(false)) = compare_equal(pair.0, pair.1) {
                    return Ok(Value::Boolean(false));
                }
            }
            Ok(Value::Boolean(true))
        },
        _ => {
            Err(InterpretationError::InternalError(String::from("")))
        }
    }
}
