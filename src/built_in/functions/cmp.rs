use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{
        interperation_result::{InterpResult, InterpretationError},
        interpreter::Interpreter,
        value::*,
    },
    symbolization::ast::*,
};

pub struct FnEqual {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnLessThan {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

pub struct FnGreaterThan {
    decl: FunctionDeclaration,
    par_left: Parameter,
    par_right: Parameter,
}

impl InternalFunction for FnEqual {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_type: Identifier = Identifier::from("T");

        let par_left = builder.req_par_s("left", "l", &TypeRef::GenericName(generic_type.clone()));
        let par_right =
            builder.req_par_s("right", "r", &TypeRef::GenericName(generic_type.clone()));
        FnEqual {
            decl: FunctionDeclaration::new_native(
                function_id,
                "equals",
                vec![generic_type.clone()],
                vec![&par_left, &par_right],
                &TypeRef::boolean(),
            ),
            par_left,
            par_right,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
    }

    fn call(&self, arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        return compare_equal(&arguments[self.par_left.id], &arguments[self.par_right.id])
            .map(Value::new_boolean);
    }
}

impl InternalFunction for FnLessThan {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_type: Identifier = Identifier::from("T");

        let par_left = builder.req_par_s("left", "l", &TypeRef::GenericName(generic_type.clone()));
        let par_right =
            builder.req_par_s("right", "r", &TypeRef::GenericName(generic_type.clone()));
        FnLessThan {
            decl: FunctionDeclaration::new_native(
                function_id,
                "less_than",
                vec![generic_type.clone()],
                vec![&par_left, &par_right],
                &TypeRef::boolean(),
            ),
            par_left,
            par_right,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
    }

    fn call(&self, arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        return compare_less(&arguments[self.par_left.id], &arguments[self.par_right.id])
            .map(Value::new_boolean);
    }
}

impl InternalFunction for FnGreaterThan {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_type: Identifier = Identifier::from("T");

        let par_left = builder.req_par_s("left", "l", &TypeRef::GenericName(generic_type.clone()));
        let par_right =
            builder.req_par_s("right", "r", &TypeRef::GenericName(generic_type.clone()));
        FnGreaterThan {
            decl: FunctionDeclaration::new_native(
                function_id,
                "greater_than",
                vec![generic_type.clone()],
                vec![&par_left, &par_right],
                &TypeRef::boolean(),
            ),
            par_left,
            par_right,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
    }

    fn call(&self, arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        return compare_greater(&arguments[self.par_left.id], &arguments[self.par_right.id])
            .map(Value::new_boolean);
    }
}

fn compare_less(left: &Value, right: &Value) -> InterpResult<bool> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => Ok(left < right),
        (Value::Float(left), Value::Float(right)) => Ok(left < right),
        (Value::Tuple(left), Value::Tuple(right)) => {
            assert_eq!(left.len(), right.len());
            for pair in left.into_iter().zip(right.into_iter()) {
                if let Ok(true) = compare_equal(pair.0, pair.1) {
                    continue;
                }

                return compare_less(pair.0, pair.1);
            }
            // equal
            Ok(false)
        },
        _ => Err(InterpretationError::InternalError(format!(
            "Try to compare type {left:?} and type {right:?}"
        ))),
    }
}

fn compare_greater(left: &Value, right: &Value) -> InterpResult<bool> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => Ok(left > right),
        (Value::Float(left), Value::Float(right)) => Ok(left > right),
        (Value::Tuple(left), Value::Tuple(right)) => {
            assert_eq!(left.len(), right.len());
            for pair in left.into_iter().zip(right.into_iter()) {
                if let Ok(true) = compare_equal(pair.0, pair.1) {
                    continue;
                }

                return compare_greater(pair.0, pair.1);
            }
            // equal
            Ok(false)
        },
        _ => Err(InterpretationError::InternalError(format!(
            "Try to compare type {left:?} and type {right:?}"
        ))),
    }
}

fn compare_equal(left: &Value, right: &Value) -> InterpResult<bool> {
    match (left, right) {
        (Value::String(left), Value::String(right)) => Ok(left == right),
        (Value::Int(left), Value::Int(right)) => Ok(left == right),
        (Value::Float(left), Value::Float(right)) => Ok(left == right),
        (Value::Tuple(left), Value::Tuple(right)) => {
            assert_eq!(left.len(), right.len());
            for pair in left.into_iter().zip(right.into_iter()) {
                if let Ok(false) = compare_equal(pair.0, pair.1) {
                    return Ok(false);
                }
            }
            Ok(true)
        },
        _ => Err(InterpretationError::InternalError(format!(
            "Try to compare type {left:?} and type {right:?}"
        ))),
    }
}
