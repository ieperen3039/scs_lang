use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{interperation_result::InterpResult, interpreter::Interpreter, value::*},
    symbolization::{
        ast::*,
        ast_util::{
            OPTION_VARIANT_ID_NONE, OPTION_VARIANT_ID_SOME, RESULT_VARIANT_ID_NEG,
            RESULT_VARIANT_ID_POS,
        },
    },
};

pub struct FnIfPosResult {
    function_id: NativeFunctionId,
    par_target: Parameter,
    par_action: Parameter,
    result_type: TypeRef,
}

pub struct FnIfNegResult {
    function_id: NativeFunctionId,
    par_target: Parameter,
    par_action: Parameter,
    result_type: TypeRef,
}

pub struct FnIfSomeOption {
    function_id: NativeFunctionId,
    par_target: Parameter,
    par_action: Parameter,
    option_type: TypeRef,
}

pub struct FnIfNoneOption {
    function_id: NativeFunctionId,
    par_target: Parameter,
    par_action: Parameter,
    option_type: TypeRef,
}

impl InternalFunction for FnIfPosResult {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_pos_type = Identifier::from("P");
        let generic_neg_type = Identifier::from("N");

        let result_type = TypeRef::Result(
            Box::from(TypeRef::GenericName(generic_pos_type.clone())),
            Box::from(TypeRef::GenericName(generic_neg_type.clone())),
        );

        FnIfPosResult {
            function_id,
            par_target: builder.req_par("target", &result_type),
            par_action: builder.req_par(
                "action",
                &TypeRef::Function(FunctionType {
                    parameters: vec![TypeRef::GenericName(generic_pos_type.clone())],
                    return_type: Box::from(TypeRef::GenericName(generic_pos_type)),
                }),
            ),
            result_type,
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "map_pos",
            Vec::new(),
            vec![&self.par_target, &self.par_action],
            &self.result_type,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        apply_monadic(
            arguments,
            &self.par_action,
            &self.par_target,
            RESULT_VARIANT_ID_POS,
            interpreter,
        )
    }

    fn as_operator(&self) -> Option<Identifier> {
        Some(Identifier::from("?"))
    }
}

impl InternalFunction for FnIfNegResult {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_pos_type = Identifier::from("P");
        let generic_neg_type = Identifier::from("N");

        let result_type = TypeRef::Result(
            Box::from(TypeRef::GenericName(generic_pos_type.clone())),
            Box::from(TypeRef::GenericName(generic_neg_type.clone())),
        );

        FnIfNegResult {
            function_id,
            par_target: builder.req_par("target", &result_type),
            par_action: builder.req_par(
                "action",
                &TypeRef::Function(FunctionType {
                    parameters: vec![TypeRef::GenericName(generic_neg_type.clone())],
                    return_type: Box::from(TypeRef::GenericName(generic_neg_type)),
                }),
            ),
            result_type,
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "map_neg",
            Vec::new(),
            vec![&self.par_target, &self.par_action],
            &self.result_type,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        apply_monadic(
            arguments,
            &self.par_action,
            &self.par_target,
            RESULT_VARIANT_ID_NEG,
            interpreter,
        )
    }

    fn as_operator(&self) -> Option<Identifier> {
        Some(Identifier::from("!"))
    }
}

impl InternalFunction for FnIfSomeOption {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_some_type = Identifier::from("T");

        let option_type = TypeRef::new_optional(TypeRef::GenericName(generic_some_type.clone()));

        FnIfSomeOption {
            function_id,
            par_target: builder.req_par("target", &option_type),
            par_action: builder.req_par(
                "action",
                &TypeRef::Function(FunctionType {
                    parameters: vec![TypeRef::GenericName(generic_some_type.clone())],
                    return_type: Box::from(TypeRef::GenericName(generic_some_type)),
                }),
            ),
            option_type,
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "map",
            Vec::new(),
            vec![&self.par_target, &self.par_action],
            &self.option_type,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        apply_monadic(
            arguments,
            &self.par_action,
            &self.par_target,
            OPTION_VARIANT_ID_SOME,
            interpreter,
        )
    }

    fn as_operator(&self) -> Option<Identifier> {
        Some(Identifier::from("?"))
    }
}

impl InternalFunction for FnIfNoneOption {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let generic_some_type = Identifier::from("T");

        let option_type = TypeRef::new_optional(TypeRef::GenericName(generic_some_type.clone()));

        FnIfNoneOption {
            function_id,
            par_target: builder.req_par("target", &option_type),
            par_action: builder.req_par(
                "action",
                &TypeRef::Function(FunctionType {
                    parameters: vec![TypeRef::GenericName(generic_some_type.clone())],
                    return_type: Box::from(TypeRef::GenericName(generic_some_type)),
                }),
            ),
            option_type,
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new_native(
            self.function_id,
            "if_none",
            Vec::new(),
            vec![&self.par_target, &self.par_action],
            &self.option_type,
        )
    }

    fn call(&self, arguments: Vec<Value>, interpreter: &Interpreter) -> InterpResult<Value> {
        // the Value of the None variant is always Value::Nothing,
        // but apply_monadic does not check on that, thus evaluate_function_call will be called with expr_value = Value::Nothing
        apply_monadic(
            arguments,
            &self.par_action,
            &self.par_target,
            OPTION_VARIANT_ID_NONE,
            interpreter,
        )
    }

    fn as_operator(&self) -> Option<Identifier> {
        Some(Identifier::from("!"))
    }
}

fn apply_monadic(
    mut arguments: Vec<Value>,
    par_action: &Parameter,
    par_target: &Parameter,
    id: u32,
    interpreter: &Interpreter,
) -> InterpResult<Value> {
    let val_action = FunctionBuilder::get_fn(&mut arguments, par_action);
    let val_target = FunctionBuilder::get_variant_if_present(&mut arguments, par_target, id);

    if val_target.is_none() {
        FunctionBuilder::get_value(&mut arguments, &par_target);
    }

    let (function, mut stack_frame) = val_action;

    let new_pos_value = interpreter.evaluate_function_call(
        function,
        &Vec::new(),
        val_target.unwrap(),
        &mut stack_frame,
    )?;

    Ok(Value::Variant(id, Box::from(new_pos_value)))
}
