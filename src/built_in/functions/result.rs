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
    decl: FunctionDeclaration,
    par_target: Parameter,
    par_action: Parameter,
}

pub struct FnIfNegResult {
    decl: FunctionDeclaration,
    par_target: Parameter,
    par_action: Parameter,
}

pub struct FnIfSomeOption {
    decl: FunctionDeclaration,
    par_target: Parameter,
    par_action: Parameter,
}

pub struct FnIfNoneOption {
    decl: FunctionDeclaration,
    par_target: Parameter,
    par_action: Parameter,
}

impl InternalFunction for FnIfPosResult {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();
        let pos_generic_type = Identifier::from("P");
        let neg_generic_type = Identifier::from("N");
        let return_generic_type = Identifier::from("R");

        let result_type = TypeRef::Result(
            Box::from(TypeRef::GenericName(pos_generic_type.clone())),
            Box::from(TypeRef::GenericName(neg_generic_type.clone())),
        );

        let par_target = builder.req_par("target", &result_type);
        let par_action = builder.req_par(
            "action",
            &TypeRef::Function(FunctionType {
                parameters: vec![TypeRef::GenericName(pos_generic_type.clone())],
                return_type: Box::from(TypeRef::GenericName(return_generic_type.clone())),
            }),
        );

        FnIfPosResult {
            decl: FunctionDeclaration::new_native(
                function_id,
                "map_pos",
                vec![pos_generic_type, neg_generic_type, return_generic_type],
                vec![&par_target, &par_action],
                &result_type,
            ),
            par_target,
            par_action,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration { &self.decl }

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
        let pos_generic_type = Identifier::from("P");
        let neg_generic_type = Identifier::from("N");
        let return_generic_type = Identifier::from("R");

        let result_type = TypeRef::Result(
            Box::from(TypeRef::GenericName(pos_generic_type.clone())),
            Box::from(TypeRef::GenericName(neg_generic_type.clone())),
        );

        let par_target = builder.req_par("target", &result_type);
        let par_action = builder.req_par(
            "action",
            &TypeRef::Function(FunctionType {
                parameters: vec![TypeRef::GenericName(neg_generic_type.clone())],
                return_type: Box::from(TypeRef::GenericName(return_generic_type.clone())),
            }),
        );
        FnIfNegResult {
            decl: FunctionDeclaration::new_native(
                function_id,
                "map_neg",
                vec![pos_generic_type, neg_generic_type, return_generic_type],
                vec![&par_target, &par_action],
                &result_type,
            ),
            par_target,
            par_action,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration {
        &self.decl
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
        let some_generic_type = Identifier::from("T");
        let return_generic_type = Identifier::from("R");

        let option_type = TypeRef::new_optional(TypeRef::GenericName(some_generic_type.clone()));

        let par_target = builder.req_par("target", &option_type);
        let par_action = builder.req_par(
            "action",
            &TypeRef::Function(FunctionType {
                parameters: vec![TypeRef::GenericName(some_generic_type.clone())],
                return_type: Box::from(TypeRef::GenericName(return_generic_type.clone())),
            }),
        );
        FnIfSomeOption {
            decl: FunctionDeclaration::new_native(
                function_id,
                "map",
                vec![some_generic_type, return_generic_type],
                vec![&par_target, &par_action],
                &option_type,
            ),
            par_target,
            par_action,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration { &self.decl }

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
        let some_generic_type = Identifier::from("T");
        let return_generic_type = Identifier::from("R");

        let option_type = TypeRef::new_optional(TypeRef::GenericName(some_generic_type.clone()));
        let par_target = builder.req_par("target", &option_type);
        let par_action = builder.req_par(
            "action",
            &TypeRef::Function(FunctionType {
                parameters: vec![TypeRef::GenericName(some_generic_type.clone())],
                return_type: Box::from(TypeRef::GenericName(return_generic_type.clone())),
            }),
        );
        FnIfNoneOption {
            decl: FunctionDeclaration::new_native(
                function_id,
                "if_none",
                vec![some_generic_type, return_generic_type],
                vec![&par_target, &par_action],
                &option_type,
            ),
            par_target,
            par_action,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration { &self.decl }

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
