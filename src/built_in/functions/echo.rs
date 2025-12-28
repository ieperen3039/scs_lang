use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{interperation_result::InterpResult, interpreter::Interpreter, value::*},
    symbolization::ast::*,
};

pub struct FnEcho {
    decl: FunctionDeclaration,
    par_in: Parameter,
    par_error: Parameter,
}

impl InternalFunction for FnEcho {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        let par_in = builder.req_par("in", &TypeRef::STRING);
        let par_error = builder.flag("error");
        FnEcho {
            decl: FunctionDeclaration::new_native(
                function_id,
                "echo",
                Vec::new(),
                vec![&par_in, &par_error],
                &TypeRef::STRING,
            ),
            par_in,
            par_error,
        }
    }

    fn get_declaration(&self) -> &FunctionDeclaration { &self.decl }

    fn call(&self, mut arguments: Vec<Value>, _: &Interpreter) -> InterpResult<Value> {
        let val_in = FunctionBuilder::get_string(&mut arguments, &self.par_in);
        let val_error = FunctionBuilder::get_boolean(&mut arguments, &self.par_error);

        if val_error {
            eprintln!("{val_in}");
        } else {
            println!("{val_in}");
        }

        Ok(Value::String(val_in))
    }
}
