use crate::{
    built_in::{function_builder::FunctionBuilder, functions::InternalFunction},
    interpretation::{value::*, interperation_result::InterpResult},
    symbolization::ast::*,
};

struct FnEcho {
    function_id: NativeFunctionId,
    par_in: Parameter,
    par_error: Parameter,
}

impl InternalFunction for FnEcho {
    fn new(function_id: NativeFunctionId) -> Self {
        let mut builder = FunctionBuilder::new();

        FnEcho {
            function_id,
            par_in: builder.req_par("in", &TypeRef::STRING),
            par_error: builder.flag(Some("error"), Some("e")),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            id: GlobalFunctionTarget::Native(self.function_id),
            name: Identifier::from("echo"),
            parameters: FunctionBuilder::sorted(&[&self.par_in, &self.par_error]),
            return_type: TypeRef::STRING.clone(),
        }
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
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
