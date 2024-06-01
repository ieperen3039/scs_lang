use crate::{
    built_in::functions::{FunctionBuilder, InternalFunction},
    interpretation::meta_structures::*,
    symbolization::{ast::*, function_collector::FunctionCollector},
};

struct FnEcho {
    fn_id: FunctionId,
    par_in: Parameter,
    par_error: Parameter,
}

impl InternalFunction for FnEcho {
    fn new(collector: &mut FunctionCollector) -> Self {
        let mut builder = FunctionBuilder::new();

        FnEcho {
            fn_id: collector.new_id(),
            par_in: builder.req_par("in", None, &TypeRef::STRING),
            par_error: builder.flag(Some("error"), Some("e")),
        }
    }

    fn get_declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration {
            id: self.fn_id,
            name: Identifier::from("echo"),
            parameters: vec![self.par_in.clone(), self.par_error.clone()],
            return_type: TypeRef::STRING.clone(),
            is_external: true,
        }
    }

    fn call(&self, mut arguments: Vec<Value>) -> InterpResult<Value> {
        let val_in = FunctionBuilder::get_string(&mut arguments, &self.par_in)?;
        let val_error = FunctionBuilder::get_boolean(&mut arguments, &self.par_error)?;

        if val_error {
            eprintln!("{val_in}");
        } else {
            println!("{val_in}");
        }

        Ok(Value::String(val_in))
    }
}
