use simple_error::{SimpleError, SimpleResult};

use crate::symbolization::ast::*;

pub struct Interpreter {
    program: Program,
}

impl Interpreter {
    pub fn new(program: Program) -> Interpreter {
        Interpreter { program }
    }

    pub fn execute(&mut self, function: &str) {
        let to_execute = self.resolve_function(function);
    }

    fn resolve_function(&self, full_name: &str) -> SimpleResult<&FunctionBody> {
        let mut full_function_scope: Vec<&str> = full_name.split(".").collect();
        let function_name = full_function_scope.pop().unwrap();

        let mut target_scope = &self.program.namespaces;
        for ele in full_function_scope {
            target_scope = target_scope
                .scopes
                .get(ele)
                .ok_or_else(|| SimpleError::new(format!("scope {ele} did not exist")))?;
        }
        let function_id = target_scope
            .functions
            .get(function_name)
            .ok_or_else(|| SimpleError::new(format!("function {function_name} did not exist")))?;

        let to_execute = self.program.function_definitions.get(function_id);
        to_execute.ok_or_else(|| SimpleError::new("Internal error (function exists, but definition is not found)"))
    }
}
