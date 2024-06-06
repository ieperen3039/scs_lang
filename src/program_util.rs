use crate::symbolization::{ast::*, semantic_result::{SemanticError, SemanticResult}};


impl FileAst {
    // only used to resolve the function to call, all subsequent functions are already resolved in the parsing stage
    pub fn resolve_function(&self, full_name: &str) -> SemanticResult<&FunctionBody> {
        let mut full_function_scope: Vec<&str> = full_name.split(".").collect();
        let function_name = full_function_scope.pop().unwrap();

        let mut target_scope = &self.namespaces;
        for ele in full_function_scope {
            target_scope =
                target_scope
                    .namespaces
                    .get(ele)
                    .ok_or_else(|| SemanticError::SymbolNotFound {
                        kind: "namespace",
                        symbol: Identifier::from(ele),
                    })?;
        }
        let function_decl = target_scope.functions.get(function_name).ok_or_else(|| {
            SemanticError::SymbolNotFound {
                kind: "function",
                symbol: Identifier::from(function_name),
            }
        })?;

        let to_execute = self.function_definitions.get(&function_decl.id);

        to_execute.ok_or_else(|| SemanticError::SymbolNotFound {
            kind: "function",
            symbol: Identifier::from(function_name),
        })
    }
}
