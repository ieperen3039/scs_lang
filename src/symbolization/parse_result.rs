use super::ast::{self, Identifier};

#[derive(Debug)]
pub enum SemanticError {
    NodeNotFound{ expected: &'static str },
    UnexpectedNode{ found: ast::Identifier, parent_node: &'static str },
    BrokenControl(&'static str),
    TypeMismatchError { expected: ast::TypeRef, found: ast::TypeRef },
    VariableExists { name: ast::Identifier },
    ArgumentRequiredError(ast::Parameter),
    ArgumentInvalid{ arg: Identifier, function: Identifier },
    InvalidNumerOfParameters{ what: &'static str, num_found: usize, expected: String },
    SymbolNotFound { kind: &'static str, symbol: ast::Identifier },
    SymbolNotFoundInScope { kind: &'static str, symbol: ast::Identifier, scope: Vec<Identifier> },
    InternalError(&'static str),
}

pub type SemanticResult<T> = Result<T, SemanticError>;
