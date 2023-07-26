use std::{collections::HashMap, rc::Rc};

use super::{ast::*, built_in_types::{TYPE_ID_STRING, TYPE_ID_INT}};

impl TypeDefinition {
}

impl TypeRef {
    pub const STRING: TypeRef = TypeRef::Defined(DefinedRef {
        id: TYPE_ID_STRING,
        generic_parameters: Vec::new(),
    });

    pub const NUMBER: TypeRef = TypeRef::Defined(DefinedRef {
        id: TYPE_ID_INT,
        generic_parameters: Vec::new(),
    });
}

impl Expression {
    pub fn get_type(&self) -> TypeRef {
        match &self {
            Expression::StaticFunctionCall(fun) => fun.function.body.return_var.var_type.clone(),
            Expression::FunctionBlock(block) => block.return_var.var_type.clone(),
            Expression::Array(array) => array.element_type.clone(),
            Expression::Literal(Literal::String(_)) => TypeRef::STRING.clone(),
            Expression::Literal(Literal::Number(_)) => TypeRef::NUMBER.clone(),
            Expression::Variable(var) => var.var_type.clone(),
        }
    }
}

impl Scope {
    pub fn new(name: &str, parent: Option<&Scope>) -> Scope {
        let mut full_name = parent.map(|sc| sc.full_name.clone()).unwrap_or(Vec::new());
        full_name.push(Rc::from(name));

        Scope {
            full_name,
            scopes: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn add_sub_scope(&mut self, scope_to_add : Scope) {
        self.scopes.insert(scope_to_add.get_name(), scope_to_add);
    }

    pub fn add_type(&mut self, type_to_add : TypeDefinition) {
        self.types.insert(type_to_add.name.clone(), type_to_add);
    }

    pub fn add_function(&mut self, fn_to_add : FunctionDefinition) {
        self.functions.insert(fn_to_add.name.clone(), fn_to_add);
    }

    pub fn get_name(&self) -> Identifier {
        self.full_name.last().unwrap().to_owned()
    }

    pub fn extend(&mut self, other: Scope) {
        self.scopes.extend(other.scopes);
        self.types.extend(other.types);
    }

    pub fn combined_with(mut self, other: Scope) -> Scope {
        self.extend(other);
        self
    }

    pub fn get(&self, name: &str) -> Option<&TypeDefinition> {
        self.types.get(name)
    }
}
