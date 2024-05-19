use std::{collections::HashMap, rc::Rc};

use crate::built_in::primitives::*;

use super::ast::*;

impl TypeDefinition {}

impl TypeRef {
    pub const STRING: TypeRef = Self::from(TYPE_ID_STRING);
    pub const NUMBER: TypeRef = Self::from(TYPE_ID_INT);
    pub const FLOAT: TypeRef = Self::from(TYPE_ID_FLOAT);
    pub const BOOLEAN: TypeRef = Self::from(TYPE_ID_BOOLEAN);

    pub const fn from(id: NumericTypeIdentifier) -> TypeRef {
        TypeRef::Defined(DefinedRef { id })
    }
}

impl FunctionExpression {
    pub fn get_type(
        &self,
        functions: &HashMap<NumericFunctionIdentifier, FunctionDeclaration>,
    ) -> TypeRef {
        match &self {
            FunctionExpression::FunctionCall(fc) => functions
                .get(&fc.id)
                .map(|f| f.return_type.clone())
                .unwrap_or(TypeRef::Void), // TODO: unknown function: is this an error?
            FunctionExpression::Assignment(_) => TypeRef::Void,
        }
    }
}

impl ValueExpression {
    pub fn get_type(
        &self,
        functions: &HashMap<NumericFunctionIdentifier, FunctionDeclaration>,
    ) -> TypeRef {
        match &self {
            ValueExpression::FunctionBody(block) => block.return_var.var_type.clone(),
            ValueExpression::Tuple(buffer_init) => buffer_init.element_type.clone(),
            ValueExpression::Literal(Literal::String(_)) => TypeRef::STRING.clone(),
            ValueExpression::Literal(Literal::Number(_)) => TypeRef::NUMBER.clone(),
            ValueExpression::Variable(var) => var.var_type.clone(),
        }
    }
}

impl Namespace {
    pub fn new(name: &str, parent: Option<&Namespace>) -> Namespace {
        let mut full_name = parent.map(|sc| sc.full_name.clone()).unwrap_or(Vec::new());
        full_name.push(Rc::from(name));

        Namespace {
            full_name,
            namespaces: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn add_sub_scope(&mut self, scope_to_add: Namespace) {
        self.namespaces.insert(scope_to_add.get_name(), scope_to_add);
    }

    pub fn add_function(&mut self, fn_to_add: &FunctionDeclaration) {
        self.functions.insert(fn_to_add.name.clone(), fn_to_add.id);
    }

    pub fn add_type(&mut self, type_to_add: &TypeDefinition) {
        self.types.insert(type_to_add.name.clone(), type_to_add.id);
    }

    pub fn get_name(&self) -> Identifier {
        self.full_name.last().unwrap().to_owned()
    }

    pub fn extend(&mut self, other: Namespace) {
        self.namespaces.extend(other.namespaces);
        self.types.extend(other.types);
    }

    pub fn combined_with(&self, other: Namespace) -> Namespace {
        let mut new_self = self.clone();
        new_self.extend(other);
        new_self
    }

    pub fn get(&self, name: &str) -> Option<&NumericTypeIdentifier> {
        self.types.get(name)
    }
}

impl Parameter {
    pub fn to_type(&self) -> &TypeRef {
        &self.par_type
    }

    pub fn identifier(&self) -> &Identifier {
        &self.long_name.or(self.short_name).unwrap()
    }
}
