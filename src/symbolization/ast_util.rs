use std::{collections::HashMap, rc::Rc};

use crate::built_in::primitives::*;

use super::ast::*;

impl TypeDefinition {}

impl TypeRef {
    pub const STRING: TypeRef = Self::from(TYPE_ID_STRING);
    pub const INT: TypeRef = Self::from(TYPE_ID_INT);
    pub const FLOAT: TypeRef = Self::from(TYPE_ID_FLOAT);
    pub const BOOLEAN: TypeRef = Self::from(TYPE_ID_BOOLEAN);

    pub const fn from(id: TypeId) -> TypeRef {
        TypeRef::Defined(DefinedRef { id })
    }

    pub fn from_fn_decl(fun: &FunctionDeclaration) -> TypeRef {
        TypeRef::Function(FunctionType {
            parameters: fun
                .parameters
                .iter()
                .map(Parameter::to_type)
                .cloned()
                .collect(),
            return_type: Box::new(fun.return_type.clone()),
        })
    }
}

impl FunctionExpression {
    pub fn get_result_type(
        &self,
        functions: &HashMap<FunctionId, FunctionDeclaration>,
    ) -> TypeRef {
        match &self {
            FunctionExpression::FunctionCall(fc) => functions
                .get(&fc.id)
                .map(|f| f.return_type.clone())
                .expect("Broken function call"), // TODO: unknown function: is this an error?
            FunctionExpression::Assignment(_) => TypeRef::Void,
            FunctionExpression::Lamda(lamda) => lamda.body.return_var.var_type.clone(),
            FunctionExpression::Operator(op) => functions
                .get(&op.id)
                .map(|f| f.return_type.clone())
                .expect("Broken function call"), // TODO: unknown function: is this an error?
        }
    }
}

impl ValueExpression {
    pub fn get_type(&self) -> TypeRef {
        match &self {
            ValueExpression::Tuple(elements) => {
                TypeRef::UnamedTuple(elements.iter().map(|ex| ex.get_type()).collect())
            },
            ValueExpression::Literal(Literal::String(_)) => TypeRef::STRING.clone(),
            ValueExpression::Literal(Literal::Number(_)) => TypeRef::INT.clone(),
            ValueExpression::Literal(Literal::Boolean(_)) => TypeRef::BOOLEAN.clone(),
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
        self.namespaces
            .insert(scope_to_add.get_name(), scope_to_add);
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

    pub fn get(&self, name: &str) -> Option<&TypeId> {
        self.types.get(name)
    }
}

impl Parameter {
    pub fn to_type(&self) -> &TypeRef {
        &self.par_type
    }

    pub fn identifier(&self) -> &VariableId {
        &self.id
    }

    pub fn name(&self) -> &Identifier {
        self.long_name
            .as_ref()
            .or(self.short_name.as_ref())
            .expect("Parameters always have either a long name or a short name")
    }
}
