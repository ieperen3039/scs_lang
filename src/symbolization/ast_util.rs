use std::{collections::HashMap, ops::Deref, rc::Rc};

use crate::built_in::primitives::*;

use super::{ast::*, variable_storage::VarStorage};

pub const RESULT_VARIANT_ID_POS : u32 = 0;
pub const RESULT_VARIANT_ID_NEG : u32 = 1;
pub const OPTION_VARIANT_ID_SOME : u32 = 0;
pub const OPTION_VARIANT_ID_NONE : u32 = 1;

impl TypeRef {
    pub const STRING: TypeRef = Self::from(TYPE_ID_STRING);
    pub const INT: TypeRef = Self::from(TYPE_ID_INT);
    pub const FLOAT: TypeRef = Self::from(TYPE_ID_FLOAT);
    pub const BOOLEAN: TypeRef = Self::from(TYPE_ID_BOOLEAN);

    pub const fn from(id: TypeId) -> TypeRef {
        TypeRef::Defined(DefinedRef { id, generics: Vec::new() })
    }
}

impl FunctionType {
    pub fn from_decl(fun: &FunctionDeclaration) -> FunctionType {
        FunctionType {
            parameters: fun
                .parameters
                .iter()
                .map(Parameter::to_type)
                .cloned()
                .collect(),
            return_type: Box::new(fun.return_type.clone()),
        }
    }
}

impl FunctionExpression {
    // the type of the expression as it appears in the code
    pub fn get_type(&self) -> TypeRef {
        match &self {
            FunctionExpression::FunctionCall(fc) => {
                if fc.value_type.parameters.is_empty() {
                    // function calls that evaluate to fn<()T> are evaluated to T
                    // explicit lamdas can be used to create fn<()T>
                    fc.value_type.return_type.deref().clone()
                } else {
                    TypeRef::Function(fc.value_type.clone())
                }
            },
            FunctionExpression::Assignment(var) => TypeRef::Function(FunctionType {
                parameters: vec![var.var_type.clone()],
                return_type: Box::from(TypeRef::Break),
            }),
            FunctionExpression::Lamda(lamda) => TypeRef::Function(FunctionType {
                parameters: lamda.parameters.clone(),
                return_type: Box::from(lamda.body.return_type.clone()),
            }),
            FunctionExpression::Operator(op) => TypeRef::Function(FunctionType {
                parameters: vec![op.arg_type.clone()],
                return_type: Box::from(op.return_type.clone()),
            }),
            FunctionExpression::Cast(t) => t.clone(),
        }
    }

    // the type that this expression returns after completing all its arguments and evaluating it
    pub fn get_return_type(&self) -> TypeRef {
        match &self {
            FunctionExpression::FunctionCall(fc) => fc.value_type.return_type.deref().clone(),
            FunctionExpression::Assignment(_) => TypeRef::Break,
            FunctionExpression::Lamda(lamda) => lamda.body.return_type.clone(),
            FunctionExpression::Operator(op) => op.return_type.clone(),
            FunctionExpression::Cast(t) => t.clone(),
        }
    }
}

impl FunctionDeclaration {
    pub fn new_native(
        id: NativeFunctionId,
        name: &str,
        generics: Vec<Identifier>,
        parameters: Vec<&Parameter>,
        return_type: &TypeRef,
    ) -> FunctionDeclaration {
        let mut parameters_sorted: Vec<Parameter> =
            parameters.into_iter().map(Parameter::to_owned).collect();
        parameters_sorted.sort_unstable_by_key(|p| p.id);

        FunctionDeclaration {
            id: GlobalFunctionTarget::Native(id),
            name: Identifier::from(name),
            parameters: parameters_sorted,
            return_type: return_type.clone(),
            start_char: 0,
            generic_parameters: generics,
        }
    }

    pub fn new_native_operator(
        id: NativeFunctionId,
        symbol: &str,
        full_name: &str,
        generics: Vec<Identifier>,
        left_par_type: TypeRef,
        left_par_name: Identifier,
        right_par_type: FunctionType,
        right_par_name: Identifier,
        return_type: &TypeRef,
    ) -> FunctionDeclaration {
        FunctionDeclaration {
            id: GlobalFunctionTarget::Native(id),
            name: Identifier::from(full_name),
            parameters: vec![],
            return_type: return_type.clone(),
            start_char: 0,
            generic_parameters: generics,
        }
    }
}

impl ValueExpression {
    pub fn get_type(&self, variables: &VarStorage) -> TypeRef {
        match &self {
            ValueExpression::Tuple(elements) => {
                TypeRef::UnamedTuple(elements.iter().map(|ex| ex.get_type(variables)).collect())
            },
            ValueExpression::Literal(Literal::String(_)) => TypeRef::STRING.clone(),
            ValueExpression::Literal(Literal::Number(_)) => TypeRef::INT.clone(),
            ValueExpression::Literal(Literal::Boolean(_)) => TypeRef::BOOLEAN.clone(),
            ValueExpression::Variable(var) => variables.get_type_of(*var).clone(),
            ValueExpression::FunctionAsValue(fn_expr) => fn_expr.get_type(),
            ValueExpression::FunctionCall(fc) => fc.value_type.return_type.deref().clone(),
        }
    }
}

impl Namespace {
    pub fn new(name: &str, parent: &Namespace) -> Namespace {
        let mut full_name = parent.full_name.clone();
        full_name.push(Rc::from(name));

        Namespace {
            full_name,
            namespaces: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn new_root() -> Namespace {
        Namespace {
            full_name: Vec::new(),
            namespaces: HashMap::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn add_sub_scope(&mut self, scope_to_add: Namespace) {
        self.namespaces
            .insert(scope_to_add.get_name(), scope_to_add);
    }

    pub fn add_function(&mut self, fn_to_add: FunctionDeclaration) {
        self.functions.insert(fn_to_add.name.clone(), fn_to_add);
    }

    pub fn add_type(&mut self, type_to_add: &TypeDefinition) {
        self.types.insert(type_to_add.name.clone(), type_to_add.id);
    }

    pub fn get_name(&self) -> Identifier {
        self.full_name
            .last()
            .cloned()
            .unwrap_or_else(|| Identifier::from("root"))
    }

    pub fn extend(&mut self, other: Namespace) {
        debug_assert_eq!(self.full_name, other.full_name);
        self.namespaces.extend(other.namespaces);
        self.types.extend(other.types);
        self.functions.extend(other.functions);
    }

    pub fn combined_with(&self, other: Namespace) -> Namespace {
        let mut new_self = self.clone();
        new_self.extend(other);
        new_self
    }

    // recursively
    pub fn find_fn(&self, name: GlobalFunctionTarget) -> Option<&FunctionDeclaration> {
        let mut result_here = self.functions.values().find(|f| f.id == name);

        let mut ns_iter = self.namespaces.values();
        while result_here.is_none() {
            if let Some(ns) = ns_iter.next() {
                result_here = ns.find_fn(name)
            } else {
                return None;
            }
        }

        return result_here;
    }
}

impl From<GlobalFunctionTarget> for LocalFunctionTarget {
    fn from(value: GlobalFunctionTarget) -> Self {
        match value {
            GlobalFunctionTarget::Defined(id) => LocalFunctionTarget::Defined(id),
            GlobalFunctionTarget::Native(id) => LocalFunctionTarget::Native(id),
        }
    }
}

impl GlobalFunctionTarget {
    pub fn assert_defined(&self) -> FunctionId {
        if let GlobalFunctionTarget::Defined(id) = self {
            return id.clone();
        }
        panic!("Main function is not a defined function");
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

    pub fn matches(&self, name: &str) -> bool {
        if let Some(short_name) = &self.short_name {
            if short_name.as_ref() == name {
                return true;
            }
        }

        if let Some(long_name) = &self.long_name {
            return long_name.as_ref() == name;
        } else {
            return false;
        }
    }
}
