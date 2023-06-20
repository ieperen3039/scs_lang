use std::{collections::HashMap, rc::Rc};

pub type TypeRef = Rc<TypeDefinition>;
pub type FunctionRef = Rc<FunctionDefinition>;
pub type VarRef = Rc<VariableDeclaration>;

pub type Identifier = Rc<str>;

pub struct Program {
    pub definitions: Scope,
    pub main: Option<FunctionRef>,
}

pub struct Scope {
    pub name: Identifier,
    pub types: HashMap<Identifier, TypeRef>,
    pub functions: HashMap<Identifier, FunctionRef>,
    pub scopes: HashMap<Identifier, Scope>,
}

pub enum Definition {
    Scope(Scope),
    Type(TypeRef),
    Function(FunctionRef),
}

// -- types --

#[derive(Hash, Eq, PartialEq)]
pub enum TypeDefinition {
    Native(Identifier),
    Derived(DerivedType),
    Array(Identifier),
    Enum(EnumDefinition),
    Function(FunctionType),
}

#[derive(Hash, Eq, PartialEq)]
pub struct DerivedType {
    pub name: Identifier,
    pub generic_parameters : Vec<TypeDefinition>,
    pub derived_from: Rc<TypeDefinition>,
}

impl TypeDefinition {
    pub fn get_name(&self) -> Identifier {
        match self {
            TypeDefinition::Native(name) => name,
            TypeDefinition::Derived(DerivedType { name, .. }) => name,
            TypeDefinition::Array(name) => name,
            TypeDefinition::Enum(EnumDefinition { name, .. }) => name,
            TypeDefinition::Function(_) => "fn",
        }
    }
}

#[derive(Hash, Eq, PartialEq)]
pub struct FunctionType {
    parameters: Vec<TypeRef>,
    return_type: TypeRef,
}

#[derive(Hash, Eq, PartialEq)]
pub struct EnumDefinition {
    pub name: Identifier,
    pub values: Vec<Identifier>,
}

// -- implementations --

pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeRef,
    pub body: FunctionBlock,
    pub is_static: bool,
}

pub struct Parameter {
    pub name: VarRef,
    pub expansion: bool,
}

pub struct VariableDeclaration {
    pub type_name: TypeRef,
    pub name: Identifier,
}

pub struct FunctionBlock {
    pub statements: Vec<Statement>,
}

pub struct Statement {
    pub base_element: Expression,
    pub modifiers: Vec<MethodCall>,
    pub return_variable: Option<VarRef>, // None if return statement
}

pub enum Expression {
    StaticFunctionCall {
        namespace: Identifier,
        function: MethodCall,
    },
    FunctionBlock(FunctionBlock),
    Array(Vec<Expression>),
    Literal(Literal),
}

pub enum Literal {
    Number(u32),
    SignedNumber(i32),
    String(String),
}

pub struct MethodCall {
    name: FunctionRef,
    arguments: Vec<VarRef>,
}
