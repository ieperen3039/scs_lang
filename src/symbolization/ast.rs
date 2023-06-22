use std::{collections::HashMap, rc::Rc};

pub type TypeRef = Rc<TypeDefinition>;
pub type FunctionRef = Rc<FunctionDefinition>;
pub type VarRef = Rc<VariableDeclaration>;

pub type Identifier = Rc<str>;

//#[derive(Hash, Eq, PartialEq)]

pub struct Program {
    pub types: HashMap<Identifier, TypeRef>,
    pub functions: HashMap<Identifier, FunctionRef>,
    pub main: Option<FunctionRef>,
}

// -- types --

pub enum TypeDefinition {
    Base(BaseType),
    Enum(EnumDefinition),
    Function(FunctionType),
}

pub struct BaseType {
    pub name: Identifier,
    pub generic_parameters : Vec<Identifier>,
    pub fields : Vec<TypeFields>,
}

pub struct TypeFields {
    pub name : Identifier,
    pub type_name : Identifier,
}

impl TypeDefinition {
    pub fn get_name(&self) -> Identifier {
        match *self {
            TypeDefinition::Base(BaseType { name, .. }) => name,
            TypeDefinition::Enum(EnumDefinition { name, .. }) => name,
            TypeDefinition::Function(_) => Rc::from("fn"),
        }
    }
}

pub struct FunctionType {
    parameters: Vec<TypeRef>,
    return_type: TypeRef,
}

pub struct EnumDefinition {
    pub name: Identifier,
    pub values: Vec<Identifier>,
}

// -- implementations --

pub struct TypeUse {
    pub definition : TypeRef,
    pub as_array: bool,
    pub generic_parameters : Vec<TypeRef>,
}

pub struct FunctionDefinition {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeUse,
    pub body: FunctionBlock,
    pub is_static: bool,
}

pub struct Parameter {
    pub name: VarRef,
    pub expansion: bool,
}

pub struct VariableDeclaration {
    pub type_name: TypeUse,
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
