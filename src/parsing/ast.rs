use std::{rc::Rc, collections::HashMap};

type TypeRef = Rc<TypeDefinition>;
type FunctionRef = Rc<FunctionDefinition>;
type VarRef = Rc<VariableDeclaration>;

pub struct Program {
    pub types: HashMap<String, TypeRef>,
    pub functions: HashMap<String, FunctionDefinition>,
    pub main: FunctionBlock,
}

pub enum TypeDefinition {
    NativeType(TypeName),
    DerivedType(DerivedTypeDefinition),
    Enum(EnumDefinition),
    Function(FunctionType),
}

pub struct DerivedTypeDefinition {
    pub type_name: TypeName,
    pub derived_from: TypeRef,
}

pub enum TypeName {
    Plain(String),
    Generic { name: String, generic: TypeRef },
    Array(String),
}

pub struct FunctionType {
    parameters: Vec<TypeRef>,
    return_type: TypeRef,
}

pub struct EnumDefinition {
    pub name: String,
    pub values: Vec<String>,
}

pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeRef,
    pub body : FunctionBlock,
    pub is_static: bool,
}

pub struct Parameter {
    pub name : VarRef,
    pub expansion: bool,
}

pub struct VariableDeclaration {
    pub type_name: TypeRef,
    pub name: String,
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
        namespace: String,
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