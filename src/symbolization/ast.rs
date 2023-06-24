use std::{collections::HashMap, rc::Rc};

pub type Identifier = Rc<str>;

//#[derive(Hash, Eq, PartialEq)]

pub struct Program {
    pub definitions: Scope,
    pub main: Option<Rc<FunctionDefinition>>,
}

pub struct Scope {
    pub scopes: HashMap<Identifier, Scope>,
    pub types: HashMap<Identifier, Rc<TypeDefinition>>,
    pub functions: HashMap<Identifier, Rc<FunctionDefinition>>,
}

// -- weak references

pub enum TypeRef {
    Struct(StructRef),
    Array(Box<TypeRef>),
    Function(FunctionRef),
}

pub struct StructRef {
    pub name: Identifier,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters : Vec<TypeRef>,
}

pub struct FunctionRef {
    pub parameters: Vec<Rc<TypeDefinition>>,
    pub return_type: Rc<TypeDefinition>,
}

// -- types --

pub enum TypeDefinition {
    Struct(StructDefinition),
    Enum(EnumDefinition),
    Variant(VariantDefinition),
}

impl TypeDefinition {
    pub fn get_name(&self) -> Identifier {
        match *self {
            TypeDefinition::Struct(StructDefinition { name, .. }) => name,
            TypeDefinition::Enum(EnumDefinition { name, .. }) => name,
            TypeDefinition::Variant(VariantDefinition { name, .. }) => name,
        }
    }
}

pub struct StructDefinition {
    pub name: Identifier,
    // a struct can only be derived from structs
    pub derived_from : Option<Rc<TypeDefinition>>,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Identifier>,
    pub fields : Vec<StructField>,
}

pub struct StructField {
    pub name : Identifier,
    // type reference (we use an implementation of the type), not an Rc to a type definition
    pub type_name : TypeRef,
}

pub struct VariantDefinition {
    pub name: Identifier,
    pub values: Vec<Identifier>,
}

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
    pub name: Rc<VariableDeclaration>,
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
    pub return_variable: Option<Rc<VariableDeclaration>>, // None if return statement
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
    name: Rc<FunctionDefinition>,
    arguments: Vec<Rc<VariableDeclaration>>,
}
