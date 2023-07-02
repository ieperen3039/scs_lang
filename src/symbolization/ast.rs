use std::{collections::HashMap, rc::Rc};

pub type Identifier = Rc<str>;
pub type ScopeRef = Vec<Identifier>;

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
    Void
}

pub struct StructRef {
    pub name: Identifier,
    pub scope : ScopeRef,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters : Vec<TypeRef>,
}

// an unspecific `fn<>` declaration
pub struct FunctionRef {
    pub parameters: Vec<TypeRef>,
    pub return_type: Box<TypeRef>,
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
    pub r#type : TypeRef,
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
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Identifier>,
    // parameter expansion must be resolved before the ast is constructed
    pub parameters: HashMap<Identifier, TypeRef>,
    pub body: FunctionBlock,
    pub is_static: bool,
    pub is_external: bool,
}

pub struct VariableDeclaration {
    pub type_name: TypeRef,
    pub name: Identifier,
}

pub struct FunctionBlock {
    pub statements: Vec<Statement>,
    pub return_var: Rc<VariableDeclaration>,
}

// an expression, followed by mutations on the expression
// this could be considered an expression by itself
// this is represented in Expression::FunctionBlock (which is a vec of statements)
pub struct Statement {
    pub base_element: Expression,
    pub mutations: Vec<Mutation>,
}

pub enum Expression {
    StaticFunctionCall(FunctionCall),
    FunctionBlock(FunctionBlock),
    Array(Vec<Expression>),
    Literal(Literal),
    // note, a variable declaration is not an expression, but a reference to a variable is.
    Variable(Rc<VariableDeclaration>),
}

pub enum Literal {
    Number(i32),
    String(Rc<str>),
}

// mutations of expressions
pub enum Mutation {
    FunctionCall(FunctionCall),
    Assignment(Rc<VariableDeclaration>),
}

pub struct FunctionCall {
    pub namespace : ScopeRef,
    pub function: Rc<FunctionDefinition>,
    pub arguments: Vec<Argument>,
}

pub struct Argument {
    pub parameter_name: Identifier,
    pub value : Expression,
}
