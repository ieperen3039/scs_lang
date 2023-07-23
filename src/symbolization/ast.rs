use std::{collections::HashMap, rc::Rc};

pub type Identifier = Rc<str>;
pub type NumericIdentifier = u32;

pub struct Program {
    pub name: String,
    pub definitions: Scope,
    pub main: Option<Rc<FunctionDefinition>>,
}

pub struct Scope {
    pub full_name : Vec<Identifier>,
    pub scopes: HashMap<Identifier, Scope>,
    pub types: HashMap<Identifier, TypeDefinition>,
    pub functions: HashMap<Identifier, FunctionDefinition>,
}

// -- references to types -- 

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum TypeRef {
    UnresolvedName(UnresolvedName),
    Defined(DefinedRef),
    UnamedTuple(Vec<TypeRef>),
    Array(Box<TypeRef>),
    Function(FunctionType),
    Generic(Rc<GenericParameter>),
    Void
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
// could be a base type, but also an enum variant or a named tuple
pub struct DefinedRef {
    pub id: NumericIdentifier,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters : Vec<TypeRef>,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct UnresolvedName {
    pub name: Identifier,
    pub scope : Vec<Identifier>,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters : Vec<TypeRef>,
}

// an unspecific `fn<>` declaration
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct FunctionType {
    pub parameters: Vec<TypeRef>,
    pub return_type: Box<TypeRef>,
}

// -- defined types --

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TypeDefinition {
    pub name: Identifier,
    pub id: NumericIdentifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Rc<GenericParameter>>,
    pub sub_type : TypeSubType,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct GenericParameter {
    pub name : Identifier
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum TypeSubType {
    Base { derived : Option<Box<TypeRef>> },
    Enum { values : Vec<Identifier> },
    Variant { variants : Vec<VariantValue> },
    Tuple { elements : Vec<TypeRef> },
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct VariantValue {
    pub name : Identifier,
    pub value_type : TypeRef,
}

// -- implementations --

pub struct FunctionDefinition {
    pub name: Identifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Rc<GenericParameter>>,
    // parameter expansion must be resolved before the ast is constructed
    pub parameters: HashMap<Identifier, TypeRef>,
    pub body: FunctionBlock,
    pub is_static: bool,
    pub is_external: bool,
}

pub struct VariableDeclaration {
    pub var_type: TypeRef,
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
    pub mutations: Vec<Mutator>,
}

pub enum Expression {
    StaticFunctionCall(FunctionCall),
    FunctionBlock(FunctionBlock),
    Array(ArrayInitialisation),
    Literal(Literal),
    // a _reference_ to a variable is an expression, not a declaration.
    Variable(Rc<VariableDeclaration>),
}

#[derive(Debug)]
pub enum Literal {
    Number(i32),
    String(Rc<str>),
    Boolean(bool),
}

// mutations of expressions
pub enum Mutator {
    FunctionCall(FunctionCall),
    Assignment(Rc<VariableDeclaration>),
}

pub struct FunctionCall {
    pub function: Rc<FunctionDefinition>,
    pub arguments: Vec<Argument>,
}

pub struct Argument {
    pub parameter_name: Identifier,
    pub value : Expression,
}

pub struct ArrayInitialisation {
    pub element_type : TypeRef,
    pub elements : Vec<Expression>,
}
