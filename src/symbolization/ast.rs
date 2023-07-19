use std::{collections::HashMap, rc::Rc};

pub type Identifier = Rc<str>;

//#[derive(Hash, Eq, PartialEq)]

pub struct Program {
    pub name: Identifier,
    pub definitions: Scope,
    pub main: Option<Rc<FunctionDefinition>>,
}

pub struct Scope {
    pub scopes: HashMap<Identifier, Scope>,
    pub types: HashMap<Identifier, Rc<TypeDefinition>>,
    pub functions: HashMap<Identifier, Rc<FunctionDefinition>>,
}

// -- references to types -- 

pub enum TypeRef {
    Defined(DefinedTypeRef),
    UnamedTuple(Vec<TypeRef>),
    Array(Box<TypeRef>),
    Function(FunctionRef),
    Generic(Rc<GenericParameter>),
    Void
}

pub struct DefinedTypeRef {
    pub definition: Rc<TypeDefinition>,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters : Vec<TypeRef>,
}

// an unspecific `fn<>` declaration
pub struct FunctionRef {
    pub parameters: Vec<TypeRef>,
    pub return_type: Box<TypeRef>,
}

// -- defined types --

pub struct TypeDefinition {
    pub name: Identifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Rc<GenericParameter>>,
    pub sub_type : TypeSubType,
}

pub struct GenericParameter {
    name : Identifier
}

pub enum TypeSubType {
    Base { derived : Option<Box<TypeRef>> },
    Enum { values : Vec<Identifier> },
    Variant { variants : Vec<VariantValue> },
    Tuple { elements : Vec<TypeRef> },
}

pub struct VariantValue {
    pub name : Identifier,
    pub value_type : TypeRef,
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
