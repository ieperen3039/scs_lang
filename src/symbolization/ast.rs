use std::{collections::HashMap, hash::Hash, rc::Rc};

pub type Identifier = Rc<str>;
pub type NumericTypeIdentifier = u32;
pub type NumericFunctionIdentifier = u32;

pub struct Program {
    pub namespaces: Namespace,
    pub type_definitions: HashMap<NumericTypeIdentifier, TypeDefinition>,
    pub function_definitions: HashMap<NumericFunctionIdentifier, FunctionBody>,
    pub member_function_definitions: HashMap<ImplType, Vec<NumericFunctionIdentifier>>,
}

pub struct ImplType {
    pub id: NumericTypeIdentifier,
    pub array_depth: u8,
}

#[derive(Clone)]
pub struct Namespace {
    pub full_name: Vec<Identifier>,
    pub namespaces: HashMap<Identifier, Namespace>,
    pub types: HashMap<Identifier, NumericTypeIdentifier>,
    // only static functions are defined here
    pub functions: HashMap<Identifier, NumericFunctionIdentifier>,
}

// -- references to types --

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum TypeRef {
    UnresolvedName(UnresolvedName),
    Defined(DefinedRef),
    Optional(Box<TypeRef>),
    Result(Box<TypeRef>, Box<TypeRef>),
    Flag,
    UnamedTuple(Vec<TypeRef>),
    Stream(Box<TypeRef>),
    Function(FunctionType),
    Void,
    NoReturn,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
// could be a base type, but also an enum variant or a named tuple
pub struct DefinedRef {
    pub id: NumericTypeIdentifier,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct UnresolvedName {
    pub name: Identifier,
    pub scope: Vec<Identifier>,
}

// an unspecific `fn<>` declaration
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct FunctionType {
    pub parameters: Vec<TypeRef>,
    pub return_type: Box<TypeRef>,
}

// -- defined types --

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub id: NumericTypeIdentifier,
    pub name: Identifier,
    pub full_scope: Vec<Identifier>,
    pub type_class: TypeClass,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum TypeClass {
    Base { derived: Option<Box<TypeRef>> },
    Enum { values: Vec<Identifier> },
    Variant { variants: Vec<VariantValue> },
    Tuple { elements: Vec<TypeRef> },
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct VariantValue {
    pub name: Identifier,
    pub value_type: TypeRef,
}

// -- implementations --

#[derive(Debug, Clone)]
pub struct Parameter {
    pub par_type: TypeRef,
    pub long_name: Option<Identifier>,
    pub short_name: Option<Identifier>,
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub id: NumericFunctionIdentifier,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeRef,
    pub is_external: bool,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub var_type: TypeRef,
    pub name: Identifier,
}

#[derive(Clone)]
pub struct FunctionBody {
    pub statements: Vec<Statement>,
    pub return_var: Rc<VariableDeclaration>,
}

// an expression, followed by mutations on the expression
// this is represented in Expression::FunctionBlock (which is a vec of statements)
#[derive(Clone)]
pub struct Statement {
    pub base_element: ValueExpression,
    pub mutations: Vec<FunctionExpression>,
}

#[derive(Clone)]
pub enum Expression {
    Value(ValueExpression),
    Functional(FunctionExpression),
}

#[derive(Clone)]
pub enum ValueExpression {
    Tuple(Vec<ValueExpression>),
    Literal(Literal),
    // a _reference_ to a variable is an expression
    Variable(Rc<VariableDeclaration>),
}

#[derive(Clone)]
pub enum Literal {
    Number(i32),
    String(Rc<str>),
    Boolean(bool),
}

// mutations of expressions
#[derive(Clone)]
pub enum FunctionExpression {
    FunctionCall(FunctionCall),
    Operator(NumericFunctionIdentifier),
    Lamda(Lamda),
    Assignment(Rc<VariableDeclaration>),
}

#[derive(Clone)]
pub struct FunctionCall {
    pub id: NumericFunctionIdentifier,
    // indices in this vector correspond to the parameter of the called function
    // (the argument vector and parameter vector should have the same len)
    // Option::None indicates that this is itself a fn expression accepting all missing arguments in order
    pub arguments: Vec<Option<Expression>>,
}

#[derive(Clone)]
pub struct Lamda {
    pub parameters: Vec<Rc<str>>,
    pub body: FunctionBody,
    pub capture: Vec<Rc<VariableDeclaration>>,
}

impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.type_class == other.type_class
    }
}

impl Eq for TypeDefinition {}

impl Hash for TypeDefinition {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}