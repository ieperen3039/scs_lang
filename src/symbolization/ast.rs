use std::{collections::HashMap, rc::Rc, hash::Hash};

pub type Identifier = Rc<str>;
pub type NumericIdentifier = u32;

pub struct Program {
    pub name: String,
    pub namespaces: Scope,
    pub type_definitions: Vec<TypeDefinition>,
    pub function_definitions: HashMap<ImplType, FunctionDefinition>,
    pub main: Option<Rc<FunctionDefinition>>,
}

pub struct ImplType {
    pub id: NumericIdentifier,
    pub array_depth: u8,
}

#[derive(Clone)]
pub struct Scope {
    pub full_name: Vec<Identifier>,
    pub scopes: HashMap<Identifier, Scope>,
    pub types: HashMap<Identifier, NumericIdentifier>,
    // only static, type-less functions are defined here
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
    Void,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
// could be a base type, but also an enum variant or a named tuple
pub struct DefinedRef {
    pub id: NumericIdentifier,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters: Vec<TypeRef>,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct UnresolvedName {
    pub name: Identifier,
    pub scope: Vec<Identifier>,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters: Vec<TypeRef>,
}

// an unspecific `fn<>` declaration
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct FunctionType {
    pub parameters: Vec<TypeRef>,
    pub return_type: Box<TypeRef>,
}

// -- defined types --

#[derive(Clone)]
pub struct TypeDefinition {
    pub name: Identifier,
    pub id: NumericIdentifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters: Vec<Rc<GenericParameter>>,
    pub sub_type: TypeSubType,
    pub member_functions: Vec<FunctionDefinition>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct GenericParameter {
    pub name: Identifier,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum TypeSubType {
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

#[derive(Clone)]
pub struct FunctionDefinition {
    pub name: Identifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters: Vec<Rc<GenericParameter>>,
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

#[derive(Clone)]
pub struct FunctionBlock {
    pub statements: Vec<Statement>,
    pub return_var: Rc<VariableDeclaration>,
}

// an expression, followed by mutations on the expression
// this could be considered an expression by itself
// this is represented in Expression::FunctionBlock (which is a vec of statements)
#[derive(Clone)]
pub struct Statement {
    pub base_element: Expression,
    pub mutations: Vec<Mutator>,
}

#[derive(Clone)]
pub enum Expression {
    StaticFunctionCall(FunctionCall),
    FunctionBlock(FunctionBlock),
    Array(ArrayInitialisation),
    Literal(Literal),
    // a _reference_ to a variable is an expression, not a declaration.
    Variable(Rc<VariableDeclaration>),
}

#[derive(Clone)]
pub enum Literal {
    Number(i32),
    String(Rc<str>),
}

// mutations of expressions
#[derive(Clone)]
pub enum Mutator {
    FunctionCall(FunctionCall),
    Assignment(Rc<VariableDeclaration>),
}

#[derive(Clone)]
pub struct FunctionCall {
    pub function: Rc<FunctionDefinition>,
    pub arguments: Vec<Argument>,
}

#[derive(Clone)]
pub struct Argument {
    pub parameter_name: Identifier,
    pub value: Expression,
}

#[derive(Clone)]
pub struct ArrayInitialisation {
    pub element_type: TypeRef,
    pub elements: Vec<Expression>,
}

impl std::fmt::Debug for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeDefinition")
            .field("name", &self.name)
            .field("id", &self.id)
            .field("generic_parameters", &self.generic_parameters)
            .field("sub_type", &self.sub_type)
            .field("num_member_functions", &self.member_functions.len())
            .finish()
    }
}

impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.sub_type == other.sub_type
    }
}

impl Eq for TypeDefinition {}

impl Hash for TypeDefinition {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}