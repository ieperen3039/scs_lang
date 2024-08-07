use std::{collections::HashMap, hash::Hash, rc::Rc};

pub type Identifier = Rc<str>;
pub type TypeId = u32;
pub type FunctionId = u32;
pub type NativeFunctionId = usize;
pub type VariableId = usize; // this one is used for indexing

pub struct FileAst {
    pub namespaces: Namespace,
    pub type_definitions: HashMap<TypeId, TypeDefinition>,
    pub function_definitions: HashMap<FunctionId, FunctionBody>,
    pub entry_function: FunctionId,
}

pub struct ImplType {
    pub id: TypeId,
    pub array_depth: u8,
}

#[derive(Clone)]
pub struct Namespace {
    pub full_name: Vec<Identifier>,
    pub namespaces: HashMap<Identifier, Namespace>,
    pub types: HashMap<Identifier, TypeId>,
    // only static functions are defined here
    pub functions: HashMap<Identifier, FunctionDeclaration>,
}

// -- references to types --

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum TypeRef {
    UnresolvedName(UnresolvedName),
    Defined(DefinedRef),
    GenericName(Identifier),
    Optional(Box<TypeRef>),
    Result(Box<TypeRef>, Box<TypeRef>),
    UnamedTuple(Vec<TypeRef>),
    Stream(Box<TypeRef>),
    Function(FunctionType),
    Void,
    Break,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
// could be a base type, but also an enum variant or a named tuple
pub struct DefinedRef {
    pub id: TypeId,
    pub generics: Vec<Identifier>,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct UnresolvedName {
    pub name: Identifier,
    pub generics: Vec<Identifier>,
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
    pub id: TypeId,
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
    pub id: VariableId,
    pub par_type: TypeRef,
    pub is_optional: bool,
    pub long_name: Option<Identifier>,
    pub short_name: Option<Identifier>,
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub id: GlobalFunctionTarget,
    pub name: Identifier,
    // decl of generics
    pub generic_parameters: Vec<Identifier>,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeRef,
    pub start_char: usize,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub id: VariableId,
    pub var_type: TypeRef,
    pub name: Identifier,
    pub is_return: bool,
}

#[derive(Clone)]
pub struct FunctionBody {
    pub parameters: Vec<VariableId>,
    pub statements: Vec<Statement>,
    pub return_type: TypeRef,
}

// an expression, followed by mutations on the expression
// this is represented in Expression::FunctionBlock (which is a vec of statements)
#[derive(Clone)]
pub struct Statement {
    pub base_element: ValueExpression,
    pub mutations: Vec<FunctionExpression>,
}

#[derive(Clone)]
pub enum ValueExpression {
    Tuple(Vec<ValueExpression>),
    Literal(Literal),
    // a _reference_ to a variable is an expression
    Variable(VariableId),
    FunctionCall(FunctionCall),
    FunctionAsValue(FunctionExpression),
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
    Operator(Operator),
    Lamda(Lamda),
    Assignment(Rc<VariableDeclaration>),
    Cast(TypeRef),
}

// effectively a more efficient FunctionCall:
// there is always 1 argument
#[derive(Clone)]
pub struct Operator {
    // the operator is implemented as some function
    pub target: GlobalFunctionTarget,
    pub arg: Box<ValueExpression>,
    pub arg_type: TypeRef,
    pub return_type: TypeRef,
}

#[derive(Clone)]
pub struct FunctionCall {
    // could be a global function, or a local variable
    pub target: LocalFunctionTarget,
    // the type of this expression as a value
    pub value_type: FunctionType,
    // arguments for the generic parameters
    pub generic_arguments: Vec<TypeRef>,
    // indices in this vector correspond to the variable ids of the function body
    // (the argument vector and parameter vector should have the same len)
    pub arguments: Vec<Option<ValueExpression>>,
}

#[derive(Clone, Copy, Debug)]
pub enum LocalFunctionTarget {
    Defined(FunctionId),
    Local(VariableId),
    Native(NativeFunctionId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GlobalFunctionTarget {
    Defined(FunctionId),
    Native(NativeFunctionId),
}

#[derive(Clone)]
pub struct Lamda {
    pub parameters: Vec<TypeRef>,
    pub body: Rc<FunctionBody>,
    pub capture: Vec<Rc<VariableDeclaration>>,
}

impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TypeDefinition {}

impl Hash for TypeDefinition {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
