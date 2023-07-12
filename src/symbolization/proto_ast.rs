use std::{collections::HashMap, rc::Rc};

pub type Identifier = Rc<str>;

pub struct ProtoScope {
    pub full_name : Vec<Identifier>,
    pub scopes: HashMap<Identifier, ProtoScope>,
    pub types: HashMap<Identifier, Rc<TypeDefinition>>
}

pub enum TypeName {
    Defined(DefinedName),
    UnamedTuple(Vec<TypeName>),
    Array(Box<TypeName>),
    FunctionType(FunctionName),
    Void
}

pub struct DefinedName {
    pub name: Identifier,
    pub scope : Vec<Identifier>,
    // implementation / our selection of types to use as generic parameters
    pub generic_parameters : Vec<TypeName>,
}

// an unspecific `fn<>` declaration
pub struct FunctionName {
    pub parameters: Vec<TypeName>,
    pub return_type: Box<TypeName>,
}

// -- types --

pub struct TypeDefinition {
    pub name: Identifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Identifier>,
    pub sub_type : TypeSubType,
}

pub enum TypeSubType {
    Base { derived : Box<TypeName> },
    Enum { values : Vec<Identifier> },
    Variant { variants : Vec<TypeName> },
    Tuple { elements : Vec<TypeName> },
}