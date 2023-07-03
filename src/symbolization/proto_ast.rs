use std::{collections::HashMap, rc::Rc};

pub type Identifier = Rc<str>;

pub struct ProtoScope {
    pub name : Identifier,
    pub scopes: HashMap<Identifier, ProtoScope>,
    pub types: HashMap<Identifier, Rc<TypeDefinition>>
}

pub enum TypeName {
    Struct(StructName),
    Array(Box<TypeName>),
    FunctionType(FunctionName),
    Void
}

pub struct StructName {
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

pub enum TypeDefinition {
    Native(NativeStruct),
    Struct(StructDefinition),
    Enum(EnumDefinition),
    Variant(VariantDefinition),
}

pub struct NativeStruct {
    pub name: Identifier,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Identifier>,
}

pub struct StructDefinition {
    pub name: Identifier,
    // a struct can only be derived from structs
    pub derived_from : Option<StructName>,
    // there are generic declarations; brand new identifiers
    pub generic_parameters : Vec<Identifier>,
    pub fields : Vec<StructField>,
}

pub struct StructField {
    pub name : Identifier,
    // we use an instantiation of the type, filling in any generic parameters
    pub field_type : TypeName,
}

pub struct VariantDefinition {
    pub name: Identifier,
    pub values: Vec<Identifier>,
    // a variant can only be derived from structs
    pub derived_from : Option<StructName>,
}

pub struct EnumDefinition {
    pub name: Identifier,
    pub values: Vec<Identifier>,
    // an enum can only be derived from structs
    pub derived_from : Option<StructName>,
}