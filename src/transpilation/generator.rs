use simple_error::SimpleError;
use std::collections::HashMap;

use crate::symbolization::ast;

struct Generator {
    pub type_definitions: HashMap<ast::NumericTypeIdentifier, ast::TypeDefinition>,
    pub function_definitions: HashMap<ast::NumericFunctionIdentifier, ast::FunctionBody>,
    pub member_function_definitions: HashMap<ast::ImplType, ast::FunctionDeclaration>,
}

impl Generator {
    pub fn generate_type(&self, definition: &ast::TypeDefinition) -> Result<String, SimpleError> {
        let this_name = self.get_full_name(&definition)?;

        match definition.sub_type {
            ast::TypeSubType::Base { derived : None } => {
                Ok(format!("struct {this_name} {{\n\tvoid* mPtr;\n}}"))
            },
            ast::TypeSubType::Base { derived : Some(derived) } => {
                Ok(format!("typedef {} {}", self.generate_type_ref(&derived)?, this_name))
            },
            ast::TypeSubType::Enum { values } => {
                let mut enum_str = format!("enum {this_name} {{\n");
                for enum_val in values {
                    enum_str += &format!("\t{this_name}${enum_val};\n");
                }
                enum_str += "}}";
                Ok(enum_str)
            }
            ast::TypeSubType::Variant { variants } => {
                let mut variant_str = format!("enum {this_name} {{\n");
                let mut union_str = String::from("\tunion {{\n");

                for variant in variants {
                    let enum_val = variant.name;
                    variant_str += &format!("\t{this_name}${enum_val};\n");
                    union_str += &format!("\t\t{} {};\n", self.generate_type_ref(&variant.value_type)?, enum_val);
                }
                variant_str += "}}";
                union_str += &format!("\t}} mData;\n");

                variant_str += &format!("struct {this_name} {{\n\tenum {this_name} mVariant;\n{union_str}}};");
                Ok(variant_str)
            },
            ast::TypeSubType::Tuple { elements } => {
                let mut struct_str = format!("struct {this_name} {{\n");
                for i in 0..elements.len() {
                    struct_str += &format!("\t{}${};\n", i, self.generate_type_ref(&elements[i])?);
                }
                struct_str += "}}";
                Ok(struct_str)
            },
        }
    }
    
    fn get_full_name(&self, real_type: &ast::TypeDefinition) -> Result<String, SimpleError> {
        let mut full_name = String::from("struct ");
        for ele in real_type.full_scope {
            full_name += &ele;
            full_name += ".";
        }
        full_name += &real_type.name;
        Ok(full_name)
    }

    pub fn generate_type_ref(&self, definition: &ast::TypeRef) -> Result<String, SimpleError> {
        match definition {
            ast::TypeRef::Defined(defined_ref) => {
                let real_type = self.type_definitions[&defined_ref.id];
                self.get_full_name(&real_type)
            },
            ast::TypeRef::UnamedTuple(tuple_types) => {
                let mut tuple_name = String::from("tuple$");
                for ele in tuple_types {
                    tuple_name += &self.generate_type_ref(&ele)?;
                    tuple_name += "$";
                }
                Ok(tuple_name)
            },
            ast::TypeRef::Buffer(array_type) => Ok(self.generate_type_ref(array_type)? + "*"),
            ast::TypeRef::Function(fn_type) => {
                let mut parameter_str = String::new();
                for ele in fn_type.parameters {
                    parameter_str += &self.generate_type_ref(&ele)?;
                    parameter_str += ", ";
                }
                let return_type_str = self.generate_type_ref(&fn_type.return_type)?;
                Ok(format!("{return_type_str} (*)({parameter_str})", ))
            },
            ast::TypeRef::Generic(generic) => Ok(generic.name.to_string()),
            ast::TypeRef::Void => Ok(String::from("void")),
            ast::TypeRef::UnresolvedName(ur) => Err(SimpleError::new(format!("Unresolved type \"{}\" in generation stage", ur.name))),
        }
    }
}

