use simple_error::SimpleError;
use std::collections::HashMap;

use crate::symbolization::ast::{self, Program};

pub struct GeneratorC {
    pub type_definitions: HashMap<ast::NumericTypeIdentifier, ast::TypeDefinition>,
    pub function_definitions: HashMap<ast::NumericFunctionIdentifier, ast::FunctionBody>,
    pub member_function_definitions: HashMap<ast::ImplType, ast::FunctionDeclaration>,
}

// call `write!`, and wrap the error into a SimpleError
macro_rules! simple_write {
    ($dst:expr, $($arg:tt)*) => {
        write!($dst, $($arg)*).map_err(SimpleError::from)
    };
}

impl GeneratorC {
    pub fn write<Writer: std::io::Write>(out: &mut Writer, program : Program) -> Result<(), SimpleError> {
        let generator = GeneratorC { type_definitions: program.type_definitions, function_definitions: program.function_definitions, member_function_definitions: program.member_function_definitions };
        
        for (_, definition) in &generator.type_definitions {
            generator.write_type_definition(out, definition)?
        }

        out.flush().map_err(SimpleError::from)?;
        
        Ok(())
    }

    fn write_type_definition<Writer: std::io::Write>(&self, out: &mut Writer, definition: &ast::TypeDefinition) -> Result<(), SimpleError> {
        let this_name = self.get_full_type_name(&definition)?;

        match &definition.sub_type {
            ast::TypeSubType::Base { derived : None } => {
                simple_write!(out, "struct {this_name} {{\n\tvoid* mPtr;\n}}")
            },
            ast::TypeSubType::Base { derived : Some(derived) } => {
                simple_write!(out, "typedef {} {}", self.get_type_ref(&derived)?, this_name)
            },
            ast::TypeSubType::Enum { values } => {
                simple_write!(out, "enum {this_name} {{\n")?;
                for enum_val in values {
                    simple_write!(out, "\t{this_name}${enum_val};\n")?;
                }
                simple_write!(out, "}}")
            }
            ast::TypeSubType::Variant { variants } => {
                // first write the enum type
                simple_write!(out, "enum {this_name} {{\n")?;
                for variant in variants {
                    let enum_val = &variant.name;
                    simple_write!(out, "\t{this_name}${enum_val};\n")?;
                }
                simple_write!(out, "}}")?;
                // now write the struct containing an enum and a union
                simple_write!(out, "struct {this_name} {{\n")?;
                simple_write!(out, "\tenum {this_name} mVariant;\n")?;
                simple_write!(out, "\tunion {{\n")?;
                for variant in variants {
                    let enum_val = variant.name.clone();
                    simple_write!(out, "\t\t{} {};\n", self.get_type_ref(&variant.value_type)?, enum_val)?;
                }
                simple_write!(out, "\t}} mData;\n")?;
                simple_write!(out, "}};")
            },
            ast::TypeSubType::Tuple { elements } => {
                simple_write!(out, "struct {this_name} {{\n")?;
                for i in 0..elements.len() {
                    simple_write!(out, "\t{}${};\n", i, self.get_type_ref(&elements[i])?)?;
                }
                simple_write!(out, "}}")
            },
        }
    }
    
    fn get_full_type_name(&self, real_type: &ast::TypeDefinition) -> Result<String, SimpleError> {
        let mut full_name = String::from("struct ");
        for ele in &real_type.full_scope {
            full_name += ele;
            full_name += ".";
        }
        full_name += &real_type.name;
        Ok(full_name)
    }

    fn get_type_ref(&self, definition: &ast::TypeRef) -> Result<String, SimpleError> {
        match definition {
            ast::TypeRef::Defined(defined_ref) => {
                let real_type = &self.type_definitions[&defined_ref.id];
                self.get_full_type_name(real_type)
            },
            ast::TypeRef::UnamedTuple(tuple_types) => {
                let mut tuple_name = String::from("tuple$");
                for ele in tuple_types {
                    tuple_name += &self.get_type_ref(&ele)?;
                    tuple_name += "$";
                }
                Ok(tuple_name)
            },
            ast::TypeRef::Buffer(array_type) => Ok(self.get_type_ref(array_type)? + "*"),
            ast::TypeRef::Function(fn_type) => {
                let mut parameter_str = String::new();
                for ele in &fn_type.parameters {
                    parameter_str += &self.get_type_ref(ele)?;
                    parameter_str += ", ";
                }
                let return_type_str = self.get_type_ref(&fn_type.return_type)?;
                Ok(format!("{return_type_str} (*)({parameter_str})", ))
            },
            ast::TypeRef::Generic(generic) => Ok(generic.name.to_string()),
            ast::TypeRef::Void => Ok(String::from("void")),
            ast::TypeRef::UnresolvedName(unresolved) => Err(SimpleError::new(format!("Unresolved type \"{}\" in generation stage", unresolved.name))),
        }
    }
}

