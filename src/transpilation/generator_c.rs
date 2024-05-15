use simple_error::{SimpleError, SimpleResult};
use std::collections::HashMap;

use crate::symbolization::ast::{self, Program};

pub struct GeneratorC {
    pub type_definitions: HashMap<ast::NumericTypeIdentifier, ast::TypeDefinition>,
    pub function_definitions: HashMap<ast::NumericFunctionIdentifier, ast::FunctionBody>,
    pub member_function_definitions: HashMap<ast::ImplType, ast::FunctionDeclaration>,
}

// call `write!`, and wrap the error into a SimpleError
macro_rules! write_fmt {
    ($dst:expr, $($arg:tt)*) => {
        write!($dst, $($arg)*).map_err(SimpleError::from)
    };
}
// call `write_str`, and wrap the error into a SimpleError
macro_rules! write_str {
    ($dst:expr, $arg:expr) => {
        write!($dst, "{}", $arg).map_err(SimpleError::from)
    };
}

impl GeneratorC {
    pub fn write<Writer: std::io::Write>(out: &mut Writer, program: Program) -> SimpleResult<()> {
        let generator = GeneratorC {
            type_definitions: program.type_definitions,
            function_definitions: program.function_definitions,
            member_function_definitions: program.member_function_definitions,
        };

        for (_, definition) in &generator.type_definitions {
            generator.write_type_definition(out, definition)?
        }

        Ok(())
    }

    fn write_type_definition<Writer: std::io::Write>(
        &self,
        out: &mut Writer,
        definition: &ast::TypeDefinition,
    ) -> SimpleResult<()> {
        
        let mut this_name_buffer: Vec<u8> = Vec::new();
        self.write_full_type_name(&mut this_name_buffer, &definition)?;
        let this_name = unsafe { std::str::from_utf8_unchecked(this_name_buffer.as_slice()) };

        match &definition.type_class {
            ast::TypeClass::Base { derived: None } => {
                write_fmt!(out, "struct {this_name} {{\n\tvoid* mPtr;\n}}")
            }
            ast::TypeClass::Base {
                derived: Some(derived),
            } => {
                write_str!(out, "typedef ")?;
                self.write_type_ref(out, &derived);
                write_fmt!(out, " {this_name}")
            }
            ast::TypeClass::Enum { values } => {
                write_fmt!(out, "enum {this_name} {{\n")?;
                for enum_val in values {
                    write_fmt!(out, "\t{this_name}${enum_val};\n")?;
                }
                write_str!(out, "}}")
            }
            ast::TypeClass::Variant { variants } => {
                // first write the enum type
                write_fmt!(out, "enum {this_name} {{\n")?;
                for variant in variants {
                    let enum_val = &variant.name;
                    write_fmt!(out, "\t{this_name}${enum_val};\n")?;
                }
                write_str!(out, "}}")?;
                // now write the struct containing an enum and a union
                write_fmt!(out, "struct {this_name} {{\n")?;
                write_fmt!(out, "\tenum {this_name} mVariant;\n")?;
                write_str!(out, "\tunion {{\n")?;
                for variant in variants {
                    let enum_val = variant.name.clone();
                    write_str!(out, "\t\t")?;
                    self.write_type_ref(out, &variant.value_type)?;
                    write_fmt!(out, " {enum_val};\n")?;
                }
                write_str!(out, "\t}} mData;\n")?;
                write_str!(out, "}};")
            }
            ast::TypeClass::Tuple { elements } => {
                write_fmt!(out, "struct {this_name} {{\n")?;
                for i in 0..elements.len() {
                    write_str!(out, "\t")?;
                    self.write_type_ref(out, &elements[i])?;
                    write_fmt!(out, " _{i};\n")?;
                }
                write_str!(out, "}}")
            }
        }
    }

    fn write_full_type_name<Writer: std::io::Write>(
        &self,
        out: &mut Writer,
        real_type: &ast::TypeDefinition,
    ) -> SimpleResult<()> {
        write_str!(out, "struct ")?;

        for ele in &real_type.full_scope {
            write_str!(out, ele)?;
            write_str!(out, ".")?;
        }
        write_str!(out, &real_type.name)
    }

    fn write_type_ref<Writer: std::io::Write>(
        &self,
        out: &mut Writer,
        definition: &ast::TypeRef,
    ) -> SimpleResult<()> {
        match definition {
            ast::TypeRef::Defined(defined_ref) => {
                let real_type = &self.type_definitions[&defined_ref.id];
                self.write_full_type_name(out, real_type)
            }
            ast::TypeRef::UnamedTuple(tuple_types) => {
                write_str!(out, "tuple$")?;
                for ele in tuple_types {
                    self.write_type_ref(out, &ele)?;
                    write_str!(out, "$")?;
                }
                Ok(())
            }
            ast::TypeRef::Buffer(array_type) => {
                self.write_type_ref(out, array_type)?;
                write_str!(out, "*")
            }
            ast::TypeRef::Function(fn_type) => {
                self.write_type_ref(out, &fn_type.return_type)?;
                write_str!(out, " (*)(")?;
                for tref in &fn_type.parameters {
                    self.write_type_ref(out, tref)?;
                    write_str!(out, ", ")?;
                }
                write_str!(out, ")")
            }
            ast::TypeRef::Void => write_str!(out, "void"),
            ast::TypeRef::UnresolvedName(unresolved) => Err(SimpleError::new(format!(
                "Unresolved type \"{}\" in generation stage",
                unresolved.name
            ))),
        }
    }
}
