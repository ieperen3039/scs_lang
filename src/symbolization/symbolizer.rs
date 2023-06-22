use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::parser::RuleNode;

use super::ast::*;

struct Scope {
    pub scopes: HashMap<Identifier, Scope>,
    pub types: Vec<TypeDeclaration>,
}

struct TypeDeclaration {
    name: Identifier, 
    generic_types : Vec<Identifier>,
    derived_from : Option<Identifier>,
}

struct Symbolizer {
    pub aliasses : HashMap<Identifier, Identifier>,
    pub scope : Scope,
}

pub fn convert_to_program(name: &str, tree: RuleNode<'_, '_>) -> Result<Program, SimpleError> {
    debug_assert_eq!(tree.rule_name, "scs_program");

    let mut symbolizer = Symbolizer {
        aliasses: HashMap::new(),
        scope: Scope::new(name),
    };

    // first collect definitions
    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {},
            "function_interface" | "function_block" => {},
            _ => {
                let sub_node = symbolizer.read_definition(&node)?;
                match sub_node {
                }
            }
        }
    }

    todo!()
}

impl Scope {
    pub fn new (name: &str) -> Scope
    {
        Scope {
            scopes: HashMap::new(),
            types: Vec::new(),
        }
    }

    fn extend(&mut self, other: Scope) {
        self.scopes.extend(other.scopes);
        self.types.extend(other.types);
    }

    fn combined_with(mut self, other: Scope) -> Scope {
        self.extend(other);
        self
    }
}

impl Symbolizer {
    fn read_scope(&self, node: &RuleNode<'_, '_>) -> Result<Scope, SimpleError> {
        debug_assert_eq!(node.rule_name, "scope");

        let mut iter = node.sub_rules.iter();
        let scope_name = expect_node(iter.next(), "scope_name")?;

        let mut scope = Scope::new(scope_name.tokens);
        
        for node in iter {
            let sub_node = self.read_definition(node)?;
            match sub_node {
            }
        }

        todo!()
    }

    fn read_definition(&self, node: &RuleNode<'_, '_>) -> Result<Definition, SimpleError> {
        let result = match node.rule_name {
            "scope" => Definition::Scope(self.read_scope(node)?) , 
            "type_definition" => Definition::Type(self.read_type(node)?), 
            "enum_definition" => Definition::Type(self.read_enum(node)?), 
            "implementation" => Definition::Function(self.read_implementation(node)?), 
            "function_definition" => Definition::Function(self.read_function(node)?),
            _ => {},
        };

        Ok(result)
    }

    // (base_type | array_type | fn_type | native_decl), { function_definition };
    fn read_type(&self, node: &RuleNode<'_, '_>) -> Result<TypeDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "type_definition");

        let mut iter = node.sub_rules.iter();
        let type_name = iter.next().ok_or(SimpleError::new(format!("Expected a type name")))?;

        let derived_from = iter.next().ok_or(SimpleError::new(format!("Expected a derived type")))?;

        let result = match type_name.rule_name {
        }

        let result = match type_name.rule_name {
            "base_type" => self.scope,
            "array_type" => ,
            "native_decl" => ,
            "fn_type" => ,
        }

        for node in iter {
            let function_definition = expect_node(iter.next(), "function_definition")?;
        }

    }

    fn read_enum(&self, node: &RuleNode<'_, '_>) -> Result<EnumDefinition, SimpleError> {
        todo!()
    }
}

fn expect_node<'r, 'p, 'd>(node: Option<&'r RuleNode<'p, 'd>>, expected : &str) -> Result<&'r RuleNode<'p, 'd>, SimpleError> {
    node
        .filter(|rule| rule.rule_name == expected)
        .ok_or(SimpleError::new(format!("Expected node {expected}")))
}

fn find_node<'r, 'p, 'd>(node: &'r Vec<RuleNode<'p, 'd>>, expected : &str) -> Option<&'r RuleNode<'p, 'd>,> {
    node.iter()
        .find(|rule| rule.rule_name == expected)
}