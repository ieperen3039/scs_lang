use std::{collections::HashMap};

use simple_error::SimpleError;

use crate::parsing::parser::RuleNode;

use super::ast::*;

struct Definitions {
    pub types: HashMap<String, TypeRef>,
    pub aliasses: HashMap<TypeRef, TypeRef>,
}

pub fn convert_to_program(tree: RuleNode<'_, '_>) -> Result<Program, SimpleError> {
    assert_eq!(tree.rule_name, "scs_program");

    let mut master_definitions = Definitions {
        types: HashMap::new(),
        aliasses: HashMap::new(),
    };

    for node in tree.sub_rules {
        match node.rule_name {
            "alias_declaration" => {
                let (k, v) = Definitions::read_alias_declaration(&master_definitions, node)?;
                master_definitions.aliasses.insert(k, v);
            }
            "version_declaration" | "include_declaration" => {},
            "function_interface" | "function_block" => {},
            _ => master_definitions.join(Definitions::read_definition(node)?),
        }
    }

    todo!()
}

impl Definitions {
    fn join(&mut self, other: Definitions) {
        self.types.extend(other.types);
        self.aliasses.extend(other.aliasses);
    }

    fn find(&self, definition : &str) -> Option<TypeRef> {
        let mut found = self.types.get(definition)?;

        while let Some(alias_found) = self.aliasses.get(found) {
            found = alias_found;
        }

        Some(found.clone())
    }
    
    // alias_declaration = identifier, identifier;
    fn read_alias_declaration(&self, node: &RuleNode<'_, '_>) -> Result<(TypeRef, TypeRef), SimpleError> {
        let mut iter = node.sub_rules.iter();
        let identifier = expect_node(iter.next(), "identifier")?;
        let alias = expect_node(iter.next(), "identifier")?;

        let referred_type = self.find(alias.tokens).ok_or(SimpleError::new("Alias must be declared after the type"))?;

        let alias_type = TypeRef::new( TypeDefinition::DerivedType(DerivedTypeDefinition { 
            type_name: TypeName::Plain(identifier.tokens.to_string()), 
            derived_from: referred_type 
        }));
        
        Ok((alias_type, referred_type))
    }

    fn read_definition(&self, node: &RuleNode<'_, '_>, scope : ) -> Result<Definitions, SimpleError> {
        match node.rule_name {
            "scope" => Definitions::read_scope(self, node), 
            "type_definition" => , 
            "enum_definition" => , 
            "variant_definition" => , 
            "implementation" => , 
            "function_definition" => ,
            _ => {},
        }
    }

    fn read_scope(&self, node: &RuleNode<'_, '_>, scope : ) -> Result<Definitions, SimpleError> {
        let mut iter = node.sub_rules.iter();
        let scope_name = expect_node(iter.next(), "scope_name")?;
        iter.map(|node| self.read_definition(node, scope)?)
            .reduce(|a, b| a.join(b))
            .or_else(|| Some(Definitions {
                types: HashMap::new(),
                aliasses: HashMap::new(),
            }))
            .map(Ok)
    }
}

fn expect_node<'r, 'p, 'd>(node: Option<&'r RuleNode<'p, 'd>>, expected : &str) -> Result<&'r RuleNode<'p, 'd>, SimpleError> {
    node
        .filter(|rule| rule.rule_name == expected)
        .ok_or(SimpleError::new(format!("Expected node {expected}")))
}