use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::parser::RuleNode;

use super::ast::*;

pub struct Symbolizer {
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

impl Symbolizer {
    fn read_scope(&self, node: &RuleNode<'_, '_>) -> Result<Scope, SimpleError> {
        debug_assert_eq!(node.rule_name, "scope");

        let mut iter = node.sub_rules.iter();
        let scope_name = expect_node(iter.next(), "scope_name")?;

        let mut scope = Scope::new(scope_name.tokens);
        
        for node in iter {
            match node.rule_name {
                "scope" => {
                    let sub_scope = self.read_scope(node)?;
                    scope.scopes.insert(Rc::from(node.tokens), sub_scope);
                }, 
                "type_definition" => {
                    let type_def = self.read_type_definition(&scope, node)?;
                    scope.types.insert(type_def.name, Rc::from(TypeDefinition::Struct(type_def)));
                }, 
                "enum_definition" => {
                    let enum_def = self.read_enum(node)?;
                    scope.types.insert(enum_def.name, Rc::from(TypeDefinition::Enum(enum_def)));
                }, 
                "implementation" => {
                    let fn_def = self.read_type_implementation(node)?;
                    scope.functions.insert(fn_def.name, Rc::from(fn_def));
                }, 
                "function_definition" =>{
                    let fn_def = self.read_function(node)?;
                    scope.functions.insert(fn_def.name, Rc::from(fn_def));
                },
                _ => {},
            }
        }

        todo!()
    }
    
    // base_type, [ derived_type | native_decl ], { field_declaration };
    fn read_type_definition(&self, scope: &Scope, node: &RuleNode<'_, '_>) -> Result<StructDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "type_definition");

        let mut iter = node.sub_rules.iter();
        // base_type = identifier, [ generic_types_decl ];
        // generic_types = identifier, { identifier };
        let base_type_node = expect_node(iter.next(), "base_type")?;
        let base_generic_names = self.extract_generic_types(base_type_node)?;
        let base_type_name = find_node(&base_type_node.sub_rules, "identifier")
            .ok_or_else(|| SimpleError::new(format!("missing identifier in base_type")))?;

        let derived_type = find_node(&node.sub_rules, "derived_type")
            .map(|node| self.read_derived_from(node)).transpose()?;


        Ok(StructDefinition { name: Rc::from(base_type_name.tokens), generic_parameters: base_generic_names, fields: (), derived_from: derived_type })
    }

    fn read_derived_from(&self, derived_node : &RuleNode<'_, '_>) -> Result<Rc<TypeDefinition>, SimpleError>
    {
        let derived_type_node = find_node(&derived_node.sub_rules, "base_type")
            .ok_or_else(|| SimpleError::new(format!("missing base_type in derived_type")))?;

        let derived_generic_types = self.extract_generic_types(derived_type_node)?;

        let derived_identifer_node = find_node(&derived_type_node.sub_rules, "identifier")
            .ok_or_else(|| SimpleError::new(format!("missing identifier")))?;

        let search_scope = self.read_scope_reference(derived_node)?;
        let derived_type = search_scope.get(derived_identifer_node.tokens)
            .ok_or_else(|| SimpleError::new(format!("type {} could not be found", derived_type_node.tokens)))?;

        Ok(derived_type)
    }

    fn read_scope_reference<'a>(&'a self, derived_node: &RuleNode<'_, '_>) -> Result<&'a Scope, SimpleError> {
        let derived_scope = find_nodes(&derived_node.sub_rules, "scope_name");
        let mut search_scope = &self.scope;
        for ele in derived_scope {
            debug_assert_eq!(ele.rule_name, "scope_name");
            search_scope = search_scope.scopes.get(ele.tokens)
                .ok_or_else(|| SimpleError::new(format!("unknown scope {}", ele.tokens)))?;
        }
        Ok(search_scope)
    }

    fn extract_generic_types(&self, base_type_node: &RuleNode<'_, '_>) -> Result<Vec<Identifier>, SimpleError> {
        debug_assert_eq!(base_type_node.rule_name, "base_type");

        let mut generic_types = Vec::new();
        if let Some(generic_types_node) = find_node(&base_type_node.sub_rules, "generic_types_decl") {
            for generic_type_node in generic_types_node.sub_rules {
                debug_assert_eq!(base_type_node.rule_name, "identifier");
                generic_types.push(Rc::from(base_type_node.tokens));
            }
        }
        
        Ok(generic_types)
    }

    fn read_enum(&self, node: &RuleNode<'_, '_>) -> Result<EnumDefinition, SimpleError> {
        todo!()
    }

    fn read_type_implementation(&self, node: &RuleNode<'_, '_>) -> Result<FunctionDefinition, SimpleError> {
        todo!()
    }

    fn read_function(&self, node: &RuleNode<'_, '_>) -> Result<FunctionDefinition, SimpleError> {
        todo!()
    }
}

fn expect_node<'r, 'p, 'd>(node: Option<&'r RuleNode<'p, 'd>>, expected : &str) -> Result<&'r RuleNode<'p, 'd>, SimpleError> {
    node
        .filter(|rule| rule.rule_name == expected)
        .ok_or_else(|| SimpleError::new(format!("Expected node {expected}")))
}

fn find_node<'r, 'p, 'd>(node: &'r Vec<RuleNode<'p, 'd>>, expected : &str) -> Option<&'r RuleNode<'p, 'd>,> {
    node.iter()
        .find(|rule| rule.rule_name == expected)
}

fn find_nodes<'r, 'p, 'd>(node: &'r Vec<RuleNode<'p, 'd>>, expected : &str) -> Vec<&'r RuleNode<'p, 'd>,> {
    node.iter()
        .filter(|rule| rule.rule_name == expected)
        .collect()
}