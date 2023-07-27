use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::{
    parsing::rule_nodes::RuleNode,
    symbolization::{ast::Scope, function_parser, type_collector::TypeCollector, type_resolver::{self}},
};

use super::ast::*;

pub fn parse_symbols(tree: RuleNode, external_scope: &Scope, type_collector: &mut TypeCollector) -> Result<Scope, SimpleError> {
    debug_assert_eq!(tree.rule_name, "faux_program");

    // first collect definitions
    let mut proto_scope = Scope::new("", None);

    for node in &tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            "scope" => {
                let new_scope = type_collector.read_scope_definitions(&node, &proto_scope)?;
                proto_scope.add_sub_scope(new_scope);
            }
            _ => {}
        }
    }

    // then resolve type cross-references
    let mut root_scope = type_resolver::resolve_scope(external_scope, proto_scope)?;

    let symbolizer = Symbolizer { type_collector };

    // parse functions
    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            _ => {
                symbolizer.read_implementations(&node, &mut root_scope)?;
            }
        }
    }

    Ok(root_scope)
}

pub struct Symbolizer<'a> {
    type_collector : &'a TypeCollector
}

impl<'a> Symbolizer<'a> {
    fn read_implementations(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &mut Scope,
    ) -> Result<(), SimpleError> {
        match node.rule_name {
            "scope" => {
                let sub_scope = self.read_scope_functions(node, this_scope)?;
            }
            "implementation" => {
                let fn_def = self.read_function(node)?;
                this_scope.add_function(fn_def);
            }
            "function_definition" => {
                let fn_def = self.read_function(node)?;
                this_scope.add_function(fn_def);
            }
            _ => {}
        }

        Ok(())
    }

    // function_definition     = function_signature, ( function_block | native_decl );
    // function_signature      = function_name, [ generic_types_decl ], [ parameter_list ], return_type;
    fn read_function(&self, node: &RuleNode<'_, '_>) -> Result<FunctionDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "function_definition");
        let signature_node = node.expect_node("function_signature")?;

        let name_node = signature_node.expect_node("function_name")?;

        let generic_parameters = {
            let generic_types_node = signature_node.find_node("generic_types_decl");
            if let Some(generic_types_node) = generic_types_node {
                self.type_collector.read_generic_types_decl(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        let parameters: HashMap<Identifier, TypeRef> = {
            let parameter_node = signature_node.find_node("parameter_list");
            if let Some(parameter_node) = parameter_node {
                self.read_parameter_list(parameter_node)?
            } else {
                HashMap::new()
            }
        };

        let return_var = {
            let return_type_node = signature_node.expect_node("return_type")?;

            debug_assert_eq!(return_type_node.sub_rules.len(), 1);
            let return_type =
                self.type_collector.read_type_ref(return_type_node.sub_rules.first().unwrap())?;

            Rc::from(VariableDeclaration {
                name: Rc::from("return"),
                var_type: return_type,
            })
        };

        let function_block_node = node.find_node("function_block");
        let function_body = {
            if let Some(function_block_node) = function_block_node {
                function_parser::read_function_body(function_block_node, &parameters, return_var)?
            } else {
                signature_node.expect_node("native_decl")?;
                FunctionBlock {
                    statements: Vec::new(),
                    return_var,
                }
            }
        };

        Ok(FunctionDefinition {
            name: name_node.as_identifier(),
            generic_parameters,
            parameters,
            body: function_body,
            is_static: true,
            // technically, only when the "native_decl" node is found
            is_external: function_block_node.is_none(),
        })
    }

    // parameter_list          = parameter, { parameter };
    // parameter               = type_ref, [ expansion_decl ], identifier;
    fn read_parameter_list(
        &self,
        node: &RuleNode,
    ) -> Result<HashMap<Identifier, TypeRef>, SimpleError> {
        debug_assert_eq!(node.rule_name, "parameter_list");
        let parameter_nodes = node.find_nodes("parameter");

        let mut parameters = HashMap::new();
        for parameter_node in parameter_nodes {
            let type_node = parameter_node.expect_node("type_ref")?;
            let type_name = self.type_collector.read_type_ref(type_node)?;
            let expansion_node = parameter_node.find_node("expansion_decl");
            let name_node = parameter_node.expect_node("identifier")?;

            parameters.insert(name_node.as_identifier(), type_name);
        }

        Ok(parameters)
    }

    fn read_scope_functions(
        &self,
        node: &RuleNode,
        super_scope: &mut Scope,
    ) -> Result<(), SimpleError> {
        debug_assert_eq!(node.rule_name, "scope");

        let this_scope_name = node
            .expect_node("scope_name")
            .map(RuleNode::as_identifier)?;
        let this_scope = super_scope
            .scopes
            .get_mut(&this_scope_name)
            .ok_or_else(|| SimpleError::new(format!("Could not find scope {this_scope_name}")))?;

        for node in &node.sub_rules {
            self.read_implementations(node, this_scope)?;
        }

        Ok(())
    }
}
