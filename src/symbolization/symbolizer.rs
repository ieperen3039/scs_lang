use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::{parsing::rule_nodes::RuleNode, symbolization::{type_collector::TypeCollector, proto_ast::ProtoScope, type_resolver::TypeResolver}};

use super::ast::*;

pub struct Symbolizer {
    
}

pub fn convert_to_program(name: &str, tree: RuleNode) -> Result<Program, SimpleError> {
    debug_assert_eq!(tree.rule_name, "scs_program");

    // first collect definitions
    let collector = TypeCollector { aliasses: HashMap::new() } ;
    let mut proto_scope = ProtoScope::new(name);

    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            _ => {
                let new_scope = collector.read_scope_definitions(&node)?;
                proto_scope.scopes.insert(new_scope.name, new_scope);
            }
        }
    }

    // then resolve type cross-references 
    let resolver = TypeResolver { aliasses: collector.aliasses, proto_scope };
    let resolved_scope = resolver.resolve_scope(proto_scope)?;

    // parse functions
    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            _ => {}
        }
    }

    todo!()
}

impl Symbolizer {
    fn read_implementations(
        &self,
        node: &RuleNode<'_, '_>,
        mut this_scope: &Scope,
    ) -> Result<(), SimpleError> {
        match node.rule_name {
            "scope" => {
                let sub_scope = self.read_scope_functions(node, &mut this_scope)?;
            }
            "implementation" => {
                let fn_def = self.read_member_function(node)?;
                this_scope.functions.insert(fn_def.name, Rc::from(fn_def));
            }
            "function_definition" => {
                let fn_def = self.read_function(node)?;
                this_scope.functions.insert(fn_def.name, Rc::from(fn_def));
            }
            _ => {}
        }

        Ok(())
    }

    fn read_member_function(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<FunctionDefinition, SimpleError> {
        todo!()
    }

    fn read_function(&self, node: &RuleNode<'_, '_>) -> Result<FunctionDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "function_definition");
        // function_definition     = function_signature, ( function_block | native_decl );
        // function_signature      = function_name, [ generic_types_decl ], [ parameter_list ], return_type;
        let signature_node = node.expect_node("function_signature")?;

        let name_node = signature_node.expect_node("function_name")?;

        let generic_parameters = {
            let generic_types_node = signature_node.find_node("generic_types_decl");
            if let Some(generic_types_node) = generic_types_node {
                self.read_generic_types_decl(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        let parameters: HashMap<Identifier, TypeName> = {
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
            let return_type = self.read_type_ref(return_type_node.sub_rules.first().unwrap())?;

            Rc::from(VariableDeclaration {
                name: Rc::from("return"),
                var_type: return_type,
            })
        };

        let function_block_node = node.find_node("function_block");
        let function_body = {
            if let Some(function_block_node) = function_block_node {
                function_parser::read_function_body(function_block_node, parameters, return_var)?
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

    fn read_parameter_list(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<HashMap<Identifier, TypeName>, SimpleError> {
        debug_assert_eq!(node.rule_name, "parameter_list");
        // parameter_list          = parameter, { parameter };
        // parameter               = type_ref, [ expansion_decl ], identifier;

        let parameter_nodes = node.find_nodes("parameter");

        let mut parameters = HashMap::new();
        for parameter_node in parameter_nodes {
            let type_node = parameter_node.expect_node("type_ref")?;
            let type_name = self.read_type_ref(type_node)?;
            let expansion_node = parameter_node.find_node("expansion_decl");
            let name_node = parameter_node.expect_node("identifier")?;

            parameters.insert(name_node.as_identifier(), type_name);
        }

        Ok(parameters)
    }

    fn get_real_type_reference(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<Rc<TypeDefinition>, SimpleError> {
        let type_node = node.expect_node("base_type_impl")?;
        let type_identifer_node = type_node.expect_node("identifier")?;
        let search_scope = self.read_scope_reference(node)?;
    
        let derived_type = search_scope
            .get(type_identifer_node.tokens)
            .ok_or_else(|| {
                SimpleError::new(format!("type {} could not be found", type_node.tokens))
            })?;
    
        let mut generic_types = Vec::new();
        if let Some(generic_types_node) = type_node.find_node("generic_types_decl") {
            for generic_type_node in generic_types_node.sub_rules {
                debug_assert_eq!(generic_type_node.rule_name, "identifier");
                generic_types.push(&generic_type_node.as_identifier());
            }
        }
    
        Ok(derived_type)
    }

    // _scope_reference = scope_name, { scope_name };
    fn read_scope_reference<'a>(&'a self, node: &RuleNode) -> Result<&'a Scope, SimpleError> {
        let derived_scope = node.find_nodes("scope_name");
        let mut search_scope = &self.scope;
        for ele in derived_scope {
            debug_assert_eq!(ele.rule_name, "scope_name");
            search_scope = search_scope
                .scopes
                .get(ele.tokens)
                .ok_or_else(|| SimpleError::new(format!("unknown scope {}", ele.tokens)))?;
        }
        Ok(search_scope)
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
        let mut this_scope = super_scope
            .scopes
            .get(&this_scope_name)
            .ok_or_else(|| SimpleError::new(format!("Could not find scope {this_scope_name}")))?;

        for node in &node.sub_rules {
            self.read_implementations(node, this_scope)?;
        }

        Ok(())
    }
}
