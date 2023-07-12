use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::{
    parsing::rule_nodes::RuleNode,
    symbolization::{
        proto_ast::ProtoScope,
        type_collector,
        type_resolver::TypeResolver,
    },
};

use super::ast::*;

pub fn convert_to_program(name: &str, tree: RuleNode) -> Result<Program, SimpleError> {
    debug_assert_eq!(tree.rule_name, "scs_program");

    // first collect definitions
    let mut proto_scope = ProtoScope::new(name, None);

    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            _ => {
                let new_scope = type_collector::read_scope_definitions(&node, &proto_scope)?;
                proto_scope.scopes.insert(new_scope.get_name(), new_scope);
            }
        }
    }

    // then resolve type cross-references
    let resolver = TypeResolver { proto_scope, };
    let mut resolved_scope = resolver.resolve_scope(proto_scope)?;

    let symbolizer = Symbolizer {};

    // parse functions
    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            _ => {
                symbolizer.read_implementations(&node, &mut resolved_scope)?;
            }
        }
    }

    todo!()
}

pub struct Symbolizer {}

impl Symbolizer {
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
                read_generic_types_decl(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        let parameters: HashMap<Identifier, Rc<TypeDefinition>> = {
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
            let return_type: Rc<TypeDefinition> =
                self.read_type_ref(return_type_node.sub_rules.first().unwrap())?;

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
        node: &RuleNode,
    ) -> Result<HashMap<Identifier, Rc<TypeDefinition>>, SimpleError> {
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

    // (( [ _scope_reference ], base_type_decl ) | fn_type ) , { array_symbol };
    fn read_type_ref(&self, node: &RuleNode) -> Result<TypeRef, SimpleError> {
        if let Some(base_type_node) = node.find_node("base_type_decl") {
            let scope = node
                .find_nodes("scope_name")
                .into_iter()
                .map(RuleNode::as_identifier)
                .collect();

            let name_node = base_type_node.expect_node("identifier")?;

            let generic_parameters = {
                let generic_types_node = base_type_node.find_node("generic_types_inst");
                if let Some(generic_types_node) = generic_types_node {
                    self.read_generic_types_inst(generic_types_node)?
                } else {
                    Vec::new()
                }
            };

            Ok(TypeRef::Defined(DefinedTypeRef {
                generic_parameters,
                definition: {}
            }))
        } else if let Some(fn_type_node) = node.find_node("fn_type") {
            // fn_type = [ unnamed_parameter_list ], return_type;
            // unnamed_parameter_list = type_ref, { type_ref };

            let parameters = {
                let mut parameters = Vec::new();
                let parameter_list_node = fn_type_node.find_node("unnamed_parameter_list");
                if let Some(parameter_list_node) = parameter_list_node {
                    for parameter_node in parameter_list_node.sub_rules {
                        parameters.push(self.read_type_ref(parameter_list_node)?);
                    }
                }
                parameters
            };

            let return_type_node = fn_type_node.expect_node("return_type")?;
            let return_type = self.read_type_ref(return_type_node)?;

            Ok(TypeRef::Function(FunctionRef {
                parameters,
                return_type: Box::from(return_type),
            }))
        } else {
            Err(SimpleError::new("Couldn't parse type name"))
        }
    }

    // generic_types_inst = type_name, { type_name };
    fn read_generic_types_inst(&self, node: &RuleNode) -> Result<Vec<TypeRef>, SimpleError> {
        debug_assert_eq!(node.rule_name, "generic_types_inst");
        let mut generic_types = Vec::new();

        for generic_type_node in node.sub_rules {
            let type_ref = self.read_type_ref(&generic_type_node)?;
            generic_types.push(type_ref);
        }

        Ok(generic_types)
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
            self.read_implementations(node, &mut this_scope)?;
        }

        Ok(())
    }
}
