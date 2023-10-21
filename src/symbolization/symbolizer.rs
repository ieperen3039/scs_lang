use std::collections::HashMap;

use simple_error::SimpleError;

use crate::{
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::Scope,
        type_collector::TypeCollector,
        type_resolver::{self}, function_parser::FunctionParser,
    },
};

use super::ast::*;

pub struct Symbolizer<'tc> {
    pub type_collector: &'tc TypeCollector,
    pub type_definitions: HashMap<NumericTypeIdentifier, TypeDefinition>,
    pub root_scope: Scope,
}

pub fn parse_symbols(
    tree: RuleNode,
    external_scope: &Scope,
    type_collector: &mut TypeCollector,
) -> Result<Scope, SimpleError> {
    debug_assert_eq!(tree.rule_name, "faux_program");

    // first collect definitions
    let mut proto_scope = Scope::new("", None);
    let mut types = Vec::new();

    for node in &tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            "scope" => {
                let (new_scope, new_types) =
                    type_collector.read_scope_definitions(&node, &proto_scope)?;
                proto_scope.add_sub_scope(new_scope);
                types.extend(new_types);
            }
            _ => {}
        }
    }

    types.sort_unstable_by_key(|t| t.id);

    // then resolve type cross-references
    let types = type_resolver::resolve_type_definitions(types, external_scope, &proto_scope)?;

    let mut type_definitions = HashMap::new();
    for t in types {
        type_definitions.insert(t.id, t);
    }

    let symbolizer = Symbolizer {
        type_collector,
        type_definitions,
        root_scope : proto_scope.combined_with(external_scope.clone())
    };

    // read function declarations
    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {
                symbolizer.read_function_declarations(&node, &mut proto_scope)?;
            }
            _ => {}
        }
    }

    let function_parser = FunctionParser{ root_scope: &symbolizer.root_scope };
    
    // parse functions bodies
    for node in tree.sub_rules {
        match node.rule_name {
            "function_interface" | "function_block" => {
                function_parser.read_function_body(&node, function_parser.root_scope, parameters, return_var)?;
            }
            _ => {}
        }
    }
    

    Ok(proto_scope)
}

impl<'a> Symbolizer<'a> {
    fn read_function_declarations(
        &self,
        node: &RuleNode<'_, '_>,
        this_scope: &mut Scope,
    ) -> Result<(), SimpleError> {
        match node.rule_name {
            "scope" => {
                self.read_scope_functions(node, this_scope)?;
            }
            "implementation" => {
                let fn_def = self.read_implementation(node, this_scope)?;
            }
            "function_definition" => {
                let fn_def = self.read_function_declaration(node)?;
                this_scope.add_function(fn_def);
            }
            _ => {}
        }

        Ok(())
    }

    // we only read the declaration part of the definition
    // function_definition     = function_signature, ( function_block | native_decl );
    // function_signature      = function_name, [ generic_types_decl ], [ parameter_list ], return_type;
    fn read_function_declaration(&self, node: &RuleNode<'_, '_>) -> Result<FunctionDeclaration, SimpleError> {
        debug_assert_eq!(node.rule_name, "function_definition");
        let signature_node = node.expect_node("function_signature")?;

        let name_node = signature_node.expect_node("function_name")?;

        let generic_parameters = {
            let generic_types_node = signature_node.find_node("generic_types_decl");
            if let Some(generic_types_node) = generic_types_node {
                self.type_collector
                    .read_generic_types_decl(generic_types_node)?
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

        let return_type = {
            let return_type_node = signature_node.expect_node("return_type")?;
            debug_assert_eq!(return_type_node.sub_rules.len(), 1);

            self.type_collector
                .read_type_ref(return_type_node.sub_rules.first().unwrap())?
        };

        // we only check whether it exists. We can't parse it before we have collected all function declarations
        let function_block_node = node.find_node("function_block");

        Ok(FunctionDeclaration {
            name: name_node.as_identifier(),
            generic_parameters,
            parameters,
            is_static: true,
            // technically, only when the "native_decl" node is found
            is_external: function_block_node.is_none(),
            return_type,
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
            self.read_function_declarations(node, this_scope)?;
        }

        Ok(())
    }

    // implementation = base_type_decl, { array_symbol }, { function_definition };
    fn read_implementation(
        &self,
        node: &RuleNode<'_, '_>,
        scope: &Scope,
    ) -> Result<(ImplType, Vec<FunctionDeclaration>), SimpleError> {
        debug_assert_eq!(node.rule_name, "implementation");
        let base_type = node.expect_node("base_type_decl")?;
        let impl_type = self.read_impl_type_decl(node, scope)?;

        let mut function_definitions = Vec::new();
        for func_node in node.find_nodes("function_definition") {
            function_definitions.push(self.read_function_declaration(func_node)?);
        }

        Ok((impl_type, function_definitions))
    }

    // base_type_decl          = identifier, [ generic_types_decl ];
    // generic_types_decl      = identifier, { identifier };
    fn read_impl_type_decl(
        &self,
        node: &RuleNode<'_, '_>,
        scope: &Scope,
    ) -> Result<ImplType, SimpleError> {
        debug_assert_eq!(node.rule_name, "base_type_decl");

        let base_type_name = node
            .expect_node("identifier")
            .map(RuleNode::as_identifier)?;

        let type_id = scope.types.get(&base_type_name).ok_or_else(|| {
            SimpleError::new(format!(
                "Could not find '{}' in scope '{:?}'",
                base_type_name, scope.full_name
            ))
        })?;

        let generic_types = {
            let generic_types_node = node.find_node("generic_types_inst");
            if let Some(generic_types_node) = generic_types_node {
                self.type_collector
                    .read_generic_types_inst(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        Ok(ImplType { id: type_id.clone(), array_depth: 0, generic_parameters: generic_types })
    }
}
