use std::collections::HashMap;

use crate::{
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::Namespace, semantic_result::SemanticError, type_collector::TypeCollector,
    },
};

use super::{ast::*, semantic_result::SemanticResult};

pub struct FunctionCollector<'tc> {
    next_id: u32,
    pub type_collector: &'tc TypeCollector,
    pub type_definitions: HashMap<TypeId, TypeDefinition>,
}

impl<'a> FunctionCollector<'a> {
    pub fn new(
        type_collector: &'a TypeCollector,
        type_definitions: HashMap<TypeId, TypeDefinition>,
    ) -> Self {
        FunctionCollector {
            next_id: 0,
            type_collector,
            type_definitions,
        }
    }

    pub fn new_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id = id + 1;
        id
    }

    pub fn read_function_declarations(
        &mut self,
        node: &RuleNode<'_, '_>,
        this_scope: &Namespace,
    ) -> SemanticResult<Vec<FunctionDeclaration>> {
        match node.rule_name {
            "namespace" => self
                .read_functions_of_namespace(node, this_scope)
                .map_err(|e| SemanticError::WhileParsing {
                    rule_name: "namespace",
                    char_idx: node.first_char(),
                    cause: Box::from(e),
                }),
            "implementation" => {
                let (impl_type, functions) =
                    self.read_implementation(node, this_scope).map_err(|e| {
                        SemanticError::WhileParsing {
                            rule_name: "implementation",
                            char_idx: node.first_char(),
                            cause: Box::from(e),
                        }
                    })?;
                // self.type_definitions
                //     .get_mut(&impl_type.id)
                //     .ok_or_else(|| SimpleError::new("Unknown type of impl block"))?
                //     .extend(functions.clone());
                Ok(functions)
            },
            "function_definition" => {
                let fn_decl = self.read_function_declaration(node).map_err(|e| {
                    SemanticError::WhileParsing {
                        rule_name: "implementation",
                        char_idx: node.first_char(),
                        cause: Box::from(e),
                    }
                })?;
                Ok(vec![fn_decl])
            },
            _ => Ok(Vec::new()),
        }
    }

    // we only read the declaration part of the definition
    // function_definition     = function_name, [ parameter_list ], return_type, function_block;
    pub fn read_function_declaration(
        &mut self,
        node: &RuleNode<'_, '_>,
    ) -> SemanticResult<FunctionDeclaration> {
        debug_assert_eq!(node.rule_name, "function_definition");

        let name_node = node.expect_node("function_name")?;

        let parameters = {
            let parameter_node = node.find_node("parameter_list");
            if let Some(parameter_node) = parameter_node {
                self.read_parameter_list(parameter_node).map_err(|e| SemanticError::WhileParsing {
                    rule_name: "parameter_list",
                    char_idx: node.first_char(),
                    cause: Box::from(e),
                })?
            } else {
                Vec::new()
            }
        };

        let return_type = {
            let return_type_node = node.expect_node("return_type")?;
            debug_assert_eq!(return_type_node.sub_rules.len(), 1);

            self.type_collector
                .read_type_ref(return_type_node.sub_rules.first().unwrap())?
        };

        // we only check whether it exists. We can't parse it before we have collected all function declarations
        let function_block_node = node.find_node("function_block");

        Ok(FunctionDeclaration {
            id: self.new_id(),
            name: name_node.as_identifier(),
            parameters,
            // technically, only when the "native_decl" node is found
            is_external: function_block_node.is_none(),
            return_type,
        })
    }

    // parameter_list          = { parameter };
    // parameter               = type_ref, identifier;
    fn read_parameter_list(&self, node: &RuleNode) -> SemanticResult<Vec<Parameter>> {
        debug_assert_eq!(node.rule_name, "parameter_list");
        let parameter_nodes = node.find_nodes("parameter");

        let mut next_id = 0;
        let mut parameters = Vec::new();
        for parameter_node in parameter_nodes {
            let type_node = parameter_node.expect_node("type_ref")?;
            let type_name = self.type_collector.read_type_ref(type_node)?;
            let name_node = parameter_node.expect_node("identifier")?;

            parameters.push(Parameter {
                par_type: type_name,
                long_name: Some(name_node.as_identifier()),
                short_name: None,
                id: next_id,
            });
            next_id += 1;
        }

        Ok(parameters)
    }

    // untyped_parameter_list  = identifier, { ",", identifier };
    pub fn read_untyped_parameter_list(&self, node: &RuleNode) -> SemanticResult<Vec<Identifier>> {
        debug_assert_eq!(node.rule_name, "untyped_parameter_list");

        let mut parameters = Vec::new();
        for parameter_node in &node.sub_rules {
            let name_node = parameter_node.expect_node("identifier")?;
            parameters.push(name_node.as_identifier())
        }

        Ok(parameters)
    }

    fn read_functions_of_namespace(
        &mut self,
        node: &RuleNode,
        super_scope: &Namespace,
    ) -> SemanticResult<Vec<FunctionDeclaration>> {
        debug_assert_eq!(node.rule_name, "namespace");

        let this_scope_name = node
            .expect_node("namespace_name")
            .map(RuleNode::as_identifier)?;

        let this_scope = super_scope
            .namespaces
            .get(&this_scope_name)
            .ok_or_else(|| SemanticError::SymbolNotFound {
                kind: "namespace",
                symbol: this_scope_name,
            })?;

        let mut scope_function_declarations = Vec::new();
        for node in &node.sub_rules {
            let function_declarations = self.read_function_declarations(node, this_scope)?;
            scope_function_declarations.extend(function_declarations);
        }

        Ok(scope_function_declarations)
    }

    // implementation = base_type_decl, { array_symbol }, { function_definition };
    fn read_implementation(
        &mut self,
        node: &RuleNode<'_, '_>,
        scope: &Namespace,
    ) -> SemanticResult<(ImplType, Vec<FunctionDeclaration>)> {
        debug_assert_eq!(node.rule_name, "implementation");
        let impl_type = self.read_impl_type_decl(node, scope)?;

        let mut function_definitions = Vec::new();
        for func_node in node.find_nodes("function_definition") {
            function_definitions.push(self.read_function_declaration(func_node)?);
        }

        Ok((impl_type, function_definitions))
    }

    // base_type_decl          = identifier
    fn read_impl_type_decl(
        &self,
        node: &RuleNode<'_, '_>,
        scope: &Namespace,
    ) -> SemanticResult<ImplType> {
        debug_assert_eq!(node.rule_name, "base_type_decl");

        let base_type_name = node
            .expect_node("identifier")
            .map(RuleNode::as_identifier)?;

        let type_id = scope.types.get(&base_type_name).ok_or_else(|| {
            SemanticError::SymbolNotFoundInScope {
                kind: "type",
                symbol: base_type_name,
                scope: scope.full_name.clone(),
            }
        })?;

        Ok(ImplType {
            id: type_id.clone(),
            array_depth: 0,
        })
    }
}
