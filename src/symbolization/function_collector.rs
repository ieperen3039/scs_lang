use crate::{
    built_in::primitives::TYPE_ID_BOOLEAN,
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::Namespace, semantic_result::SemanticError, type_collector::TypeCollector,
        type_resolver,
    },
};

use super::{ast::*, semantic_result::SemanticResult};

pub struct FunctionCollector {
    next_id: u32,
}

impl FunctionCollector {
    pub fn new() -> Self {
        FunctionCollector { next_id: 0 }
    }

    pub fn new_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id = id + 1;
        id
    }

    pub fn read_function_declarations(
        &mut self,
        node: &RuleNode<'_, '_>,
        root_namespace: &Namespace,
        local_namespace: &Namespace,
    ) -> SemanticResult<Vec<FunctionDeclaration>> {
        match node.rule_name {
            "namespace" => self
                .read_functions_of_namespace(node, root_namespace, local_namespace),
            "implementation" => {
                let (impl_type, functions) = self
                    .read_implementation(node, root_namespace, local_namespace)?;
                Ok(functions)
            },
            "function_definition" => {
                let fn_decl = self
                    .read_function_declaration(node, root_namespace, local_namespace)?;
                Ok(vec![fn_decl])
            },
            _ => Ok(Vec::new()),
        }
    }

    // we only read the declaration part of the definition
    // function_definition     = function_name, [ parameter_list ], return_type, function_body;
    pub fn read_function_declaration(
        &mut self,
        node: &RuleNode<'_, '_>,
        root_namespace: &Namespace,
        local_namespace: &Namespace,
    ) -> SemanticResult<FunctionDeclaration> {
        debug_assert_eq!(node.rule_name, "function_definition");

        let name_node = node.expect_node("function_name")?;

        let parameters = {
            let parameter_node = node.find_node("parameter_list");
            if let Some(parameter_node) = parameter_node {
                self.read_parameter_list(parameter_node, root_namespace, local_namespace)?
            } else {
                Vec::new()
            }
        };

        let return_type = {
            let return_type_node = node.expect_node("return_type")?;
            debug_assert_eq!(return_type_node.sub_rules.len(), 1);

            let maybe_type_node = return_type_node.find_node("type_ref");

            if let Some(type_node) = maybe_type_node {
                let type_ref = TypeCollector::read_type_ref(type_node)?;
                type_resolver::resolve_type_name(type_ref, root_namespace, local_namespace)?
            } else {
                return_type_node.expect_node("no_return_type_ref")?;
                TypeRef::NoReturn
            }
        };

        // we only check whether it exists. We can't parse it before we have collected all function declarations
        let function_body_node = node.find_node("function_body");

        Ok(FunctionDeclaration {
            id: self.new_id(),
            name: name_node.as_identifier(),
            parameters,
            // technically, only when the "native_decl" node is found
            is_native: function_body_node.is_none(),
            return_type,
        })
    }

    // parameter_list          = { parameter };
    // parameter               = type_ref, identifier;
    fn read_parameter_list(
        &self,
        node: &RuleNode,
        root_namespace: &Namespace,
        local_namespace: &Namespace,
    ) -> SemanticResult<Vec<Parameter>> {
        debug_assert_eq!(node.rule_name, "parameter_list");
        let parameter_nodes = node.find_nodes("parameter");

        let mut next_id = 0;
        let mut parameters = Vec::new();
        for parameter_node in parameter_nodes {
            let type_node = parameter_node.expect_node("type_ref")?;
            let unresolved_type_ref = TypeCollector::read_type_ref(type_node)?;
            let raw_type = type_resolver::resolve_type_name(
                unresolved_type_ref,
                root_namespace,
                local_namespace,
            )?;
            let name_node = parameter_node.expect_node("identifier")?;

            let value = match raw_type {
                TypeRef::Optional(inner_type) => Parameter {
                    par_type: *inner_type,
                    long_name: Some(name_node.as_identifier()),
                    short_name: None,
                    id: next_id,
                    is_optional: true,
                },
                TypeRef::Defined(bool_type) if bool_type.id == TYPE_ID_BOOLEAN => Parameter {
                    par_type: TypeRef::Defined(bool_type),
                    long_name: Some(name_node.as_identifier()),
                    short_name: None,
                    id: next_id,
                    is_optional: true,
                },
                other_type => Parameter {
                    par_type: other_type,
                    long_name: Some(name_node.as_identifier()),
                    short_name: None,
                    id: next_id,
                    is_optional: true,
                },
            };

            parameters.push(value);
            next_id += 1;
        }

        Ok(parameters)
    }

    // untyped_parameter_list  = identifier, { ",", identifier };
    pub fn read_untyped_parameter_list(&self, node: &RuleNode) -> SemanticResult<Vec<Identifier>> {
        debug_assert_eq!(node.rule_name, "untyped_parameter_list");

        let mut parameters = Vec::new();
        for parameter_node in &node.sub_rules {
            if parameter_node.rule_name != "identifier" {
                return Err(SemanticError::UnexpectedNode {
                    found: Identifier::from(parameter_node.rule_name)
                }.while_parsing(node));
            }
            parameters.push(parameter_node.as_identifier())
        }

        Ok(parameters)
    }

    fn read_functions_of_namespace(
        &mut self,
        node: &RuleNode,
        root_namespace: &Namespace,
        super_scope: &Namespace,
    ) -> SemanticResult<Vec<FunctionDeclaration>> {
        debug_assert_eq!(node.rule_name, "namespace");

        let local_namespace_name = node
            .expect_node("namespace_name")
            .map(RuleNode::as_identifier)?;

        let local_namespace = super_scope
            .namespaces
            .get(&local_namespace_name)
            .ok_or_else(|| SemanticError::SymbolNotFound {
                kind: "namespace",
                symbol: local_namespace_name,
            }.while_parsing(node))?;

        let mut scope_function_declarations = Vec::new();
        for node in &node.sub_rules {
            let function_declarations =
                self.read_function_declarations(node, root_namespace, local_namespace)?;
            scope_function_declarations.extend(function_declarations);
        }

        Ok(scope_function_declarations)
    }

    // implementation = base_type_decl, { array_symbol }, { function_definition };
    fn read_implementation(
        &mut self,
        node: &RuleNode<'_, '_>,
        root_namespace: &Namespace,
        local_namespace: &Namespace,
    ) -> SemanticResult<(ImplType, Vec<FunctionDeclaration>)> {
        debug_assert_eq!(node.rule_name, "implementation");
        let impl_type = self.read_impl_type_decl(node, local_namespace)?;

        let mut function_definitions = Vec::new();
        for func_node in node.find_nodes("function_definition") {
            function_definitions.push(self.read_function_declaration(
                func_node,
                root_namespace,
                local_namespace,
            )?);
        }

        Ok((impl_type, function_definitions))
    }

    // base_type_decl          = identifier
    fn read_impl_type_decl(
        &self,
        node: &RuleNode<'_, '_>,
        namespace: &Namespace,
    ) -> SemanticResult<ImplType> {
        debug_assert_eq!(node.rule_name, "base_type_decl");

        let base_type_name = node
            .expect_node("identifier")
            .map(RuleNode::as_identifier)?;

        let type_id = namespace.types.get(&base_type_name).ok_or_else(|| {
            SemanticError::SymbolNotFoundInScope {
                kind: "type",
                symbol: base_type_name,
                scope: namespace.full_name.clone(),
            }.while_parsing(node)
        })?;

        Ok(ImplType {
            id: type_id.clone(),
            array_depth: 0,
        })
    }
}
