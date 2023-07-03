use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::proto_ast::*;

pub struct TypeCollector {
    pub aliasses: HashMap<Identifier, Identifier>,
}

impl TypeCollector {
    pub fn read_scope_definitions(&self, node: &RuleNode) -> Result<ProtoScope, SimpleError> {
        debug_assert_eq!(node.rule_name, "scope");

        let scope_name = node.expect_node("scope_name")?;

        let mut scope = ProtoScope::new(scope_name.tokens);

        for node in &node.sub_rules {
            self.read_definitions(node, &mut scope)?;
        }

        Ok(scope)
    }

    fn read_definitions(&self, node: &RuleNode, scope: &mut ProtoScope) -> Result<(), SimpleError> {
        match node.rule_name {
            "scope" => {
                let sub_scope = self.read_scope_definitions(node)?;
                scope.scopes.insert(node.as_identifier(), sub_scope);
            }
            "type_definition" => {
                let type_def = self.read_type_definition(node)?;
                scope
                    .types
                    .insert(type_def.name, Rc::from(TypeDefinition::Struct(type_def)));
            }
            "enum_definition" => {
                let enum_def = self.read_enum(node)?;
                scope
                    .types
                    .insert(enum_def.name, Rc::from(TypeDefinition::Enum(enum_def)));
            }
            _ => {}
        }

        Ok(())
    }

    // base_type, [ derived_type | native_decl ], { field_declaration };
    fn read_type_definition(&self, node: &RuleNode) -> Result<StructDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "type_definition");

        // base_type = identifier, [ generic_types_decl ];
        let base_type_node = node.expect_node("base_type_decl")?;

        let generic_types = {
            let generic_types_node = base_type_node.find_node("generic_types_decl");
            if let Some(generic_types_node) = generic_types_node {
                self.read_generic_types_decl(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        let base_type_name = base_type_node
            .expect_node("identifier")
            .map(RuleNode::as_identifier)?;

        let derived_from = {
            let derived_type_node = node.find_node("derived_type");
            if let Some(derived_type_node) = derived_type_node {
                Some(self.read_derived_type(derived_type_node)?)
            } else {
                None
            }
        };

        let field_nodes = node.find_nodes("field_declaration");

        let mut fields = Vec::new();
        for field_node in field_nodes {
            let identifier_node = field_node.expect_node("identifier")?;
            let type_node = field_node.expect_node("type_name")?;
            let type_ref = self.read_type_ref(type_node)?;
            fields.push(StructField {
                name: identifier_node.as_identifier(),
                field_type: type_ref,
            })
        }

        Ok(StructDefinition {
            name: base_type_name,
            generic_parameters: generic_types,
            fields,
            derived_from,
        })
    }

    fn read_derived_type(
        &self,
        derived_node: &RuleNode<'_, '_>,
    ) -> Result<StructName, SimpleError> {
        let scope = derived_node
            .find_nodes("scope_name")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        let base_type_node = derived_node.expect_node("base_type_ref")?;
        let name_node = base_type_node.expect_node("identifier")?;
        let generic_parameters = {
            let generic_types_node = base_type_node.find_node("generic_types_inst");
            if let Some(generic_types_node) = generic_types_node {
                self.read_generic_types_inst(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        Ok(StructName {
            name: name_node.as_identifier(),
            generic_parameters,
            scope,
        })
    }

    // (( [ _scope_reference ], base_type_decl ) | fn_type ) , { array_symbol };
    fn read_type_ref(&self, node: &RuleNode) -> Result<TypeName, SimpleError> {
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

            Ok(TypeName::Struct(StructName {
                name: name_node.as_identifier(),
                generic_parameters,
                scope,
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

            Ok(TypeName::FunctionType(FunctionName {
                parameters,
                return_type: Box::from(return_type),
            }))
        } else {
            Err(SimpleError::new("Couldn't parse type name"))
        }
    }

    // generic_types = identifier, { identifier };
    fn read_generic_types_decl(&self, node: &RuleNode) -> Result<Vec<Identifier>, SimpleError> {
        debug_assert_eq!(node.rule_name, "generic_types_decl");
        let mut generic_types = Vec::new();

        for generic_type_node in node.sub_rules {
            if generic_type_node.rule_name != "identifier" {
                return Err(SimpleError::new("expected identifier"));
            }

            generic_types.push(generic_type_node.as_identifier());
        }

        Ok(generic_types)
    }

    // generic_types_inst = type_name, { type_name };
    fn read_generic_types_inst(&self, node: &RuleNode) -> Result<Vec<TypeName>, SimpleError> {
        debug_assert_eq!(node.rule_name, "generic_types_inst");
        let mut generic_types = Vec::new();

        for generic_type_node in node.sub_rules {
            let type_ref = self.read_type_ref(&generic_type_node)?;
            generic_types.push(type_ref);
        }

        Ok(generic_types)
    }

    // enum_definition = identifier, { identifier };
    fn read_enum(&self, node: &RuleNode) -> Result<EnumDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "enum_definition");

        let name_node = node.expect_node("identifier")?;

        let value_names = node
            .find_nodes("enum_value_decl")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        Ok(EnumDefinition {
            name: name_node.as_identifier(),
            values: value_names,
            derived_from: None,
        })
    }
}
