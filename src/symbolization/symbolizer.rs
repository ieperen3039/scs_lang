use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::{ast::*, function_parser};

pub struct Symbolizer {
    pub aliasses: HashMap<Identifier, Identifier>,
    pub scope: Scope,
}

pub fn convert_to_program(name: &str, tree: RuleNode) -> Result<Program, SimpleError> {
    debug_assert_eq!(tree.rule_name, "scs_program");

    let mut symbolizer = Symbolizer {
        aliasses: HashMap::new(),
        scope: Scope::new(name),
    };

    // first collect definitions
    for node in tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {}
            _ => {
                symbolizer.read_definitions(&node, &mut symbolizer.scope)?;
            }
        }
    }

    todo!()
}

impl Symbolizer {
    fn read_scope_definitions(&self, node: &RuleNode) -> Result<Scope, SimpleError> {
        debug_assert_eq!(node.rule_name, "scope");

        let scope_name = node.expect_node("scope_name")?;

        let mut scope = Scope::new(scope_name.tokens);

        for node in &node.sub_rules {
            self.read_definitions(node, &mut scope)?;
        }

        Ok(scope)
    }

    fn read_definitions(
        &self,
        node: &RuleNode<'_, '_>,
        scope: &mut Scope,
    ) -> Result<(), SimpleError> {
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
    fn read_type_definition(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<StructDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "type_definition");

        // base_type = identifier, [ generic_types_decl ];
        let base_type_node = node.expect_node("base_type_decl")?;

        let generic_types = {
            if let Some(generic_types_node) =
                base_type_node.find_node("generic_types_decl")
            {
                self.read_generic_types_decl(generic_types_node)?
            } else {
                Vec::new()
            }
        };

        let base_type_name =
        base_type_node.expect_node("identifier").map(RuleNode::as_identifier)?;

        let derived_from = node.find_node("derived_type")
            .map(|node| self.get_real_type_reference(node))
            .transpose()?;

        let field_nodes = node.find_nodes("field_declaration");

        let mut fields = Vec::new();
        for field_node in field_nodes {
            let identifier_node = field_node.expect_node("identifier")?;
            let type_node = field_node.expect_node("type_name")?;
            let type_ref = self.read_type_ref(type_node)?;
            fields.push(StructField {
                name: identifier_node.as_identifier(),
                r#type: type_ref,
            })
        }

        Ok(StructDefinition {
            name: base_type_name,
            generic_parameters: generic_types,
            fields,
            derived_from,
        })
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

    // (( [ _scope_reference ], base_type_decl ) | fn_type ) , { array_symbol };
    fn read_type_ref(&self, node: &RuleNode<'_, '_>) -> Result<TypeRef, SimpleError> {
        if let Some(base_type_node) = node.find_node("base_type_decl") {
            let scope = node.find_nodes("scope_name")
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

            Ok(TypeRef::Struct(StructRef {
                name: name_node.as_identifier(),
                generic_parameters,
                scope,
            }))
        } else if let Some(fn_type_node) = node.find_node("fn_type") {
            // fn_type = [ unnamed_parameter_list ], return_type;
            // unnamed_parameter_list = type_ref, { type_ref };

            let parameters = {
                let mut parameters = Vec::new();
                let parameter_list_node =
                    fn_type_node.find_node("unnamed_parameter_list");
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

    // _scope_reference = scope_name, { scope_name };
    fn read_scope_reference<'a>(
        &'a self,
        node: &RuleNode<'_, '_>,
    ) -> Result<&'a Scope, SimpleError> {
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

    // generic_types = identifier, { identifier };
    fn read_generic_types_decl(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<Vec<Identifier>, SimpleError> {
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
    fn read_generic_types_inst(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<Vec<TypeRef>, SimpleError> {
        debug_assert_eq!(node.rule_name, "generic_types_inst");
        let mut generic_types = Vec::new();

        for generic_type_node in node.sub_rules {
            let type_ref = self.read_type_ref(&generic_type_node)?;
            generic_types.push(type_ref);
        }

        Ok(generic_types)
    }

    // enum_definition = identifier, { identifier };
    fn read_enum(&self, node: &RuleNode<'_, '_>) -> Result<EnumDefinition, SimpleError> {
        debug_assert_eq!(node.rule_name, "enum_definition");

        let name_node = node.expect_node("identifier")?;

        let value_names = node.find_nodes("enum_value_decl")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        Ok(EnumDefinition {
            name: name_node.as_identifier(),
            values: value_names,
        })
    }

    fn read_scope_functions(
        &self,
        node: &RuleNode<'_, '_>,
        super_scope: &mut Scope,
    ) -> Result<(), SimpleError> {
        debug_assert_eq!(node.rule_name, "scope");

        let this_scope_name = node.expect_node("scope_name").map(RuleNode::as_identifier)?;
        let mut this_scope = super_scope
            .scopes
            .get(&this_scope_name)
            .ok_or_else(|| SimpleError::new(format!("Could not find scope {this_scope_name}")))?;

        for node in &node.sub_rules {
            self.read_declarations(node, this_scope)?;
        }

        Ok(())
    }

    fn read_declarations(
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
            let return_type = self.read_type_ref(return_type_node.sub_rules.first().unwrap())?;

            Rc::from(VariableDeclaration {
                name: Rc::from("return"),
                type_name: return_type,
            })
        };

        let function_block_node = node.find_node("function_block");
        let function_body = {
            if let Some(function_block_node) = function_block_node {
                self.read_function_block(function_block_node, parameters, return_var)?
            } else {
                signature_node.expect_node("native_decl")?;
                FunctionBlock {
                    statements: Vec::new(),
                }
            }
        };

        Ok(FunctionDefinition {
            name: name_node.as_identifier(),
            generic_parameters,
            parameters,
            return_var,
            body: function_body,
            is_static: true,
            // technically, only when the "native_decl" node is found
            is_external: function_block_node.is_none(),
        })
    }

    fn read_parameter_list(
        &self,
        node: &RuleNode<'_, '_>,
    ) -> Result<HashMap<Identifier, TypeRef>, SimpleError> {
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

    fn read_function_block(
        &self,
        node: &RuleNode<'_, '_>,
        parameters: HashMap<Identifier, TypeRef>,
        return_var: Rc<VariableDeclaration>,
    ) -> Result<FunctionBlock, SimpleError> {
        function_parser::read_function_block(node, parameters, return_var)
    }
}
