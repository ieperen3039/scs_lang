use std::rc::Rc;

use simple_error::{SimpleError, SimpleResult};

use crate::parsing::{rule_nodes::RuleNode, FIRST_CUSTOM_TYPE_ID};

use super::ast::*;

pub struct TypeCollector {
    next_id: u32,
}

pub enum Definition {
    Type(TypeDefinition),
    Scope(Namespace, Vec<TypeDefinition>),
    Other,
}

impl TypeCollector {
    pub fn new() -> Self {
        TypeCollector {
            next_id: FIRST_CUSTOM_TYPE_ID,
        }
    }

    fn new_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id = id + 1;
        id
    }

    pub fn read_scope_definitions(
        &mut self,
        node: &RuleNode,
        parent_scope: &Namespace,
    ) -> SimpleResult<(Namespace, Vec<TypeDefinition>)> {
        debug_assert_eq!(node.rule_name, "scope");

        let scope_name = node.expect_node("scope_name")?;

        let mut scope = Namespace::new(&scope_name.tokens_as_string(), Some(parent_scope));
        let mut types = Vec::new();

        for node in &node.sub_rules {
            let found = self.read_definitions(node, &scope)?;
            match found {
                Definition::Type(type_def) => {
                    scope.add_type(&type_def);
                    types.push(type_def);
                },
                Definition::Scope(sub_scope, new_types) => {
                    scope.add_sub_scope(sub_scope);
                    types.extend(new_types);
                },
                _ => {},
            }
        }

        Ok((scope, types))
    }

    // scope | type_definition | enum_definition | variant_definition | implementation | function_definition
    pub fn read_definitions(
        &mut self,
        node: &RuleNode,
        scope: &Namespace,
    ) -> SimpleResult<Definition> {
        match node.rule_name {
            "scope" => {
                let (sub_scope, types) = self.read_scope_definitions(node, scope)?;
                Ok(Definition::Scope(sub_scope, types))
            },
            "type_definition" => {
                let type_def = self.read_type_definition(node, scope)?;
                Ok(Definition::Type(type_def))
            },
            "enum_definition" => {
                let enum_def = self.read_enum(node, scope)?;
                Ok(Definition::Type(enum_def))
            },
            "variant_definition" => {
                let variant_def = self.read_variant(node, scope)?;
                Ok(Definition::Type(variant_def))
            },
            _ => Ok(Definition::Other),
        }
    }

    // base_type, [ derived_type | native_decl ], { field_declaration };
    pub fn read_type_definition(
        &mut self,
        node: &RuleNode,
        scope: &Namespace,
    ) -> SimpleResult<TypeDefinition> {
        debug_assert_eq!(node.rule_name, "type_definition");

        // base_type = identifier;
        let base_type_node = node.expect_node("base_type_decl")?;

        let base_type_name = base_type_node
            .expect_node("identifier")
            .map(RuleNode::as_identifier)?;

        let derived_from = {
            let derived_type_node = node.find_node("derived_type");
            if let Some(derived_type_node) = derived_type_node {
                let derived_name = self.read_derived_type(derived_type_node)?;
                Some(Box::from(derived_name))
            } else {
                None
            }
        };

        Ok(TypeDefinition {
            name: base_type_name,
            id: self.new_id(),
            type_class: TypeClass::Base {
                derived: derived_from,
            },
            full_scope: scope.full_name.clone(),
        })
    }

    pub fn read_derived_type(&self, derived_node: &RuleNode) -> SimpleResult<TypeRef> {
        let maybe_base_type_node = derived_node.find_node("base_type_ref");

        if let Some(base_type_node) = maybe_base_type_node {
            return self
                .read_scoped_base_type(derived_node, base_type_node)
                .map(TypeRef::UnresolvedName);
        }

        let tuple_inst_node = derived_node.find_node("tuple_inst").ok_or_else(|| {
            SimpleError::new(format!("Expected either a base_type_ref or a tuple_ref"))
        })?;

        self.read_tuple_inst(tuple_inst_node)
            .map(TypeRef::UnamedTuple)
    }

    pub fn read_tuple_inst(&self, tuple_inst_node: &RuleNode) -> SimpleResult<Vec<TypeRef>> {
        let mut types = Vec::new();
        for node in tuple_inst_node.find_nodes("type_ref") {
            types.push(self.read_type_ref(node)?)
        }

        Ok(types)
    }

    pub fn read_scoped_base_type(
        &self,
        derived_node: &RuleNode,
        base_type_node: &RuleNode,
    ) -> SimpleResult<UnresolvedName> {
        let scope = derived_node
            .find_nodes("scope_name")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        let name_node = base_type_node.expect_node("identifier")?;

        Ok(UnresolvedName {
            name: name_node.as_identifier(),
            scope,
        })
    }

    // enum_definition = identifier, { identifier };
    pub fn read_enum(
        &mut self,
        node: &RuleNode,
        scope: &Namespace,
    ) -> SimpleResult<TypeDefinition> {
        debug_assert_eq!(node.rule_name, "enum_definition");

        let name_node = node.expect_node("identifier")?;

        let value_names = node
            .find_nodes("enum_value_decl")
            .into_iter()
            .map(RuleNode::as_identifier)
            .collect();

        Ok(TypeDefinition {
            name: name_node.as_identifier(),
            id: self.new_id(),
            type_class: TypeClass::Enum {
                values: value_names,
            },
            full_scope: scope.full_name.clone(),
        })
    }

    // variant_definition = identifier, variant_value_decl, { variant_value_decl };
    pub fn read_variant(
        &mut self,
        node: &RuleNode,
        scope: &Namespace,
    ) -> SimpleResult<TypeDefinition> {
        debug_assert_eq!(node.rule_name, "variant_definition");

        let name_node = node.expect_node("identifier")?;

        let mut variant_values = Vec::new();
        for node in node.find_nodes("variant_value_decl") {
            let value = self.read_variant_value(node)?;
            variant_values.push(value);
        }

        Ok(TypeDefinition {
            name: name_node.as_identifier(),
            id: self.new_id(),
            type_class: TypeClass::Variant {
                variants: variant_values,
            },
            full_scope: scope.full_name.clone(),
        })
    }

    // variant_value_decl = identifier, type_ref;
    pub fn read_variant_value(&self, node: &RuleNode) -> SimpleResult<VariantValue> {
        let name_node = node.expect_node("identifier")?;
        let type_node = node.expect_node("type_ref")?;
        let type_name = self.read_type_ref(type_node)?;

        Ok(VariantValue {
            name: name_node.as_identifier(),
            value_type: type_name,
        })
    }

    // type_ref = ( base_type_ref | result_type_ref | optional_type_ref | flag_type_ref | tuple_type_ref | fn_type ), { stream_symbol };
    pub fn read_type_ref(&self, node: &RuleNode) -> SimpleResult<TypeRef> {
        let mut stream_depth = node.find_nodes("stream_symbol").len();

        let mut base = if let Some(base_type_node) = node.find_node("base_type_ref") {
            let unresolved_name = self.read_scoped_base_type(node, base_type_node)?;
            TypeRef::UnresolvedName(unresolved_name)
        } else if let Some(result_node) = node.find_node("result_type_ref") {
            let types = result_node.find_nodes("type_ref");
            assert_eq!(types.len(), 2, "results are supposed to have 2 sub-types");
            let first = types.first().unwrap();
            let second = types.last().unwrap();
            TypeRef::Result(
                Box::new(self.read_type_ref(first)?),
                Box::new(self.read_type_ref(second)?),
            )
        } else if let Some(base_type_node) = node.find_node("optional_type_ref") {
            let sub_type = node.find_node("type_ref").unwrap();
            TypeRef::Optional(Box::new(self.read_type_ref(sub_type)?))
        } else if let Some(_) = node.find_node("flag_type_ref") {
            TypeRef::Flag
        } else if let Some(tuple_inst_node) = node.find_node("tuple_type_ref") {
            TypeRef::UnamedTuple(self.read_tuple_inst(tuple_inst_node)?)
        } else if let Some(fn_type_node) = node.find_node("fn_type") {
            TypeRef::Function(self.read_fn_type(fn_type_node)?)
        } else {
            return Err(SimpleError::new("Couldn't parse type name"));
        };

        while stream_depth > 0 {
            base = TypeRef::Stream(Box::from(base));
            stream_depth -= 1;
        }

        return Ok(base);
    }

    // fn_type = [ unnamed_parameter_list ], return_type;
    // unnamed_parameter_list = type_ref, { type_ref };
    pub fn read_fn_type(&self, fn_type_node: &RuleNode<'_, '_>) -> SimpleResult<FunctionType> {
        let parameters = {
            let mut parameters = Vec::new();
            let parameter_list_node = fn_type_node.find_node("unnamed_parameter_list");
            if let Some(parameter_list_node) = parameter_list_node {
                for parameter_node in &parameter_list_node.sub_rules {
                    parameters.push(self.read_type_ref(parameter_node)?);
                }
            }
            parameters
        };
        let return_type_node = fn_type_node.expect_node("return_type")?;
        let return_type = self.read_type_ref(return_type_node)?;

        Ok(FunctionType {
            parameters,
            return_type: Box::from(return_type),
        })
    }
}
