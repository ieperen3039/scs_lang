use std::rc::Rc;

use simple_error::SimpleError;

use crate::parsing::rule_nodes::RuleNode;

use super::ast::*;

pub fn read_scope_definitions(
    node: &RuleNode,
    parent_scope: &Scope,
) -> Result<Scope, SimpleError> {
    debug_assert_eq!(node.rule_name, "scope");

    let scope_name = node.expect_node("scope_name")?;

    let mut scope = Scope::new(scope_name.tokens, Some(parent_scope));

    for node in &node.sub_rules {
        read_definitions(node, &mut scope)?;
    }

    Ok(scope)
}

// scope | type_definition | enum_definition | variant_definition | implementation | function_definition
fn read_definitions(node: &RuleNode, scope: &mut Scope) -> Result<(), SimpleError> {
    match node.rule_name {
        "scope" => {
            let sub_scope = read_scope_definitions(node, scope)?;
            scope.scopes.insert(node.as_identifier(), sub_scope);
        }
        "type_definition" => {
            let type_def = read_type_definition(node)?;
            scope.types.insert(type_def.name, Rc::from(type_def));
        }
        "enum_definition" => {
            let enum_def = read_enum(node)?;
            scope.types.insert(enum_def.name, Rc::from(enum_def));
        }
        "variant_definition" => {
            let variant_def = read_variant(node)?;
            scope.types.insert(variant_def.name, Rc::from(variant_def));
        }
        _ => {}
    }

    Ok(())
}

// base_type, [ derived_type | native_decl ], { field_declaration };
fn read_type_definition(node: &RuleNode) -> Result<TypeDefinition, SimpleError> {
    debug_assert_eq!(node.rule_name, "type_definition");

    // base_type = identifier, [ generic_types_decl ];
    let base_type_node = node.expect_node("base_type_decl")?;

    let generic_types = {
        let generic_types_node = base_type_node.find_node("generic_types_decl");
        if let Some(generic_types_node) = generic_types_node {
            read_generic_types_decl(generic_types_node)?
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
            let derived_name = read_derived_type(derived_type_node)?;
            Some(Box::from(derived_name))
        } else {
            None
        }
    };

    Ok(TypeDefinition {
        name: base_type_name,
        generic_parameters: generic_types,
        sub_type: TypeSubType::Base {
            derived: derived_from,
        },
    })
}

fn read_derived_type(derived_node: &RuleNode) -> Result<TypeRef, SimpleError> {
    let maybe_base_type_node = derived_node.find_node("base_type_ref");

    if let Some(base_type_node) = maybe_base_type_node {
        return Ok(TypeRef::UnresolvedName(read_scoped_base_type(
            derived_node,
            base_type_node,
        )?));
    }

    let tuple_inst_node = derived_node.find_node("tuple_inst").ok_or_else(|| {
        SimpleError::new(format!("Expected either a base_type_ref or a tuple_ref"))
    })?;

    read_tuple_inst(tuple_inst_node)
}

fn read_tuple_inst(tuple_inst_node: &RuleNode) -> Result<TypeRef, SimpleError> {
    let mut types = Vec::new();
    for node in tuple_inst_node.find_nodes("type_ref") {
        types.push(read_type_ref(node)?)
    }

    Ok(TypeRef::UnamedTuple(types))
}

fn read_scoped_base_type(
    derived_node: &RuleNode,
    base_type_node: &RuleNode,
) -> Result<UnresolvedName, SimpleError> {
    let scope = derived_node
        .find_nodes("scope_name")
        .into_iter()
        .map(RuleNode::as_identifier)
        .collect();

    let name_node = base_type_node.expect_node("identifier")?;

    let generic_parameters = {
        let generic_types_node = base_type_node.find_node("generic_types_inst");
        if let Some(generic_types_node) = generic_types_node {
            read_generic_types_inst(generic_types_node)?
        } else {
            Vec::new()
        }
    };

    Ok(UnresolvedName {
        name: name_node.as_identifier(),
        generic_parameters,
        scope,
    })
}

// enum_definition = identifier, { identifier };
fn read_enum(node: &RuleNode) -> Result<TypeDefinition, SimpleError> {
    debug_assert_eq!(node.rule_name, "enum_definition");

    let name_node = node.expect_node("identifier")?;

    let value_names = node
        .find_nodes("enum_value_decl")
        .into_iter()
        .map(RuleNode::as_identifier)
        .collect();

    Ok(TypeDefinition {
        name: name_node.as_identifier(),
        generic_parameters: Vec::new(),
        sub_type: TypeSubType::Enum {
            values: value_names,
        },
    })
}

// variant_definition = identifier, [ generic_types_decl ], variant_value_decl, { variant_value_decl };
fn read_variant(node: &RuleNode) -> Result<TypeDefinition, SimpleError> {
    debug_assert_eq!(node.rule_name, "variant_definition");

    let name_node = node.expect_node("identifier")?;

    let generic_types = {
        let generic_types_node = node.find_node("generic_types_decl");
        if let Some(generic_types_node) = generic_types_node {
            read_generic_types_decl(generic_types_node)?
        } else {
            Vec::new()
        }
    };

    let mut variant_values = Vec::new();
    for node in node.find_nodes("variant_value_decl") {
        let value = read_variant_value(node)?;
        variant_values.push(value);
    }

    Ok(TypeDefinition {
        name: name_node.as_identifier(),
        generic_parameters: generic_types,
        sub_type: TypeSubType::Variant {
            variants: variant_values,
        },
    })
}

// variant_value_decl = identifier, type_ref;
fn read_variant_value(node: &RuleNode) -> Result<VariantValue, SimpleError> {
    let name_node = node.expect_node("identifier")?;
    let type_node = node.expect_node("type_ref")?;
    let type_name = read_type_ref(type_node)?;

    Ok(VariantValue {
        name: name_node.as_identifier(),
        value_type: type_name,
    })
}

// type_ref = ( ( [ _scope_reference ], ".", base_type_ref ) | fn_type | tuple_inst ), { array_symbol };
fn read_type_ref(node: &RuleNode) -> Result<TypeRef, SimpleError> {
    if let Some(base_type_node) = node.find_node("base_type_decl") {
        let unresolved_name = read_scoped_base_type(node, base_type_node)?;
        Ok(TypeRef::UnresolvedName(unresolved_name))
    } else if let Some(fn_type_node) = node.find_node("fn_type") {
        let function_name = read_fn_type(fn_type_node)?;
        Ok(TypeRef::Function(function_name))
    } else if let Some(tuple_inst_node) = node.find_node("tuple_inst") {
        read_tuple_inst(tuple_inst_node)
    } else {
        Err(SimpleError::new("Couldn't parse type name"))
    }
}

// fn_type = [ unnamed_parameter_list ], return_type;
// unnamed_parameter_list = type_ref, { type_ref };
fn read_fn_type(fn_type_node: &RuleNode<'_, '_>) -> Result<FunctionType, SimpleError> {
    let parameters = {
        let mut parameters = Vec::new();
        let parameter_list_node = fn_type_node.find_node("unnamed_parameter_list");
        if let Some(parameter_list_node) = parameter_list_node {
            for parameter_node in parameter_list_node.sub_rules {
                parameters.push(read_type_ref(parameter_list_node)?);
            }
        }
        parameters
    };
    let return_type_node = fn_type_node.expect_node("return_type")?;
    let return_type = read_type_ref(return_type_node)?;
    
    Ok(FunctionType {
        parameters,
        return_type: Box::from(return_type),
    })
}

// generic_types = identifier, { identifier };
fn read_generic_types_decl(node: &RuleNode) -> Result<Vec<Rc<GenericParameter>>, SimpleError> {
    debug_assert_eq!(node.rule_name, "generic_types_decl");
    let mut generic_types = Vec::new();

    for generic_type_node in node.sub_rules {
        if generic_type_node.rule_name != "identifier" {
            return Err(SimpleError::new("expected identifier"));
        }

        generic_types.push(Rc::from(GenericParameter { name: generic_type_node.as_identifier() }));
    }

    Ok(generic_types)
}

// generic_types_inst = type_name, { type_name };
fn read_generic_types_inst(node: &RuleNode) -> Result<Vec<TypeRef>, SimpleError> {
    debug_assert_eq!(node.rule_name, "generic_types_inst");
    let mut generic_types = Vec::new();

    for generic_type_node in node.sub_rules {
        let type_ref = read_type_ref(&generic_type_node)?;
        generic_types.push(type_ref);
    }

    Ok(generic_types)
}
