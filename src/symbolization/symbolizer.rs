use std::{collections::HashMap, rc::Rc};

use simple_error::SimpleError;

use crate::{
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::{Scope, VariableDeclaration, Identifier}, function_collector::FunctionCollector, function_parser::FunctionParser,
        type_collector::TypeCollector, type_resolver,
    },
};

pub fn parse_symbols(
    tree: RuleNode,
    external_scope: &Scope,
    type_collector: &mut TypeCollector,
) -> Result<Scope, SimpleError> {
    debug_assert_eq!(tree.rule_name, "faux_program");

    // first collect definitions
    let mut proto_scope = Scope::new("", None);
    let mut types = Vec::new();
    let mut functions = Vec::new();

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

    let type_definitions = types
        .into_iter()
        .map(|t| (t.id, t))
        .collect();

    let mut function_collector = FunctionCollector::new(
        type_collector,
        type_definitions,
    );

    let mut root_scope = proto_scope.combined_with(external_scope.clone());
    
    // read function declarations
    for node in &tree.sub_rules {
        match node.rule_name {
            "version_declaration" | "include_declaration" => {}
            "function_interface" | "function_block" => {
                let new_functions =
                    function_collector.read_function_declarations(&node, &root_scope)?;
                
                new_functions.iter().for_each(|f| {
                    root_scope.add_function(f);
                    // add these separately to the local scope
                    proto_scope.add_function(f);
                });
                functions.extend(new_functions);
            }
            _ => {}
        }
    }
    
    let function_definitions = functions.into_iter().map(|f| (f.id, f)).collect();

    let function_parser = FunctionParser {
        root_scope: &root_scope,
        functions: function_definitions,
    };

    // parse functions bodies
    for node in &tree.sub_rules {
        match node.rule_name {
            "function_interface" | "function_block" => {
                let decl = function_collector.read_function_declaration(&node)?;

                function_parser.read_function_body(
                    &node,
                    function_parser.root_scope,
                    &decl.parameters,
                    Rc::from(VariableDeclaration{ var_type: decl.return_type, name: Identifier::from("return") }),
                )?;
            }
            _ => {}
        }
    }

    Ok(proto_scope)
}
