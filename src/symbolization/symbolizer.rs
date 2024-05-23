use std::{collections::HashMap, rc::Rc};

use clap::Id;
use simple_error::{SimpleError, SimpleResult};

use crate::{
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::{self, Identifier, Namespace, NumericFunctionIdentifier, VariableDeclaration},
        function_collector::FunctionCollector,
        function_parser::FunctionParser,
        type_collector::{Definition, TypeCollector},
        type_resolver,
    },
};

pub fn parse_symbols(
    tree: RuleNode,
    external_scope: &Namespace,
    type_collector: &mut TypeCollector,
) -> SimpleResult<ast::Program> {
    debug_assert_eq!(tree.rule_name, "faux_program");
    // faux_program = [ version_declaration ], { include_declaration }, { _definition }, [ program_interface, function_block ];
    // _definition = constant_def | scope | type_definition | enum_definition | variant_definition | implementation | function_definition;

    let mut proto_scope = Namespace::new("", None);
    let mut types = Vec::new();
    let mut functions = Vec::new();

    // collect type definitions
    for node in &tree.sub_rules {
        let found = type_collector.read_definitions(node, &proto_scope)?;
        match found {
            Definition::Type(type_def) => {
                proto_scope.add_type(&type_def);
                types.push(type_def);
            },
            Definition::Scope(sub_scope, new_types) => {
                proto_scope.add_sub_scope(sub_scope);
                types.extend(new_types);
            },
            _ => {},
        }
    }

    types.sort_unstable_by_key(|t| t.id);

    // then resolve type cross-references
    let types = type_resolver::resolve_type_definitions(types, external_scope, &proto_scope)?;

    let type_definitions: HashMap<ast::NumericTypeIdentifier, ast::TypeDefinition> =
        types.into_iter().map(|t| (t.id, t)).collect();

    let mut function_collector = FunctionCollector::new(type_collector, type_definitions.clone());

    let mut root_scope = proto_scope.combined_with(external_scope.clone());

    // read function declarations
    for node in &tree.sub_rules {
        let new_functions = function_collector.read_function_declarations(&node, &root_scope)?;

        new_functions.iter().for_each(|f| {
            root_scope.add_function(f);
            // add these separately to the local scope
            proto_scope.add_function(f);
        });
        functions.extend(new_functions);
    }

    let function_declarations: HashMap<NumericFunctionIdentifier, ast::FunctionDeclaration> =
        functions.into_iter().map(|f| (f.id, f)).collect();

    let function_map: HashMap<Identifier, NumericFunctionIdentifier> = function_declarations
        .iter()
        .map(|(id, decl)| (decl.name.clone(), id.to_owned()))
        .collect();

    let function_parser = FunctionParser::new(
        &root_scope,
        function_declarations,
        function_collector,
        type_collector,
    );

    let mut function_definitions: HashMap<NumericFunctionIdentifier, ast::FunctionBody> =
        HashMap::new();

    // parse functions bodies
    for node in &tree.sub_rules {
        match node.rule_name {
            "function_definition" => {
                let function_name = node.expect_node("function_name")?;
                let id = function_map.get(&function_name.as_identifier()).unwrap();

                let function_body = function_parser.read_function_body(
                    *id,
                    &node,
                    function_parser.root_namespace,
                )?;

                function_definitions.insert(*id, function_body);
            },
            _ => {},
        }
    }

    let mut member_function_definitions = HashMap::new();
    // todo!();

    Ok(ast::Program {
        namespaces: root_scope,
        type_definitions,
        member_function_definitions,
        function_definitions,
    })
}
