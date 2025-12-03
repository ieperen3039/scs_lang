use std::collections::HashMap;

use super::semantic_result::SemanticResult;
use crate::symbolization::generics_storage::GenStorage;
use crate::{
    built_in,
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::{self, FunctionId, Identifier, Namespace},
        function_collector::FunctionCollector,
        function_parser::FunctionParser,
        type_collector::{Definition, TypeCollector},
        type_resolver,
        variable_storage::VarStorage,
    },
};

pub fn parse_faux_program(
    ast: RuleNode,
    external_namespace: &Namespace,
    type_collector: &mut TypeCollector,
    function_collector: &mut FunctionCollector,
) -> SemanticResult<ast::FileAst> {
    debug_assert_eq!(ast.rule_name, "faux_program");
    // faux_program = [ version_declaration ], { include_declaration }, { _definition }, [ program_interface, function_body ];
    // _definition = constant_def | scope | type_definition | enum_definition | variant_definition | implementation | function_definition;

    let mut internal_namespace = Namespace::new_root();
    let mut types = Vec::new();

    // collect type definitions
    for node in &ast.sub_rules {
        let found = type_collector.read_definitions(node, &internal_namespace)?;
        match found {
            Definition::Type(type_def) => {
                internal_namespace.add_type(&type_def);
                types.push(type_def);
            },
            Definition::Scope(sub_scope, new_types) => {
                internal_namespace.add_sub_scope(sub_scope);
                types.extend(new_types);
            },
            _ => {},
        }
    }

    types.sort_unstable_by_key(|t| t.id);

    // then resolve type cross-references
    let combined_namespace = internal_namespace.combined_with(external_namespace.clone());
    let types = type_resolver::resolve_type_definitions(types, &combined_namespace)?;

    let type_definitions: HashMap<ast::TypeId, ast::TypeDefinition> =
        types.into_iter().map(|t| (t.id, t)).collect();

    // read function declarations
    parse_function_declarations(
        &ast,
        function_collector,
        &combined_namespace,
        &mut internal_namespace,
    )?;

    // update combined namespace with the newly parsed functions
    let combined_namespace = internal_namespace.combined_with(external_namespace.clone());
    let function_parser =
        FunctionParser::new(&type_definitions, &combined_namespace, function_collector);
    let function_definitions = parse_function_definitions(&ast, &function_parser)?;

    let main_fn = type_resolver::resolve_function_name(
        Identifier::from("main"),
        &[],
        &internal_namespace,
        &internal_namespace,
    )?;

    Ok(ast::FileAst {
        namespaces: internal_namespace,
        type_definitions,
        function_definitions,
        entry_function: main_fn.id.assert_defined(),
    })
}

pub fn parse_faux_script(
    ast: RuleNode,
    external_namespace: &Namespace,
    function_collector: &mut FunctionCollector,
) -> SemanticResult<ast::FileAst> {
    debug_assert_eq!(ast.rule_name, "faux_script");
    // faux_script = { function_definition }, statement, { statement_separator, statement }, end_symbol, { function_definition };

    let type_definitions: HashMap<ast::TypeId, ast::TypeDefinition> =
        built_in::primitives::build_primitives()
            .into_iter()
            .map(|t| (t.id, t))
            .collect();

    let mut internal_namespace = Namespace::new_root();

    // read function declarations
    parse_function_declarations(
        &ast,
        function_collector,
        external_namespace,
        &mut internal_namespace,
    )?;

    // update combined namespace with the newly parsed functions
    let combined_namespace = internal_namespace.combined_with(external_namespace.clone());
    let entry_function_id = function_collector.new_id();

    let function_parser =
        FunctionParser::new(&type_definitions, &combined_namespace, function_collector);

    let mut function_definitions = parse_function_definitions(&ast, &function_parser)?;

    let start_of_functions = ast
        .sub_rules
        .iter()
        .position(|n| n.rule_name == "function_definition")
        .unwrap_or(ast.sub_rules.len());

    let entry_function = function_parser.read_statements(
        &ast.sub_rules[..start_of_functions],
        &combined_namespace,
        &mut VarStorage::new(),
        &mut GenStorage::new(),
    )?;

    function_definitions.insert(entry_function_id, entry_function);

    Ok(ast::FileAst {
        namespaces: internal_namespace,
        type_definitions,
        function_definitions,
        entry_function: entry_function_id,
    })
}

fn parse_function_declarations(
    ast: &RuleNode,
    function_collector: &mut FunctionCollector,
    combined_namespace: &Namespace,
    internal_namespace: &mut Namespace,
) -> SemanticResult<()> {
    for node in &ast.sub_rules {
        let new_functions = function_collector.read_function_declarations(
            &node,
            combined_namespace,
            combined_namespace,
        )?;

        for fn_decl in new_functions {
            // no need to add the function to the combined namespace
            internal_namespace.add_function(fn_decl);
        }
    }
    Ok(())
}

fn parse_function_definitions(
    ast: &RuleNode,
    function_parser: &FunctionParser,
) -> SemanticResult<HashMap<FunctionId, ast::FunctionBody>> {
    let mut function_definitions: HashMap<FunctionId, ast::FunctionBody> = HashMap::new();
    for node in &ast.sub_rules {
        match node.rule_name {
            "function_definition" => {
                let function_name_node = node.expect_node("function_name")?;
                let function_name = function_name_node.as_identifier();
                let function_declaration = function_parser
                    .root_namespace
                    .functions
                    .get(&function_name)
                    .unwrap();

                let function_body_node = node.expect_node("function_body")?;
                let function_body = function_parser.read_function_body(
                    function_declaration,
                    function_parser.root_namespace,
                    function_body_node,
                )?;

                function_definitions
                    .insert(function_declaration.id.assert_defined(), function_body);
            },
            _ => {},
        }
    }
    Ok(function_definitions)
}
