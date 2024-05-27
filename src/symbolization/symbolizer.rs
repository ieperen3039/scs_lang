use std::collections::HashMap;

use crate::{
    built_in,
    parsing::rule_nodes::RuleNode,
    symbolization::{
        ast::{self, FunctionId, Identifier, Namespace},
        function_collector::FunctionCollector,
        function_parser::FunctionParser,
        semantic_result::SemanticError,
        type_collector::{Definition, TypeCollector},
        type_resolver,
        variable_storage::VarStorage,
    },
};

use super::semantic_result::SemanticResult;

pub fn parse_faux_program(
    ast: RuleNode,
    external_namespace: &Namespace,
    type_collector: &mut TypeCollector,
) -> SemanticResult<ast::Program> {
    debug_assert_eq!(ast.rule_name, "faux_program");
    // faux_program = [ version_declaration ], { include_declaration }, { _definition }, [ program_interface, function_body ];
    // _definition = constant_def | scope | type_definition | enum_definition | variant_definition | implementation | function_definition;

    let mut proto_scope = Namespace::new("", None);
    let mut types = Vec::new();

    // collect type definitions
    for node in &ast.sub_rules {
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
    let types = type_resolver::resolve_type_definitions(types, external_namespace, &proto_scope)?;

    let type_definitions: HashMap<ast::TypeId, ast::TypeDefinition> =
        types.into_iter().map(|t| (t.id, t)).collect();

    let mut function_collector = FunctionCollector::new(type_collector, type_definitions.clone());

    let mut root_scope = proto_scope.combined_with(external_namespace.clone());

    // read function declarations
    let functions = parse_function_declarations(&ast, &mut function_collector, &mut root_scope)?;

    let function_declarations: HashMap<FunctionId, ast::FunctionDeclaration> =
        functions.into_iter().map(|f| (f.id, f)).collect();

    let function_map: HashMap<Identifier, FunctionId> = function_declarations
        .iter()
        .map(|(id, decl)| (decl.name.clone(), id.to_owned()))
        .collect();

    let function_parser = FunctionParser::new(
        &root_scope,
        function_declarations,
        function_collector,
        type_collector,
    );

    let function_definitions = parse_function_definitions(&ast, &function_map, &function_parser)?;

    let main_fn_id =
        function_map
            .get("main")
            .cloned()
            .ok_or_else(|| SemanticError::SymbolNotFound { kind: "function", symbol: Identifier::from("main") })?;

    Ok(ast::Program {
        namespaces: root_scope,
        type_definitions,
        function_definitions,
        entry_function: main_fn_id,
    })
}

pub fn parse_faux_script(
    ast: RuleNode,
    external_namespace: &Namespace,
    type_collector: &mut TypeCollector,
) -> SemanticResult<ast::Program> {
    debug_assert_eq!(ast.rule_name, "faux_script");
    // faux_script = { function_definition }, statement, { statement_separator, statement }, end_symbol, { function_definition };

    let mut types = built_in::primitives::get_primitives();

    types.sort_unstable_by_key(|t| t.id);
    let type_definitions: HashMap<ast::TypeId, ast::TypeDefinition> =
        types.into_iter().map(|t| (t.id, t)).collect();

    let mut function_collector = FunctionCollector::new(type_collector, type_definitions.clone());

    let mut root_scope = external_namespace.clone();

    // read function declarations
    let functions = parse_function_declarations(&ast, &mut function_collector, &mut root_scope).map_err(|e| SemanticError::WhileParsing {
        rule_name: "faux_script (parse_function_declarations)",
        char_idx: ast.first_char(),
        cause: Box::from(e),
    })?;

    let function_declarations: HashMap<FunctionId, ast::FunctionDeclaration> =
        functions.into_iter().map(|f| (f.id, f)).collect();

    let function_map: HashMap<Identifier, FunctionId> = function_declarations
        .iter()
        .map(|(id, decl)| (decl.name.clone(), id.to_owned()))
        .collect();

    let entry_function_id = function_collector.new_id();

    let function_parser = FunctionParser::new(
        &root_scope,
        function_declarations,
        function_collector,
        type_collector,
    );

    let mut function_definitions =
        parse_function_definitions(&ast, &function_map, &function_parser).map_err(|e| SemanticError::WhileParsing {
            rule_name: "faux_script (parse_function_definitions)",
            char_idx: ast.first_char(),
            cause: Box::from(e),
        })?;

    let entry_function = function_parser
        .read_statements(&ast.sub_rules[..], &root_scope, &mut VarStorage::new())
        .map_err(|e| SemanticError::WhileParsing {
            rule_name: "faux_script (read_statements)",
            char_idx: ast.first_char(),
            cause: Box::from(e),
        })?;

    function_definitions.insert(entry_function_id, entry_function);

    Ok(ast::Program {
        namespaces: root_scope,
        type_definitions,
        function_definitions,
        entry_function: entry_function_id,
    })
}

fn parse_function_declarations(
    ast: &RuleNode,
    function_collector: &mut FunctionCollector,
    root_scope: &mut Namespace,
) -> SemanticResult<Vec<ast::FunctionDeclaration>> {
    let mut functions = Vec::new();
    for node in &ast.sub_rules {
        let new_functions = function_collector.read_function_declarations(&node, &*root_scope)?;

        for fn_decl in &new_functions {
            root_scope.add_function(fn_decl);
        }

        functions.extend(new_functions);
    }
    Ok(functions)
}

fn parse_function_definitions(
    ast: &RuleNode,
    function_map: &HashMap<Identifier, FunctionId>,
    function_parser: &FunctionParser,
) -> SemanticResult<HashMap<FunctionId, ast::FunctionBody>> {
    let mut function_definitions: HashMap<FunctionId, ast::FunctionBody> = HashMap::new();
    for node in &ast.sub_rules {
        match node.rule_name {
            "function_definition" => {
                let function_name = node.expect_node("function_name")?;
                let id = function_map.get(&function_name.as_identifier()).unwrap();

                let function_body_node = node.expect_node("function_body")?;
                let function_body = function_parser.read_function_body(
                    *id,
                    function_body_node,
                    function_parser.root_namespace,
                ).map_err(|e| SemanticError::WhileParsing {
                    rule_name: "function_definition",
                    char_idx: function_body_node.first_char(),
                    cause: Box::from(e),
                })?;

                function_definitions.insert(*id, function_body);
            },
            _ => {},
        }
    }
    Ok(function_definitions)
}
