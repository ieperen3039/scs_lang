use crate::built_in::function_builder::FunctionBuilder;
use crate::built_in::functions::NativeFunctionBuilder;
use crate::built_in::type_builder::TypeBuilder;
use crate::symbolization::ast;
use crate::symbolization::ast::*;

struct TestNamespace {
    namespace: Namespace,
    foo: TypeRef,
    bar: TypeRef,
}

fn create_test_namespace() -> TestNamespace {
    let mut fn_ids = NativeFunctionBuilder::new();
    let mut type_builder = TypeBuilder::new();
    let mut namespace = Namespace::new_root();

    namespace.add_constant_literal(ast::Identifier::from("true"), Literal::Boolean(true));
    namespace.add_constant_literal(ast::Identifier::from("false"), Literal::Boolean(false));

    let foo_definition = add_type(&mut fn_ids, &mut type_builder, &mut namespace, vec!["foo"]);
    let bar_definition = add_type(&mut fn_ids, &mut type_builder, &mut namespace, vec!["bar"]);

    TestNamespace {
        namespace,
        foo: TypeRef::from(&foo_definition),
        bar: TypeRef::from(&bar_definition)
    }
}

fn add_type(
    fn_ids: &mut NativeFunctionBuilder,
    type_builder: &mut TypeBuilder,
    namespace: &mut Namespace,
    name: Vec<&str>,
) -> TypeDefinition {
    let foo_definition = type_builder.build_plain(name);
    namespace.add_type(&foo_definition);

    {
        let mut foo_ns = Namespace::new(&foo_definition.name, &namespace);
        let foo_type_ref = TypeRef::from(&foo_definition);

        foo_ns.add_constructor(
            GlobalFunctionTarget::Native(fn_ids.new_id()),
            foo_type_ref.clone(),
            vec![],
        );

        foo_ns.add_function({
            let mut builder = FunctionBuilder::new();
            FunctionDeclaration {
                id: GlobalFunctionTarget::Native(fn_ids.new_id()),
                name: ast::Identifier::from("execute"),
                parameters: vec![builder.req_par("par1", &foo_type_ref)],
                return_type: TypeRef::STRING,
                start_char: 0,
                generic_parameters: Vec::new(),
            }
        });

        namespace.add_sub_scope(foo_ns);
    };

    foo_definition
}

#[test]
fn test_constructor() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        foo.new()
    "#;

    let namespace = create_test_namespace();

    let program_result =
        crate::tests::parse_with_custom_namespace(definition, program, namespace.namespace);

    match program_result {
        Err(error) => {
            panic!("Error parsing program: \n{}", error.error_string(program));
        },
        Ok(result) => {
            assert_eq!(result.function_definitions.len(), 1);
            let (_, main_function) = result.function_definitions.iter().next().unwrap();
            assert_eq!(main_function.return_type, namespace.foo);
        },
    }
}

#[test]
fn test_namespace() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        foo.new()
            foo.execute()
            = foo_val;

        bar.new()
            bar.execute()
            = bar_val;

        bar.new()
    "#;

    let namespace = create_test_namespace();
    let program_result =
        crate::tests::parse_with_custom_namespace(definition, program, namespace.namespace);

    match program_result {
        Err(error) => {
            panic!("Error parsing program: \n{}", error.error_string(program));
        }
        Ok(result) => {
            assert_eq!(result.function_definitions.len(), 1);
            let (_, main_function) = result.function_definitions.iter().next().unwrap();
            assert_eq!(main_function.return_type, namespace.bar);
        },
    }
}

#[test]
fn test_member() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    let program = r#"
        foo.new()
            execute()
            = foo_val;

        bar.new()
            execute()
            = bar_val;

        bar.new()
    "#;

    let namespace = create_test_namespace();
    let program_result =
        crate::tests::parse_with_custom_namespace(definition, program, namespace.namespace);

    match program_result {
        Err(error) => {
            panic!("Error parsing program: \n{}", error.error_string(program));
        }
        Ok(result) => {
            assert_eq!(result.function_definitions.len(), 1);
            let (_, main_function) = result.function_definitions.iter().next().unwrap();
            assert_eq!(main_function.return_type, namespace.bar);
        },
    }
}
