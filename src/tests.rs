use crate::{compiler::FauxCompiler, symbolization::ast};

// #[test]
#[allow(dead_code)]
fn transitive_include() {
    let definition = include_str!("../doc/definition.ebnf");

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let result = compiler.compile(&std::path::PathBuf::from("../test_data/include_test/include_test_4.faux")).unwrap();
    
    assert!(result.namespaces.functions.contains_key("include_test_5"));
}

#[test]
fn faux_script_compile() {
    let definition = include_str!("../doc/faux_script.ebnf");
    let program = r#"
        add(1, 3)
            mul(4)
            = alpha

        add(5, 6)
            mul(r=alpha)
            div(l=1) // calculates 1/x
    "#;

    let parse_result = FauxCompiler::compile_script(&definition, &program);

    match parse_result {
        Err(error) => {
            panic!("{error}");
        },
        Ok(program) => {
            // only the script function should be here
            assert_eq!(program.function_definitions.len(), 1);
            let entry_fn = program.function_definitions.get(&program.entry_function);
            let Some(entry_fn) = entry_fn else {
                panic!("program.entry_function not found")
            };

            assert_eq!(entry_fn.return_type, ast::TypeRef::INT);
        },
    }
}