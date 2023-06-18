use crate::compiler::ScsCompiler;

#[test]
fn transitive_include() {
    let definition = include_str!("../doc/definition.ebnf");

    let mut compiler = ScsCompiler::build(definition, None).unwrap();

    let result = compiler.compile(&std::path::PathBuf::from("test_data/include_test/include_test_4.scs")).unwrap();
    
    assert!(result.functions.contains_key("include_test_5"));
}