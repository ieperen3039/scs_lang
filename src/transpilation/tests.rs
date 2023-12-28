use std::io::stdout;

use crate::{compiler::FauxCompiler, transpilation::generator::GeneratorC};

#[test]
fn type_definition() {
    let definition = include_str!("../../doc/definition.ebnf");

    let program_file = r#"
        version faux 0.0.0
        fs {
            type FilePath : String
            type AbsoluteFilePath : FilePath
            type RelativeFilePath : FilePath
        }
    "#;

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let program = compiler.compile(program_file).unwrap();

    let mut writer = stdout();
    GeneratorC::write(&mut writer, program).unwrap()
}