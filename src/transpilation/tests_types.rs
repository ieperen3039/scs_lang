use std::io::Write;

use crate::{compiler::FauxCompiler, transpilation::generator::GeneratorC};
use super::test_util::create_file_from_text;

#[test]
fn simple_type() {
    let definition = include_str!("../../doc/definition.ebnf");

    let program_path = create_file_from_text(r#"
        version faux 0.0.0
        type FilePath : String
        type AbsoluteFilePath : FilePath
        type RelativeFilePath : FilePath
    "#);

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let program = compiler.compile(program_path).unwrap();

    let mut writer = std::io::stdout();
    GeneratorC::write(&mut writer, program).unwrap()
}

#[test]
fn namespace_type() {
    let definition = include_str!("../../doc/definition.ebnf");

    let program_path = create_file_from_text(r#"
        version faux 0.0.0

        fs {
            type FilePath : String
            core {
                type AbsoluteFilePath : FilePath
                type RelativeFilePath : FilePath
            }
        }
    "#);

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let program = compiler.compile(program_path).unwrap();

    let mut writer = std::io::stdout();
    GeneratorC::write(&mut writer, program).unwrap()
}

#[test]
fn variant() {
    let definition = include_str!("../../doc/definition.ebnf");

    let program_path = create_file_from_text(r#"
        version faux 0.0.0

        variant Result<P, N> [
            Pos(P),
            Neg(N),
        ]
    "#);

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let program = compiler.compile(program_path).unwrap();

    let mut writer = std::io::stdout();
    GeneratorC::write(&mut writer, program).unwrap()
}

#[test]
fn variant_derived() {
    let definition = include_str!("../../doc/definition.ebnf");

    let program_path = create_file_from_text(r#"
        version faux 0.0.0

        variant Result<P, N> [
            Pos(P),
            Neg(N),
        ]

        type Maybe<T> = Result<T, void>
        type boolean = Result<void, void>
    "#);

    let mut compiler = FauxCompiler::build(definition, None).unwrap();

    let program = compiler.compile(program_path).unwrap();

    let mut writer = std::io::stdout();
    GeneratorC::write(&mut writer, program).unwrap()
}
