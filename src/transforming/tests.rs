
use std::io::Write;
use simple_error::SimpleError;

use crate::{parsing::{ebnf_parser, ebnf_ast_util::ebnf_ast_write, lexer::Lexer, parser}, transforming::{chomsker, greibacher}};

#[test]
fn try_parse_example_faux() {
    let definition = include_str!("../../doc/definition.ebnf");
    let program = include_str!("../../doc/example.faux");

    let grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            err
        })
        .unwrap();
    println!("ebnf parse done");

    let grammar = chomsker::convert_to_normal_form(grammar);
    let mut converted_out = std::fs::File::create("chomsky.ebnf").unwrap();
    write!(converted_out, "{}\n\n", ebnf_ast_write(&grammar)).unwrap();
    println!("chonker done");

    let grammar = greibacher::convert_to_normal_form(grammar);

    let mut converted_out = std::fs::File::create("greibach_ebnf.ebnf").unwrap();
    write!(converted_out, "{}\n\n", ebnf_ast_write(&grammar)).unwrap();
    println!("greibach done");

    // return;
    // let xml_out = std::fs::File::create("test_try_parse_example_faux_output.xml").ok();
    let xml_out = None;

    let tokens = Lexer {}.read_all(&program).map_err(|err| {
        SimpleError::new(parser::Failure::LexerError{char_idx : err}.error_string(definition))
    }).unwrap();
    let parser = parser::Parser::new(grammar, xml_out).unwrap();
    let parse_result = parser.parse_program(&tokens);

    if parse_result.is_err() {
        print!(
            "Error parsing program: \n{}",
            parse_result
                .as_ref()
                .unwrap_err()
                .into_iter()
                .map(|err| err.error_string(program) + "\n---\n\n")
                .collect::<String>()
        );
        assert!(parse_result.is_ok());
    }

    let program_ast = parse_result.unwrap();

    assert!(program_ast.rule_name == "faux_program")
}
