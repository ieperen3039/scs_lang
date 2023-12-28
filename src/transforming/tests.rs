
use std::io::Write;

use crate::{parsing::ebnf_parser, transforming::{chomsker::{self, Chomsky}, grammatificator, grammar_util::grammar_write}};

#[test]
fn write_conversion_outputs() {
    let definition = include_str!("../../doc/definition.ebnf");
    let ebnf_grammar = ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            err
        })
        .unwrap();
    println!("ebnf parse done");

    let grammar = grammatificator::convert_to_grammar(ebnf_grammar);
    let mut converted_out = std::fs::File::create("grammar.ebnf").unwrap();
    write!(converted_out, "{}\n\n", grammar_write(&grammar)).unwrap();
    println!("grammar converted");

    let chomsky = Chomsky::from(grammar.clone());
    let mut converted_out = std::fs::File::create("chomsky.ebnf").unwrap();
    write!(converted_out, "{}\n\n", chomsker::chomsky_write(&chomsky)).unwrap();
    println!("chonker done");
}
