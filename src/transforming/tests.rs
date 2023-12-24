
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

    // let greibach = greibacher::convert(grammar.clone());
    // let mut converted_out = std::fs::File::create("greibach_ebnf.ebnf").unwrap();
    // write!(converted_out, "{}\n\n", grammar_write(&greibach)).unwrap();
    // println!("greibach done");

    let chomsky = Chomsky::from(grammar.clone()).unwrap();
    let mut converted_out = std::fs::File::create("chomsky.ebnf").unwrap();
    write!(converted_out, "{}\n\n", chomsker::chomsky_write(&chomsky)).unwrap();
    println!("chonker done");

    // let program = include_str!("../../doc/example.faux");
    // let tokens = Lexer {}.read_all(&program).map_err(|err| {
    //     SimpleError::new(parser::Failure::LexerError{char_idx : err}.error_string(program))
    // }).unwrap();
    
    // let mut converted_out = std::fs::File::create("lexer_out.txt").unwrap();

    // for t in tokens {
    //     if t.slice.contains('\n') { write!(converted_out, "\n").unwrap() };
    //     write!(converted_out, "{:?} ", t.class).unwrap();
    // }
}
