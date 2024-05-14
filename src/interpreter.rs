use std::{path::Path, rc::Rc};

use simple_error::{SimpleError, SimpleResult};

use crate::{
    parsing::{ebnf_parser, left_left_parser, lexer, parser, token::Token},
    transforming::grammatificator,
};

pub struct Interpreter {
    lexer: lexer::StreamLexer,
    parser: Rc<left_left_parser::Parser>,
}

impl Interpreter {
    pub fn build(definition: &str) -> Option<Interpreter> {
        let grammar = match ebnf_parser::parse_ebnf(definition) {
            Ok(ebnf_grammar) => grammatificator::convert_to_grammar(ebnf_grammar),
            Err(err) => {
                println!(
                    "Error parsing EBNF definition: {}",
                    ebnf_parser::error_string(&err, definition)
                );
                return None;
            }
        };

        let parser = left_left_parser::Parser::new(grammar, None);

        Some(Interpreter {
            lexer: lexer::StreamLexer::new(true),
            parser: Rc::from(parser),
        })
    }

    pub fn parse_line(&mut self) -> SimpleResult<()> {
        let mut tokens: Vec<Token> = Vec::new();

        while tokens.len() < 10 {
            let next_token = self.lexer.next_token().map_err(|(char_idx, string)| {
                SimpleError::new(parser::Failure::LexerError { char_idx }.error_string(&string))
            })?;
            tokens.push(next_token);
        }

        // we clone the parser rc, because parse_program returns a result that borrows from this parser.
        // if we don't clone, the borrow prevents us from recursively parse includes before using the parse_result
        let parser = &self.parser.clone();
        let parse_result = parser.parse_program(&tokens);

        Ok(())
    }

    pub fn is_eof(&self) -> bool {
        true
    }
}
