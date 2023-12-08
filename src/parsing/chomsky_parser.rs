use std::{collections::VecDeque, io::Write};

use simple_error::SimpleError;

use crate::transforming::chomsker::Chomsky;

use super::{token::{Token, TokenClass}, parser::*, rule_nodes::RuleNode};

pub struct Parser {
    grammar: Chomsky,
    xml_out: Option<std::fs::File>,
}

impl<'bnf> Parser {
    pub fn new(
        grammar: Chomsky,
        xml_out: Option<std::fs::File>,
    ) -> Result<Parser, SimpleError> {
        if grammar.rules.is_empty() {
            Err(SimpleError::new("No rules in grammar"))
        } else {
            Ok(Parser { grammar, xml_out })
        }
    }

    fn log(&'bnf self, string: &str) {
        if let Some(mut file) = self.xml_out.as_ref() {
            let _ = write!(file, "{string}");
        }
    }

    pub fn parse_program<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
    ) -> Result<RuleNode<'prog, 'bnf>, Vec<Failure<'bnf>>> {
        todo!();
    }
}