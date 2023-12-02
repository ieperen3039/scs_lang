use std::collections::HashMap;

use crate::parsing::token::TokenClass;

use super::rule_name_generator::RuleNameGenerator;

#[derive(Clone)]
pub struct Grammar {
    pub start_rule : String,
    pub rules : HashMap<String, Vec<Term>>,
    pub name_generator : RuleNameGenerator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Concatenation(Vec<Term>),
    Alternation(Vec<Term>),
    Identifier(String),
    Terminal(Terminal)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminal {
    Literal(String),
    Token(TokenClass),
    Empty
}