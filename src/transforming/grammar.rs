use crate::parsing::token::TokenClass;

use super::rule_name_generator::RuleNameGenerator;

#[derive(Clone)]
pub struct Grammar {
    pub rules : Vec<Rule>,
    pub name_generator : RuleNameGenerator,
}

#[derive(Clone)]
pub struct Rule {
    pub identifier : String,
    pub pattern : Term
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