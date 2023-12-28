use std::{collections::HashMap, rc::Rc};

pub type RuleId = Rc<str>;
pub type RuleStorage = HashMap<RuleId, Vec<Term>>;

use crate::parsing::token::TokenClass;

use super::rule_name_generator::RuleNameGenerator;

#[derive(Clone)]
pub struct Grammar {
    pub start_rule : RuleId,
    pub rules : RuleStorage,
    pub name_generator : RuleNameGenerator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Concatenation(Vec<Term>),
    Alternation(Vec<Term>),
    Identifier(RuleId),
    Terminal(Terminal),
    Empty
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Terminal {
    Literal(String),
    Token(TokenClass)
}