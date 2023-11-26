use crate::parsing::token::TokenClass;
#[derive(Debug)]
pub struct ChomskyAst {
    pub rules : Vec<Rule>
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub identifier : String,
    pub prefix : Terminal,
    pub pattern : Pattern
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Concatenation(Vec<String>),
    Terminal(Terminal)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminal {
    Literal(String),
    Token(TokenClass),
}

// grammar = { rule };