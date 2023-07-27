use super::token::TokenClass;

// term = "(", alternation, ")"
//      | "[", alternation, "]"
//      | "{", alternation, "}"
//      | terminal
//      | identifier;
// concatenation = term, { ",", term };
// alternation = concatenation, { "|", concatenation };
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Optional(Box<Term>),
    Repetition(Box<Term>),
    Concatenation(Vec<Term>),
    Alternation(Vec<Term>),
    // identifier = letter, { letter | digit | "_" };
    Identifier(String),
    // terminal = "'", character, { character }, "'" 
    //          | '"', character, { character }, '"'
    Literal(String),
    //          | '?', ' ', character, { character },' ',  '?';
    Token(TokenClass),
}


// rule = identifier, "=", term, terminator;
#[derive(Debug, Clone)]
pub struct Rule {
    pub identifier : String,
    pub pattern : Term
}

// grammar = { rule };
#[derive(Debug)]
pub struct EbnfAst {
    pub rules : Vec<Rule>,
    pub ignore_rule : Option<Term>,
}