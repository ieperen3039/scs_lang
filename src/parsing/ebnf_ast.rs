// term = "(" , alternation , ")"
//      | "[" , alternation , "]"
//      | "{" , alternation , "}"
//      | terminal
//      | identifier ;
// concatenation = term , { "," , term };
// alternation = concatenation , { "|" , concatenation };
#[derive(Debug, PartialEq, Eq)]
pub enum Term {
    Optional(Box<Term>),
    Repetition(Box<Term>),
    Concatenation(Vec<Term>),
    Alternation(Vec<Term>),
    // terminal = "'" , character , { character } , "'" 
    //          | '"' , character , { character } , '"' ;
    Terminal(String),
    // identifier = letter , { letter | digit | "_" } ;
    Identifier(String),
}

// rule = identifier , "=" , term , terminator ;
#[derive(Debug)]
pub struct Rule {
    pub identifier : String,
    pub pattern : Term
}

// grammar = { rule } ;
#[derive(Debug)]
pub struct EbnfAst {
    pub rules : Vec<Rule>,
}