// term = "(" , term , ")"
//      | "[" , term , "]"
//      | "{" , term , "}"
//      | concatenation 
//      | alternation 
//      | terminal
//      | identifier ;

pub enum Term {
    Optional(Box<Term>),
    Repetition(Box<Term>),
    // concatenation = term , "," , term , { "," , term };
    Concatenation(Vec<Term>),
    // alternation = term , "|" , term , { "|" , term };
    Alternation(Vec<Term>),
    // terminal = "'" , character , { character } , "'" 
    //          | '"' , character , { character } , '"' ;
    Terminal(String),
    // identifier = letter , { letter | digit | "_" } ;
    Identifier(String),
}

// rule = identifier , "=" , term , terminator ;
pub struct Rule {
    pub identifier : String,
    pub pattern : Term
}

// grammar = { rule } ;
pub struct EbnfAst {
    pub rules : Vec<Rule>,
}