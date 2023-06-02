// term = "(" , term , ")"
//      | "[" , term , "]"
//      | "{" , term , "}"
//      | concatenation 
//      | alternation 
//      | terminal
//      | identifier ;

pub enum Term {
    Group(Vec<Term>),
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
    pub formula : Term
}

// grammar = { rule } ;
pub struct EbnfAst {
    pub rules : Vec<Rule>,
}