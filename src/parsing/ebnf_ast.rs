// term = "(" , alternation , ")"
//      | "[" , alternation , "]"
//      | "{" , alternation , "}"
//      | terminal
//      | identifier ;
// concatenation = term , { "," , term };
// alternation = concatenation , { "|" , concatenation };
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Optional(Box<Term>),
    Repetition(Box<Term>),
    Concatenation(Vec<Term>),
    Alternation(Vec<Term>),
    // terminal = "'" , character , { character } , "'" 
    //          | '"' , character , { character } , '"' ;
    Literal(String),
    Regex(RegexWrapper),
    // identifier = letter , { letter | digit | "_" } ;
    Identifier(String),
}

// explicit PartialEq implementation for regex
#[derive(Debug, Clone)]
pub struct RegexWrapper {
    pub regex : regex::Regex
}

impl PartialEq for RegexWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str()
    }
}

// rule = identifier , "=" , term , terminator ;
#[derive(Debug, Clone)]
pub struct Rule {
    pub identifier : String,
    pub pattern : Term
}

// grammar = { rule } ;
#[derive(Debug)]
pub struct EbnfAst {
    pub rules : Vec<Rule>,
    pub ignore_rule : Option<Term>,
}