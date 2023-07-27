#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenClass {
    IDENTIFIER,
    NUMERIC,
    STRING,
    WHITESPACE,
    SYMBOL,
}

impl TokenClass {
    pub fn str(&self) -> &'static str {
        match self {
            IDENTIFIER => "IDENTIFIER",
            NUMERIC => "NUMERIC",
            STRING => "STRING",
            WHITESPACE => "WHITESPACE",
            SYMBOL => "SYMBOL",
        }
    }
    
    pub fn from_str(string : &str) -> Self {
        match string {
            "IDENTIFIER" => Self::IDENTIFIER,
            "NUMERIC" => Self::NUMERIC,
            "STRING" => Self::STRING,
            "WHITESPACE" => Self::WHITESPACE,
            "SYMBOL" => Self::SYMBOL,
        }
    }
}

#[derive(Clone, Debug, Eq)]
pub struct Token<'a> {
    pub class: TokenClass,
    pub slice: &'a str,
    pub char_idx: usize,
}

impl<'a> Token<'a> {
    pub fn is_similar_to(&self, other: &Self) -> bool {
        self.slice == other.slice
    }
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.slice)
    }
}

impl<'a> std::hash::Hash for Token<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.char_idx.hash(state);
    }
}

impl<'a> PartialEq for Token<'a> {
    // true iff it refers to the exact same token in the program.
    // A token with the same characters at a different place is therefore not equal
    fn eq(&self, other: &Self) -> bool {
        self.char_idx == other.char_idx
    }
}