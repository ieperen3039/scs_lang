use crate::lexer::Lexer;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ScsToken {
    Whitespace,
    Name,
    NumberLiteral,
    StringLiteral,
    ParenthesisOpen,
    ParenthesisClose,
    BracketOpen,
    BracketClose,
    SquareBracketOpen,
    SquareBracketClose,
    AngleBracketOpen,
    AngleBracketClose,
    Colon,
    SemiColon,
    Comma,
    Period,
    Comment,
    CommentBlock,
}

impl Lexer<ScsToken> {
    pub fn new_scs() -> Lexer<ScsToken> {
        Lexer::new(vec![
            (ScsToken::Whitespace, r#"[\s\n]+"#),
            (ScsToken::Comment, r#"\/\/.*"#),
            (ScsToken::CommentBlock, r#"\/\*.*\*\/"#),
            (ScsToken::BracketOpen, r#"\{"#),
            (ScsToken::BracketClose, r#"\}"#),
            (ScsToken::ParenthesisOpen, r#"\("#),
            (ScsToken::ParenthesisClose, r#"\)"#),
            (ScsToken::SquareBracketOpen, r#"\["#),
            (ScsToken::SquareBracketClose, r#"\]"#),
            (ScsToken::AngleBracketOpen, r#"<"#),
            (ScsToken::AngleBracketClose, r#">"#),
            (ScsToken::Colon, r#":"#),
            (ScsToken::SemiColon, r#";"#),
            (ScsToken::Comma, r#","#),
            (ScsToken::Period, r#"\."#),
            (ScsToken::Name, r#"[a-zA-Z_][a-zA-Z0-9_]*"#),
            (ScsToken::StringLiteral, r#""(.*?[^\\]|)\""#),
            (ScsToken::NumberLiteral, r#"[\d_]+.?[\d_]*"#),
        ])
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ProgramToken {
    SheBang
}

pub fn create_program_lexer() -> Lexer<ProgramToken> {
    Lexer::new(vec![
        (ProgramToken::SheBang, r#"#!.*"#),
    ])
}
