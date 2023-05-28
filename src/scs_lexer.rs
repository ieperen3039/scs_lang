use crate::lexer::Lexer;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ScsTokens {
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
    Comment,
    CommentBlock,
}

pub fn create_scs_lexer() -> Lexer<ScsTokens> {
    Lexer::new(vec![
        (ScsTokens::Whitespace, r#"[\s\n]+"#),
        (ScsTokens::Comment, r#"\/\/.*"#),
        (ScsTokens::CommentBlock, r#"\/\*.*\*\/"#),
        (ScsTokens::BracketOpen, r#"\{"#),
        (ScsTokens::BracketClose, r#"\}"#),
        (ScsTokens::ParenthesisOpen, r#"\("#),
        (ScsTokens::ParenthesisClose, r#"\)"#),
        (ScsTokens::SquareBracketOpen, r#"\["#),
        (ScsTokens::SquareBracketClose, r#"\]"#),
        (ScsTokens::AngleBracketOpen, r#"<"#),
        (ScsTokens::AngleBracketClose, r#">"#),
        (ScsTokens::Name, r#"[a-zA-Z_][a-zA-Z0-9_]*"#),
        (ScsTokens::StringLiteral, r#""(.*?[^\\]|)\""#),
        (ScsTokens::NumberLiteral, r#"[\d_]+.?[\d_]*"#),
    ])
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ProgramTokens {
    SheBang
}

pub fn create_program_lexer() -> Lexer<ProgramTokens> {
    Lexer::new(vec![
        (ProgramTokens::SheBang, r#"#!.*"#),
    ])
}
