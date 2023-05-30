use crate::lexer::{
    Lexer,
    RawLexMethod::{Literal, Regex},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ScsToken {
    Name,
    DecimalLiteral,
    HexadecimalLiteral,
    BinaryLiteral,
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
    ThreeDots,
    Period,
    EqualSign,
    KeywordType,
    KeywordFn,
    KeywordEnum,
    KeywordNative,
    KeywordThis,
    KeywordVoid,
    KeywordReturn,
    KeywordStatic,
    KeywordUse,
    // ignored
    Whitespace,
    Comment,
    CommentBlock,
}

impl Lexer<ScsToken> {
    pub fn new_scs() -> Lexer<ScsToken> {
        Lexer::new(vec![
            (ScsToken::Whitespace, Regex(r#"[\s\n]+"#)),
            (ScsToken::Comma, Literal(",")),
            (ScsToken::Colon, Literal(":")),
            (ScsToken::SemiColon, Literal(";")),
            (ScsToken::ParenthesisOpen, Literal("(")),
            (ScsToken::ParenthesisClose, Literal(")")),
            (ScsToken::BracketOpen, Literal("{")),
            (ScsToken::BracketClose, Literal("}")),
            (ScsToken::SquareBracketOpen, Literal("[")),
            (ScsToken::SquareBracketClose, Literal("]")),
            (ScsToken::AngleBracketOpen, Literal("<")),
            (ScsToken::AngleBracketClose, Literal(">")),
            (ScsToken::ThreeDots, Literal("...")),
            (ScsToken::Period, Literal(".")),
            (ScsToken::EqualSign, Literal("=")),
            (ScsToken::KeywordType, Literal("type")),
            (ScsToken::KeywordFn, Literal("fn")),
            (ScsToken::KeywordEnum, Literal("enum")),
            (ScsToken::KeywordNative, Literal("native")),
            (ScsToken::KeywordThis, Literal("this")),
            (ScsToken::KeywordVoid, Literal("void")),
            (ScsToken::KeywordReturn, Literal("return")),
            (ScsToken::KeywordStatic, Literal("static")),
            (ScsToken::KeywordUse, Literal("use")),
            (ScsToken::Comment, Regex(r#"\/\/.*"#)),
            (ScsToken::CommentBlock, Regex(r#"\/\*.*\*\/"#)),
            (ScsToken::Name, Regex(r#"[a-zA-Z_][a-zA-Z0-9_]*"#)),
            (ScsToken::StringLiteral, Regex(r#""(.*?[^\\]|)\""#)),
            (ScsToken::DecimalLiteral, Regex(r#"0-9[0-9_]*.?[0-9]*"#)),
            (ScsToken::HexadecimalLiteral, Regex(r#"0x[0-9A-F_]+"#)),
            (ScsToken::BinaryLiteral, Regex(r#"0b[01_]+.?[01_]*"#)),
        ])
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ProgramToken {
    SheBang,
}

pub fn create_program_lexer() -> Lexer<ProgramToken> {
    Lexer::new(vec![(ProgramToken::SheBang, Regex(r#"#!.*"#))])
}
