use crate::lexer::{
    Lexer,
    RawLexMethod::{Literal, Regex, Word},
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
    Ellipsis,
    Period,
    Equal,
    Minus,
    Star,
    KeywordType,
    KeywordFn,
    KeywordImpl,
    KeywordEnum,
    KeywordNative,
    KeywordThis,
    KeywordVoid,
    KeywordReturn,
    KeywordStatic,
    KeywordUse,
    KeywordVersion,

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
            (ScsToken::Ellipsis, Word("...")),
            (ScsToken::Period, Literal(".")),
            (ScsToken::Equal, Literal("=")),
            (ScsToken::KeywordType, Word("type")),
            (ScsToken::KeywordFn, Word("fn")),
            (ScsToken::KeywordEnum, Word("enum")),
            (ScsToken::KeywordNative, Word("native")),
            (ScsToken::KeywordThis, Word("this")),
            (ScsToken::KeywordVoid, Word("void")),
            (ScsToken::KeywordReturn, Word("return")),
            (ScsToken::KeywordStatic, Word("static")),
            (ScsToken::KeywordUse, Word("use")),
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
