use simple_error::SimpleError;

use crate::{lexer::{Lexer, Token}, scs_lexer::ScsToken, parser::Parser};

pub struct ScsCompiler {
    lexer : Lexer<ScsToken>
}

impl ScsCompiler {
    pub fn new() -> ScsCompiler
    {
        ScsCompiler { lexer : Lexer::new_scs() }
    }

    pub fn compile(&self, program_string : &str) -> Result<(), SimpleError>
    {
        let mut tokens : Vec<Token<ScsToken>> = self.lexer.read_all(program_string)?;
        tokens.retain(|t| (t.class != ScsToken::Whitespace) && (t.class != ScsToken::Comment) && (t.class != ScsToken::CommentBlock));

        let mut parser = Parser::new();
        let result = parser.process(&tokens)?;

        Ok(())
    }
}