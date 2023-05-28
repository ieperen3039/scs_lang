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
        let tokens = self.lexer.read_all(program_string)?;

        let mut parser = Parser::new();
        let result = parser.process(&tokens)?;

        Ok(())
    }
}