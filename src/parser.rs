use simple_error::SimpleError;

use crate::{lexer::Lexer, scs_lexer::ScsTokens};

pub struct ScsParser {
    lexer : Lexer<ScsTokens>
}

impl ScsParser {
    pub fn new() -> ScsParser
    {
        ScsParser { lexer : Lexer::new_scs() }
    }

    pub fn parse(&self, program_string : &str) -> Result<(), SimpleError>
    {
        let tokens = self.lexer.read_all(program_string)?;

        // parse...

        Ok(())
    }
}