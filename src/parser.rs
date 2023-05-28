use simple_error::SimpleError;

use crate::{lexer::{Lexer, Token}, scs_lexer::ScsToken};

pub struct ScsCompiler {
    lexer : Lexer<ScsToken>
}

struct ParseToken<'a>
{
    base : Token<'a, ScsToken>,

}

struct Parser<'a>
{
    tokens : Vec<Token<'a, ScsToken>>
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

        for token in tokens {
            match token.class {
                ScsToken::Whitespace |
                ScsToken::Comment |
                ScsToken::CommentBlock => {}
                _ => parser.process(token)
            }
        }

        Ok(())
    }
}

impl <'a> Parser<'a> {
    fn new() -> Parser<'a> {
        Parser { tokens : Vec::new() }
    }

    fn process(&mut self, token: Token<'a, ScsToken>) {
        
    }
}