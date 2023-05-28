use simple_error::SimpleError;

use crate::{lexer::Token, scs_lexer::ScsToken};

struct ParseToken<'a> {
    base: Token<'a, ScsToken>,
}

enum TreeNode<'a> {
    Branch(Vec<TreeNode<'a>>),
    Token(ParseToken<'a>),
}

pub struct Parser<'a> {
    current_scope: TreeNode<'a>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Parser<'a> {
        todo!()
    }

    pub fn process(&mut self, tokens: &Vec<Token<'a, ScsToken>>) -> Result<(), SimpleError> {
        let token = tokens
            .first()
            .ok_or(SimpleError::new("Ran out of tokens"))?;

        match token.class {
            ScsToken::Whitespace | ScsToken::Comment | ScsToken::CommentBlock => Ok(()),
            ScsToken::BracketOpen
            | ScsToken::ParenthesisOpen
            | ScsToken::AngleBracketOpen
            | ScsToken::SquareBracketOpen => {
                // TreeNode::Branch(Vec::new());

                Ok(())
            }
            _ => Ok(()),
        }
    }
}
