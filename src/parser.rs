use simple_error::SimpleError;
use crate::ast::*;

use crate::{lexer::Token, scs_lexer::ScsToken};

struct ParseResult<'a> {
    node: ProtoExpression,
    remaining_tokens: &'a [Token<'a, ScsToken>],
}

struct ProtoFunctionBody {
    statements : Vec<ProtoStatement>
}

struct ProtoStatement {
    expressions : Vec<ProtoExpression>
}

enum ProtoExpression {
    VariableName(String),
    StaticFunctionCall(ProtoFunctionCall),
    MemberFunctionCall(ProtoFunctionCall),
    FunctionBlock(ProtoFunctionBody)
}

struct ProtoFunctionCall {
    name : String,
    parameters : Vec<ProtoExpression>,
}

pub struct Parser {
    variables: Vec<TypeDefinition>,
    functions: Vec<FunctionDefinition>,
}

impl Parser {
    pub fn new() -> Parser {
        todo!()
    }

    pub fn process(&self, tokens: &Vec<Token<ScsToken>>) -> Result<FunctionBody, SimpleError> {
        todo!()
    }

    // function_call = Name, [ParenthesisOpen, { Name, } ParenthesisClose]
    fn process_function_call(&self, tokens: &[Token<ScsToken>]) -> Option<ParseResult> {
        let name = Parser::get_first_if(&tokens, ScsToken::Name)?;

        if Parser::first_is(&tokens[1..], ScsToken::ParenthesisOpen) {
            let parameter_types: Vec<String>;
            let token_idx = 2;

            while let Some(variable_name) =
                Parser::get_first_if(&tokens[token_idx..], ScsToken::Name)
            {
            }
        }

        todo!()
    }

    fn get_first_if<'a>(tokens: &[Token<'a, ScsToken>], token: ScsToken) -> Option<&'a str> {
        tokens.get(0).filter(|t| t.class == token).map(|t| t.slice)
    }

    fn first_is(tokens: &[Token<ScsToken>], token: ScsToken) -> bool {
        tokens.get(0).map(|t| t.class == token).unwrap_or(false)
    }
}
