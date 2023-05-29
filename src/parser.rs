use simple_error::SimpleError;

use crate::{lexer::Token, scs_lexer::ScsToken};

pub struct OkResult<'a, T> {
    val: T,
    remaining_tokens: &'a [Token<'a, ScsToken>],
}

pub enum ErrResult<'a> {
    Error(SimpleError),
    OutOfTokens {
        expected: ScsToken,
    },
    UnexpectedToken {
        found: Token<'a, ScsToken>,
        expected: ScsToken,
    },
}

type ParseResult<'a, T> = Result<OkResult<'a, T>, ErrResult<'a>>;

pub struct FunctionBody {
    statements: Vec<Statement>,
}

pub struct Statement {
    expressions: Vec<Expression>,
}

pub enum Expression {
    VariableName(String),
    StaticFunctionCall(StaticFunctionCall),
    MemberFunctionCall(MethodCall),
    FunctionBlock(FunctionBody),
}

pub struct StaticFunctionCall {
    namespace: String,
    name: String,
    arguments: Vec<NamedArgument>,
}

pub struct MethodCall {
    name: String,
    arguments: Vec<NamedArgument>,
}

pub struct NamedArgument {
    parameter_name: String,
    value: Expression,
}

pub struct Parser {}

impl Parser {
    pub fn new() -> Parser {
        todo!()
    }

    pub fn parse(&self, tokens: &Vec<Token<ScsToken>>) -> ParseResult<FunctionBody> {
        todo!()
    }

    fn process_expression(&self, tokens: &[Token<ScsToken>]) -> ParseResult<Expression> {
        todo!()
    }

    // static_function_call = Name, Colon, Colon, method_call
    fn process_static_function_call(
        &self,
        tokens: &[Token<ScsToken>],
    ) -> ParseResult<StaticFunctionCall> {
        let namespace = self.process_token(&tokens, ScsToken::Name)?;
        let colon1 = self.process_token(namespace.remaining_tokens, ScsToken::Colon)?;
        let colon2 = self.process_token(colon1.remaining_tokens, ScsToken::Colon)?;
        let method_call = self.process_method_call(colon2.remaining_tokens)?;

        Ok(OkResult {
            val: StaticFunctionCall {
                namespace: String::from(namespace.val),
                name: method_call.val.name,
                arguments: method_call.val.arguments,
            },
            remaining_tokens: method_call.remaining_tokens,
        })
    }

    // method_call = Name, [ParenthesisOpen, argument_list, ParenthesisClose]
    fn process_method_call(&self, tokens: &[Token<ScsToken>]) -> ParseResult<MethodCall> {
        let name = self.process_token(&tokens, ScsToken::Name)?;

        if let Ok(parenthesis) =
            self.process_token(name.remaining_tokens, ScsToken::ParenthesisOpen)
        {
            let argument_list = self.process_argument_list(parenthesis.remaining_tokens)?;
            let close_parenthesis =
                self.process_token(argument_list.remaining_tokens, ScsToken::ParenthesisClose)?;

            Ok(OkResult {
                val: MethodCall {
                    name: String::from(name.val),
                    arguments: argument_list.val,
                },
                remaining_tokens: close_parenthesis.remaining_tokens,
            })
        } else {
            Ok(OkResult {
                val: MethodCall {
                    name: String::from(name.val),
                    arguments: Vec::new(),
                },
                remaining_tokens: name.remaining_tokens,
            })
        }
    }

    // argument_list = named_argument, { Comma, named_argument }
    fn process_argument_list(&self, tokens: &[Token<ScsToken>]) -> ParseResult<Vec<NamedArgument>> {
        let mut all_arguments = Vec::new();

        let mut this_argument = self.process_named_argument(tokens)?;
        all_arguments.push(this_argument.val);

        while let Ok(comma) = self.process_token(this_argument.remaining_tokens, ScsToken::Comma) {
            this_argument = self.process_named_argument(comma.remaining_tokens)?;
            all_arguments.push(this_argument.val);
        }

        Ok(OkResult {
            val: all_arguments,
            remaining_tokens: this_argument.remaining_tokens,
        })
    }

    // named_argument = Name, Colon, expression
    fn process_named_argument(&self, tokens: &[Token<ScsToken>]) -> ParseResult<NamedArgument> {
        let parameter_name = self.process_token(&tokens, ScsToken::Name)?;
        let comma = self.process_token(parameter_name.remaining_tokens, ScsToken::Colon)?;
        let arg_value = self.process_expression(comma.remaining_tokens)?;

        Ok(OkResult {
            val: NamedArgument {
                parameter_name: String::from(parameter_name.val),
                value: arg_value.val,
            },
            remaining_tokens: arg_value.remaining_tokens,
        })
    }

    // if the first token in `tokens` is of the given token class, wrap the string slice of this token into an OkResult.
    // if the first token in `tokens` is not of the given token class, return an ErrResult::UnexpectedToken.
    // if there are no tokens in `tokens` return an ErrResult::OutOfTokens.
    fn process_token(&self, tokens: &[Token<ScsToken>], token: ScsToken) -> ParseResult<&str> {
        let found_token = tokens
            .get(0)
            .ok_or(ErrResult::OutOfTokens { expected: token })?;

        if found_token.class == token {
            Ok(OkResult {
                val: found_token.slice,
                remaining_tokens: &tokens[1..],
            })
        } else {
            Err(ErrResult::UnexpectedToken {
                found: found_token.clone(),
                expected: token,
            })
        }
    }
}
