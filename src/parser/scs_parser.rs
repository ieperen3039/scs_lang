use crate::{lexer::Token, scs_lexer::ScsToken};
use simple_error::SimpleError;

use super::{proto_ast::*, ErrResult, OkResult, ParseResult};
pub struct Parser {}

impl Parser {
    // scs_program             = [ version_declaration ], { definition | use_declaration }, function_block;
    pub fn parse(&self, tokens: &Vec<Token<ScsToken>>) -> ParseResult<FunctionBlock> {
        todo!()
    }

    // version_declaration     = KeywordVersion, Name, ( Star | version );
    // version                 = DecimalLiteral, Period, DecimalLiteral, Period, DecimalLiteral;

    // use_declaration         = KeywordUse, StringLiteral;

    // definition              = scope | type_definition | enum_definition | implementation | function_definition;
    // scope                   = Name, BracketOpen, { definition }, BracketClose;

    // type_definition         = KeywordType, Equal, ( type_name | KeywordNative ), [ BracketOpen, { function_definition }, BracketClose ];
    // type_name               = named_type | array_type | fn_type;
    // named_type              = Name, [ generic_type ];
    // generic_type            = AngleBracketOpen, Name, AngleBracketClose;
    // array_type              = type_name, SquareBracketOpen, SquareBracketClose;
    // fn_type                 = KeywordFn, [ AngleBracketOpen, [ ParenthesisOpen, { unnamed_parameter_list }, ParenthesisClose ], type_name, AngleBracketClose ];
    // unnamed_parameter_list  = type_name, { Comma, type_name };
    // enum_definition         = KeywordEnum, Name, SquareBracketOpen, { Name, Comma }, SquareBracketClose;

    // implementation          = KeywordImpl, type_name, BracketOpen, { function_definition }, BracketClose;

    // function_definition     = function_signature, ( function_block | KeywordNative );
    // function_signature      = KeywordFn, [ KeywordStatic ], Name, [ generic_type ], [ ParenthesisOpen, [ parameter_list ], ParenthesisClose ], Colon, type_name;
    // parameter_list          = parameter, { Comma, parameter };
    // parameter               = type_name, [ Ellipsis ], Name;
    // function_block          = BracketOpen, { statement }, [ last_statement ], BracketClose;
    // statement               = expression, { method_call }, [ assignment ], SemiColon;
    // last_statement          = expression, { method_call };
    // static_function_call    = Name, Period, method_call;
    // method_call             = Name, [ ParenthesisOpen, ( single_argument | named_argument_list ), ParenthesisClose ];
    // single_argument         = Name | named_argument;
    // named_argument_list     = [ named_argument, { Comma, named_argument } ];
    // named_argument          = Name, Colon, expression;
    // array_initialisation    = SquareBracketOpen, [ expression, { Comma, expression } ], SquareBracketClose;
    // literal                 = StringLiteral | DecimalLiteral | HexadecimalLiteral | BinaryLiteral;
    // assignment              = AngleBracketClose, ( KeywordReturn | [ type_name ], Name);

    // expression              = Name | static_function_call | function_block | array_initialisation | literal;
    fn process_expression(&self, remaining_tokens: &[Token<ScsToken>]) -> ParseResult<Expression> {
        todo!()
    }

    // static_function_call    = Name, Period, method_call;
    fn process_static_function_call(&self, tokens: &[Token<ScsToken>]) -> ParseResult<Expression> {
        let namespace = self.process_token(&tokens, ScsToken::Name)?;
        let period = self.process_token(namespace.remaining_tokens, ScsToken::Period)?;
        let function = self.process_method_call(period.remaining_tokens)?;

        Ok(OkResult {
            val: Expression::StaticFunctionCall {
                namespace: namespace.val,
                function: function.val,
            },
            remaining_tokens: function.remaining_tokens,
        })
    }

    // method_call             = Name, [ ParenthesisOpen, [ argument_list ], ParenthesisClose ];
    fn process_method_call(&self, tokens: &[Token<ScsToken>]) -> ParseResult<MethodCall> {
        let name = self.process_token(&tokens, ScsToken::Name)?;

        let maybe_open = self.process_token(name.remaining_tokens, ScsToken::ParenthesisOpen);
        let maybe_args = maybe_open.and_then(|p| self.process_argument_list(p.remaining_tokens));

        let maybe_close = if let Ok(args) = maybe_args {
            self.process_token(args.remaining_tokens, ScsToken::ParenthesisClose)
        } else {
            maybe_open
                .and_then(|p| self.process_token(p.remaining_tokens, ScsToken::ParenthesisClose))
        };

        if maybe_open.is_err() || maybe_close.is_err() {
            Ok(OkResult {
                val: MethodCall {
                    name: name.val,
                    arguments: ArgumentList::Empty,
                },
                remaining_tokens: name.remaining_tokens,
            })
        } else if let Ok(args) = maybe_args {
            Ok(OkResult {
                val: MethodCall {
                    name: name.val,
                    arguments: args.val,
                },
                remaining_tokens: maybe_close.unwrap().remaining_tokens,
            })
        } else {
            Ok(OkResult {
                val: MethodCall {
                    name: name.val,
                    arguments: ArgumentList::Empty,
                },
                remaining_tokens: maybe_close.unwrap().remaining_tokens,
            })
        }
    }

    // argument_list           = Name | named_argument, { Comma, named_argument };
    fn process_argument_list(&self, tokens: &[Token<ScsToken>]) -> ParseResult<ArgumentList> {
        let single_name = self.process_token(tokens, ScsToken::Name);

        if let Ok(name) = single_name {
            return Ok(OkResult {
                val: ArgumentList::Name(name.val),
                remaining_tokens: name.remaining_tokens,
            });
        }
        let mut all_arguments = Vec::new();

        let mut this_argument = self.process_named_argument(tokens)?;
        all_arguments.push(this_argument.val);

        while let Ok(comma) = self.process_token(this_argument.remaining_tokens, ScsToken::Comma) {
            this_argument = self.process_named_argument(comma.remaining_tokens)?;
            all_arguments.push(this_argument.val);
        }

        Ok(OkResult {
            val: ArgumentList::NamedList(all_arguments),
            remaining_tokens: this_argument.remaining_tokens,
        })
    }

    // named_argument          = Name, Colon, expression;
    fn process_named_argument(&self, tokens: &[Token<ScsToken>]) -> ParseResult<NamedArgument> {
        let parameter_name = self.process_token(&tokens, ScsToken::Name)?;
        let comma = self.process_token(parameter_name.remaining_tokens, ScsToken::Colon)?;
        let arg_value = self.process_expression(comma.remaining_tokens)?;

        Ok(OkResult {
            val: NamedArgument {
                parameter_name: parameter_name.val,
                value: arg_value.val,
            },
            remaining_tokens: arg_value.remaining_tokens,
        })
    }

    // if the first token in `tokens` is of the given token class, wrap the string slice of this token into an OkResult.
    // if the first token in `tokens` is not of the given token class, return an ErrResult::UnexpectedToken.
    // if there are no tokens in `tokens` return an ErrResult::OutOfTokens.
    fn process_token(&self, tokens: &[Token<ScsToken>], token: ScsToken) -> ParseResult<&str> {
        let found_token = tokens.get(0).ok_or(ErrResult::OutOfTokens {
            while_parsing: format!("{:?}", token),
        })?;

        if found_token.class == token {
            Ok(OkResult {
                val: found_token.slice,
                remaining_tokens: &tokens[1..],
            })
        } else {
            Err(ErrResult::UnexpectedToken {
                found: found_token.clone(),
                expected: vec![token],
            })
        }
    }
}
