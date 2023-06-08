use simple_error::SimpleError;

use super::ebnf_ast;

type ParseResult<'prog, 'bnf> = Result<OkResult<'prog, 'bnf>, ErrResult>;

// the entire resulting syntax tree consists of these nodes
#[derive(PartialEq, Eq, Clone)]
pub struct RuleNode<'prog, 'bnf> {
    pub rule: &'bnf str,
    pub tokens: &'prog str,
    pub sub_rules: Vec<RuleNode<'prog, 'bnf>>,
}

impl<'prog, 'bnf> std::fmt::Debug for RuleNode<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sub_rules.is_empty() {
            f.write_fmt(format_args!("{{{}, \"{}\"}}", self.rule, self.tokens))
        } else {
            f.write_fmt(format_args!("{{{}, {:?}}}", self.rule, self.sub_rules))
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrResult {
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        tokens_remaining: usize,
        while_parsing: String,
    },
    // the end of the program was reached, but more was expected (equivalent to an UnexpectedToken at the end of the program)
    OutOfTokens,
    // a group was opened, but never closed (equivalent to an early-evaluated OutOfTokens)
    UnclosedGroup {
        tokens_remaining: usize,
    },
    // the program or grammar has some unspecified syntax error
    Error(SimpleError),
    // the parser has a bug
    InternalError(SimpleError),
}

#[derive(Debug)]
struct OkResult<'prog, 'bnf> {
    val: ParseNode<'prog, 'bnf>,
    num_tokens_not_consumed : usize,
    remaining_tokens: &'prog str,
}

#[derive(Debug)]
enum ParseNode<'prog, 'bnf> {
    Rule(RuleNode<'prog, 'bnf>),
    Group(Vec<RuleNode<'prog, 'bnf>>),
    Terminal(&'prog str),
}

pub fn parse_program_with_grammar<'prog, 'bnf>(
    program: &'prog str,
    grammar: &'bnf ebnf_ast::EbnfAst,
) -> Result<RuleNode<'prog, 'bnf>, ErrResult> {
    if grammar.rules.is_empty() {
        return Err(ErrResult::Error(SimpleError::new("No rules in grammar")));
    }

    let primary_rule = grammar.rules.get(0).expect("Grammar must have rules");

    // skip whitespace
    let tokens = grammar.next_of(program);
    let result = grammar.apply_rule(tokens, primary_rule)?;

    if !result.remaining_tokens.is_empty() {
        return Err(ErrResult::UnclosedGroup {
            tokens_remaining: result.remaining_tokens.len(),
        });
    }

    if let ParseNode::Rule(rule_node) = result.val {
        return Ok(rule_node);
    } else {
        return Err(ErrResult::InternalError(SimpleError::new(
            "Primary rule was no rule",
        )));
    }
}

impl<'bnf> ebnf_ast::EbnfAst {
    // apply rule on tokens
    fn apply_rule<'prog>(
        &'bnf self,
        tokens: &'prog str,
        rule: &'bnf ebnf_ast::Rule,
    ) -> ParseResult<'prog, 'bnf> {
        let found_rule = self.apply_term(tokens, &rule.pattern)?;

        let num_tokens_consumed = tokens.len() - found_rule.num_tokens_not_consumed;
        if num_tokens_consumed == 0 {
            return Err(ErrResult::Error(SimpleError::new(format!(
                "Rule '{}' yielded nothing",
                rule.identifier
            ))));
        }

        let sub_rules = match found_rule.val {
            ParseNode::Rule(node) => vec![node],
            ParseNode::Group(sub_rules) => sub_rules,
            ParseNode::Terminal(_) => Vec::new(),
        };

        Ok(OkResult {
            val: ParseNode::Rule(RuleNode {
                rule: &rule.identifier,
                tokens : &tokens[..num_tokens_consumed],
                sub_rules,
            }),
            num_tokens_not_consumed: found_rule.num_tokens_not_consumed,
            remaining_tokens: found_rule.remaining_tokens,
        })
    }

    fn apply_term<'prog>(
        &'bnf self,
        tokens: &'prog str,
        term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        match term {
            ebnf_ast::Term::Optional(sub_term) => self.apply_optional(tokens, sub_term),
            ebnf_ast::Term::Repetition(sub_term) => self.apply_repetition(tokens, sub_term),
            ebnf_ast::Term::Concatenation(sub_terms) => self.apply_concatenation(tokens, sub_terms),
            ebnf_ast::Term::Alternation(sub_terms) => self.apply_alternation(tokens, sub_terms),
            ebnf_ast::Term::Terminal(literal) => self.parse_literal(tokens, literal),
            ebnf_ast::Term::Identifier(rule_name) => self.parse_rule_identifier(tokens, rule_name),
        }
    }

    fn apply_alternation<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let mut problems = Vec::new();

        for term in sub_terms {
            let result = self.apply_term(tokens, term);

            match result {
                Ok(ok_result) => {
                    let num_bytes_parsed = tokens.len() - ok_result.remaining_tokens.len();
                    // if the remaining_tokens is the same as the input tokens, that means that this term evaluated to nothing
                    if num_bytes_parsed > 0 {
                        // return the first successful
                        return Ok(ok_result);
                    }
                }
                Err(ErrResult::InternalError(_)) => {
                    // if this is a syntax-error (or internal error), return it.
                    return result;
                }
                Err(err_result) => {
                    // otherwise, just store it in case no rule applies
                    problems.push(err_result)
                }
            }
        }

        // no rule applied, so we look for the best parse result that we could find
        let mut least_remaining: usize = 0;
        let mut furthest_err = ErrResult::OutOfTokens;

        for err in problems {
            match err {
                ErrResult::UnclosedGroup { tokens_remaining }
                | ErrResult::UnexpectedToken {
                    tokens_remaining, ..
                } => {
                    if tokens_remaining < least_remaining {
                        least_remaining = tokens_remaining;
                        furthest_err = err;
                    }
                }
                ErrResult::OutOfTokens => {
                    return Err(err);
                }
                _ => (),
            }
        }

        Err(furthest_err)
    }

    fn apply_concatenation<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let mut nodes = Vec::new();
        let mut remaining_tokens = tokens;
        let mut num_tokens_not_consumed = tokens.len();

        for sub_term in sub_terms {
            let term_result = self.apply_term(remaining_tokens, sub_term);
            match term_result {
                Ok(result) => {
                    match result.val {
                        ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
                        ParseNode::Rule(rule) => nodes.push(rule),
                        _ => {}
                    }
                    remaining_tokens = result.remaining_tokens;
                    num_tokens_not_consumed = result.num_tokens_not_consumed;
                }
                Err(result) => return Err(result),
            }
        }

        Ok(OkResult {
            val: ParseNode::Group(nodes),
            num_tokens_not_consumed,
            remaining_tokens,
        })
    }

    fn apply_repetition<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        let mut nodes = Vec::new();
        let mut remaining_tokens = tokens;
        let mut num_tokens_not_consumed = tokens.len();

        while let Ok(result) = self.apply_term(remaining_tokens, sub_term) {
            match result.val {
                ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
                ParseNode::Rule(rule) => nodes.push(rule),
                _ => {}
            }
            remaining_tokens = result.remaining_tokens;
            num_tokens_not_consumed = result.num_tokens_not_consumed;
        }

        Ok(OkResult {
            val: ParseNode::Group(nodes),
            num_tokens_not_consumed,
            remaining_tokens,
        })
    }

    fn apply_optional<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        match self.apply_term(tokens, sub_term) {
            Ok(result) => Ok(result),
            Err(ErrResult::UnexpectedToken { .. }) => {
                // return an empty non-producing syntax node
                Ok(OkResult {
                    val: ParseNode::Group(vec![]),
                    num_tokens_not_consumed: tokens.len(),
                    remaining_tokens: tokens,
                })
            }
            Err(err) => Err(err),
        }
    }

    fn parse_literal<'prog>(
        &'bnf self,
        tokens: &'prog str,
        literal: &'bnf str,
    ) -> ParseResult<'prog, 'bnf> {
        if tokens.starts_with(literal) {
            let remaining_tokens = &tokens[literal.len()..];
            Ok(OkResult {
                val: ParseNode::Terminal(remaining_tokens),
                num_tokens_not_consumed: remaining_tokens.len(),
                remaining_tokens: self.next_of(remaining_tokens),
            })
        } else {
            Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                while_parsing: literal.to_string(),
            })
        }
    }

    fn next_of<'prog>(&self, new_tokens: &'prog str) -> &'prog str {
        if let Some(ignore) = &self.ignore_rule {
            self.apply_term(new_tokens, ignore)
                .map(|res| res.remaining_tokens)
                .unwrap_or(new_tokens)
        } else {
            new_tokens
        }
    }

    fn parse_rule_identifier<'prog>(
        &'bnf self,
        tokens: &'prog str,
        rule_name: &'bnf str,
    ) -> ParseResult<'prog, 'bnf> {
        let rule = self.rules.iter().find(|rule| rule.identifier == rule_name);

        match rule {
            Some(rule) => self.apply_rule(tokens, rule),
            None => Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                while_parsing: rule_name.to_string(),
            }),
        }
    }
}
