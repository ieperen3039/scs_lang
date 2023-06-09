use std::cmp;

use regex::Regex;
use simple_error::SimpleError;

use super::ebnf_ast;

type ParseResult<'prog, 'bnf> = Result<OkResult<'prog, 'bnf>, ErrResult<'bnf>>;

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
pub enum ErrResult<'bnf> {
    // the worst error of no errors at all
    Multiple {
        errors: Vec<ErrResult<'bnf>>
    },
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        tokens_remaining: usize,
        expected: &'bnf str
    },
    // the end of the program was reached, but more was expected (equivalent to an UnexpectedToken at the end of the program)
    OutOfTokens {
        expected: &'bnf str,
    },
    // a group was opened, but never closed (equivalent to an early-evaluated OutOfTokens)
    UnclosedGroup {
        tokens_remaining: usize,
    },
    // the program or grammar has some unspecified syntax error
    Error(SimpleError),
    // the parser has a bug
    InternalError(SimpleError),
}

pub fn error_string(error: &ErrResult, source: &str) -> String {
    match error {
        ErrResult::UnexpectedToken {
            tokens_remaining, ..
        }
        | ErrResult::UnclosedGroup { tokens_remaining } => {
            let offset = source.len() - tokens_remaining;
            let line_number = source[..=offset].bytes().filter(|c| c == &b'\n').count();
            let lower_newline = source[..=offset].rfind("\n").map(|v| v + 1).unwrap_or(0);
            let upper_newline = source[offset..]
                .find("\n")
                .map(|v| v + offset)
                .unwrap_or(source.len());
            let lb = cmp::max(offset as i64 - 40, lower_newline as i64) as usize;
            let ub = cmp::min(offset as i64 + 40, upper_newline as i64) as usize;
            format!(
                "{:?} on line {line_number}:\n\n{:>40}{:<40}\n{:>40}^ when parsing here",
                error,
                &source[lb..offset],
                &source[offset..ub],
                ""
            )
        }
        _ => format!("{:?}", error),
    }
}

#[derive(Debug)]
struct OkResult<'prog, 'bnf> {
    val: ParseNode<'prog, 'bnf>,
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
) -> Result<RuleNode<'prog, 'bnf>, ErrResult<'bnf>> {
    if grammar.rules.is_empty() {
        return Err(ErrResult::Error(SimpleError::new("No rules in grammar")));
    }

    let primary_rule = grammar.rules.get(0).expect("Grammar must have rules");

    let result = grammar.apply_rule(program, primary_rule)?;

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
        let rule_result = self.apply_term(tokens, &rule.pattern)?;

        let is_transparent = rule.identifier.as_bytes()[0] == b'_';
        if is_transparent {
            Ok(rule_result)
        } else {
            let num_tokens = tokens.len() - rule_result.remaining_tokens.len();
            if num_tokens == 0 {
                return Err(ErrResult::Error(SimpleError::new(format!(
                    "Rule '{}' yielded nothing",
                    rule.identifier
                ))));
            }

            let sub_rules = match rule_result.val {
                ParseNode::Rule(node) => vec![node],
                ParseNode::Group(sub_rules) => sub_rules,
                ParseNode::Terminal(_) => Vec::new(),
            };

            Ok(OkResult {
                val: ParseNode::Rule(RuleNode {
                    rule: &rule.identifier,
                    tokens: &tokens[..num_tokens],
                    sub_rules,
                }),
                remaining_tokens: rule_result.remaining_tokens,
            })
        }
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
            ebnf_ast::Term::Literal(literal) => self.parse_literal(tokens, literal),
            ebnf_ast::Term::Identifier(rule_name) => self.parse_rule_identifier(tokens, rule_name),
            ebnf_ast::Term::Regex(ebnf_ast::RegexWrapper { regex }) => {
                self.parse_regex(tokens, regex)
            }
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
                    // if this is a internal error, return it.
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
        let mut furthest_err = ErrResult::Multiple { errors: Vec::new() };

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
                ErrResult::OutOfTokens { .. } => {
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

        for sub_term in sub_terms {
            let term_result = self.apply_term(remaining_tokens, sub_term);
            match term_result {
                Ok(result) => {
                    match result.val {
                        ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
                        ParseNode::Rule(rule) => nodes.push(rule),
                        _ => {} // literals are ignored
                    }
                    remaining_tokens = result.remaining_tokens;
                }
                Err(result) => return Err(result),
            }
        }

        Ok(OkResult {
            val: ParseNode::Group(nodes),
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

        while let Ok(result) = self.apply_term(remaining_tokens, sub_term) {
            match result.val {
                ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
                ParseNode::Rule(rule) => nodes.push(rule),
                _ => {} // literals are ignored
            }
            remaining_tokens = result.remaining_tokens;
        }

        Ok(OkResult {
            val: ParseNode::Group(nodes),
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
                    remaining_tokens: tokens,
                })
            }
            Err(err) => Err(err),
        }
    }

    fn parse_regex<'prog>(
        &'bnf self,
        tokens: &'prog str,
        regex: &'bnf Regex,
    ) -> ParseResult<'prog, 'bnf> {
        if let Some(found) = regex.find(tokens) {
            let slice = &tokens[..found.end()];
            Ok(OkResult {
                val: ParseNode::Terminal(slice),
                remaining_tokens: &tokens[found.end()..],
            })
        } else {
            Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: regex.as_str(),
            })
        }
    }

    fn parse_literal<'prog>(
        &'bnf self,
        tokens: &'prog str,
        literal: &'bnf str,
    ) -> ParseResult<'prog, 'bnf> {
        if tokens.starts_with(literal) {
            Ok(OkResult {
                val: ParseNode::Terminal(&tokens[..literal.len()]),
                remaining_tokens: &tokens[literal.len()..],
            })
        } else {
            Err(ErrResult::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: literal,
            })
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
                expected: rule_name,
            }),
        }
    }
}
