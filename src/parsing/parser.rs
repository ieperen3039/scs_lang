use std::fmt::Pointer;

use simple_error::SimpleError;

use super::ebnf_ast;

pub type ParseResult<'prog, 'bnf> = Result<OkResult<'prog, 'bnf>, ErrResult>;

// the entire resulting syntax tree consists of these nodes
pub struct RuleNode<'prog, 'bnf> {
    rule: &'bnf str,
    tokens: &'prog str,
    subrules: Vec<RuleNode<'prog, 'bnf>>,
}

impl<'prog, 'bnf> std::fmt::Debug for RuleNode<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.subrules.is_empty() {
            f.write_fmt(format_args!("{{{}}}", self.rule))
        } else {
            f.write_fmt(format_args!("{{{}, {:?}}}", self.rule, self.subrules))
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
    remaining_tokens: &'prog str,
}

#[derive(Debug)]
enum ParseNode<'prog, 'bnf> {
    Rule(RuleNode<'prog, 'bnf>),
    Group(Vec<RuleNode<'prog, 'bnf>>),
}

pub fn parse_program_with_grammar<'prog, 'bnf>(
    program: &'prog String,
    grammar: &'bnf ebnf_ast::EbnfAst,
) -> Result<RuleNode<'prog, 'bnf>, ErrResult> {
    if grammar.rules.is_empty() {
        return Err(ErrResult::Error(SimpleError::new("No rules in grammar")));
    }

    let primary_rule = grammar.rules.get(0).expect("Grammar must have rules");
    let result = apply_rule(&program, primary_rule, grammar)?;

    if let ParseNode::Rule(rule_node) = result.val {
        return Ok(rule_node);
    } else {
        return Err(ErrResult::InternalError(SimpleError::new(
            "Primary rule was no rule",
        )));
    }
}

// apply rule on tokens
fn apply_rule<'prog, 'bnf>(tokens: &'prog str, rule: &'bnf ebnf_ast::Rule, grammar: &'bnf ebnf_ast::EbnfAst) -> ParseResult<'prog, 'bnf> {
    let found_rule = apply_term(tokens, &rule.pattern, grammar)?;

    let num_bytes_parsed = tokens.len() - found_rule.remaining_tokens.len();
    if num_bytes_parsed == 0 {
        return Err(ErrResult::Error(SimpleError::new(format!(
            "Rule '{}' yielded nothing",
            rule.identifier
        ))));
    }

    let sub_rules = match found_rule.val {
        ParseNode::Rule(node) => vec![node],
        ParseNode::Group(sub_rules) => sub_rules,
    };

    Ok(OkResult {
        val: ParseNode::Rule(RuleNode {
            rule: &rule.identifier,
            tokens: &tokens[..num_bytes_parsed],
            subrules: sub_rules,
        }),
        remaining_tokens: found_rule.remaining_tokens,
    })
}

fn apply_term<'prog, 'bnf>(tokens: &'prog str, term: &'bnf ebnf_ast::Term, grammar: &'bnf ebnf_ast::EbnfAst) -> ParseResult<'prog, 'bnf> {
    match term {
        ebnf_ast::Term::Optional(sub_term) => apply_optional(tokens, sub_term, grammar),
        ebnf_ast::Term::Repetition(sub_term) => apply_repeptition(tokens, sub_term, grammar),
        ebnf_ast::Term::Concatenation(sub_terms) => apply_concatenation(tokens, sub_terms, grammar),
        ebnf_ast::Term::Alternation(sub_terms) => apply_alternation(tokens, sub_terms, grammar),
        ebnf_ast::Term::Terminal(literal) => parse_literal(tokens, literal),
        ebnf_ast::Term::Identifier(rule_name) => parse_rule_identifier(tokens, rule_name, grammar),
    }
}

fn apply_alternation<'prog, 'bnf>(
    tokens: &'prog str,
    sub_terms: &'bnf [ebnf_ast::Term], grammar: &'bnf ebnf_ast::EbnfAst,
) -> ParseResult<'prog, 'bnf> {
    let mut problems = Vec::new();

    for term in sub_terms {
        let result = apply_term(tokens, term, grammar);

        match result {
            Ok(ok_result) => {
                let num_bytes_parsed = tokens.len() - ok_result.remaining_tokens.len();
                // if the remaining_tokens is the same as the input tokens, that means that this term evaluated to nothing
                if num_bytes_parsed > 0 {
                    return Ok(ok_result);
                }
                // return the first successful
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
                tokens_remaining,
                while_parsing: _,
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

fn apply_concatenation<'prog, 'bnf>(
    tokens: &'prog str,
    sub_terms: &'bnf [ebnf_ast::Term], grammar: &'bnf ebnf_ast::EbnfAst,
) -> ParseResult<'prog, 'bnf> {
    let mut nodes = Vec::new();
    let mut remaining_tokens = tokens;

    for sub_term in sub_terms {
        let term_result = apply_term(remaining_tokens, sub_term, grammar);
        match term_result {
            Ok(result) => {
                match result.val {
                    ParseNode::Rule(rule_node) => nodes.push(rule_node),
                    ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
                }
                remaining_tokens = result.remaining_tokens
            }
            Err(result) => return Err(result),
        }
    }

    Ok(OkResult {
        val: ParseNode::Group(nodes),
        remaining_tokens: tokens,
    })
}

fn apply_repeptition<'prog, 'bnf>(
    tokens: &'prog str,
    sub_term: &'bnf ebnf_ast::Term, grammar: &'bnf ebnf_ast::EbnfAst,
) -> ParseResult<'prog, 'bnf> {
    let mut nodes = Vec::new();
    let mut remaining_tokens = tokens;

    while let Ok(result) = apply_term(remaining_tokens, sub_term, grammar) {
        match result.val {
            ParseNode::Rule(rule_node) => nodes.push(rule_node),
            ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
        }
        remaining_tokens = result.remaining_tokens
    }

    Ok(OkResult {
        val: ParseNode::Group(nodes),
        remaining_tokens,
    })
}

fn apply_optional<'prog, 'bnf>(
    tokens: &'prog str,
    sub_term: &'bnf ebnf_ast::Term, grammar: &'bnf ebnf_ast::EbnfAst,
) -> ParseResult<'prog, 'bnf> {
    match apply_term(tokens, sub_term, grammar) {
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

fn parse_literal<'prog, 'bnf>(
    tokens: &'prog str,
    literal: &'bnf str,
) -> Result<OkResult<'prog, 'bnf>, ErrResult> {
    if tokens.starts_with(literal) {
        Ok(OkResult {
            val: ParseNode::Rule(RuleNode {
                rule: literal,
                tokens,
                subrules: Vec::new(),
            }),
            remaining_tokens: &tokens[literal.len()..],
        })
    } else {
        Err(ErrResult::UnexpectedToken {
            tokens_remaining: tokens.len(),
            while_parsing: literal.to_string(),
        })
    }
}

fn parse_rule_identifier<'prog, 'bnf>(
    tokens: &'prog str,
    rule_name: &'bnf str,
    grammar: &'bnf ebnf_ast::EbnfAst,
) -> Result<OkResult<'prog, 'bnf>, ErrResult> {
    let rule = grammar.rules.iter().find(|rule| rule.identifier == rule_name);

    match rule {
        Some(rule) => apply_rule(tokens, rule, grammar),
        None => Err(ErrResult::UnexpectedToken { tokens_remaining: tokens.len(), while_parsing: rule_name.to_string() }),
    }
}
