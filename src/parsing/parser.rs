use core::panic;

use simple_error::SimpleError;

use super::ebnf_ast;

pub type ParseResult<'prog, 'bnf> = Result<OkResult<'prog, 'bnf>, ErrResult>;

// the entire resulting syntax tree consists of these nodes
#[derive(Debug)]
pub struct RuleNode<'prog, 'bnf> {
    rule: &'bnf str,
    tokens: &'prog str,
    subrules: Vec<RuleNode<'prog, 'bnf>>,
}

#[derive(Debug)]
pub enum ErrResult {
    Error(SimpleError),
    OutOfTokens,
    UnclosedGroup {
        tokens_remaining: usize,
    },
    UnexpectedToken {
        tokens_remaining: usize,
        while_parsing: &'static str,
    },
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
    program: String,
    grammar: ebnf_ast::EbnfAst,
) -> Result<RuleNode<'prog, 'bnf>, ErrResult> {
    todo!()
}

// apply rule on tokens
fn apply_any_rule<'prog, 'bnf>(
    tokens: &'prog str,
    grammar: ebnf_ast::EbnfAst,
) -> ParseResult<'prog, 'bnf> {
    let mut problems = Vec::new();

    for rule in grammar.rules {
        let result = apply_term(tokens, rule.pattern);

        match result {
            Ok(found_rule) => {
                let num_bytes_parsed = tokens.len() - found_rule.remaining_tokens.len();
                // if the remaining_tokens is the same as the input tokens, that means that this rule parsed nothing
                if num_bytes_parsed > 0 {
                    let sub_rules = match found_rule.val {
                        ParseNode::Rule(node) => vec![node],
                        ParseNode::Group(sub_rules) => sub_rules,
                    };

                    return Ok(OkResult {
                        val: ParseNode::Rule(RuleNode {
                            rule: &rule.identifier,
                            tokens: &tokens[..num_bytes_parsed],
                            subrules: sub_rules,
                        }),
                        remaining_tokens: found_rule.remaining_tokens,
                    });
                }
            }
            Err(ErrResult::OutOfTokens)
            | Err(ErrResult::UnclosedGroup { .. })
            | Err(ErrResult::Error(_)) => {
                // if this is a syntax-error, return it.
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
        if let ErrResult::UnexpectedToken {
            tokens_remaining,
            while_parsing: _,
        } = err
        {
            if tokens_remaining < least_remaining {
                least_remaining = tokens_remaining;
                furthest_err = err;
            }
        }
    }

    Err(furthest_err)
}

fn apply_term<'prog, 'bnf>(tokens: &'prog str, term: ebnf_ast::Term) -> ParseResult<'prog, 'bnf> {
    match term {
        ebnf_ast::Term::Optional(sub_term) => apply_optional(tokens, sub_term),
        ebnf_ast::Term::Repetition(sub_term) => apply_repeptition(tokens, sub_term),
        ebnf_ast::Term::Concatenation(sub_terms) => apply_concatenation(tokens, sub_terms),
        ebnf_ast::Term::Alternation(sub_terms) => apply_alternation(tokens, sub_terms),
        ebnf_ast::Term::Terminal(literal) => todo!(),
        ebnf_ast::Term::Identifier(rule) => todo!(),
    }
}

fn apply_alternation(tokens: &str, sub_terms: Vec<ebnf_ast::Term>) -> Result<OkResult, ErrResult> {
    let mut problems = Vec::new();

    for term in sub_terms {
        let result = apply_term(tokens, term);

        match result {
            Ok(_) => {
                // return the first successful
                return result;
            }
            Err(ErrResult::OutOfTokens)
            | Err(ErrResult::UnclosedGroup { .. })
            | Err(ErrResult::Error(_)) => {
                // if this is a syntax-error, return it.
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
        if let ErrResult::UnexpectedToken {
            tokens_remaining,
            while_parsing: _,
        } = err
        {
            if tokens_remaining < least_remaining {
                least_remaining = tokens_remaining;
                furthest_err = err;
            }
        }
    }

    Err(furthest_err)
}

fn apply_concatenation(
    tokens: &str,
    sub_terms: Vec<ebnf_ast::Term>,
) -> Result<OkResult, ErrResult> {
    let mut nodes = Vec::new();
    let mut remaining_tokens = tokens;

    for sub_term in sub_terms {
        let term_result = apply_term(remaining_tokens, sub_term);
        match term_result {
            Ok(result) => {
                match result.val {
                    ParseNode::Rule(rule_node) => nodes.push(rule_node),
                    ParseNode::Group(mut sub_nodes) => nodes.append(&mut sub_nodes),
                }
                remaining_tokens = result.remaining_tokens
            }
            Err(result) => return term_result,
        }
    }

    Ok(OkResult {
        val: ParseNode::Group(nodes),
        remaining_tokens: tokens,
    })
}

fn apply_repeptition(tokens: &str, sub_term: Box<ebnf_ast::Term>) -> Result<OkResult, ErrResult> {
    let mut nodes = Vec::new();
    let mut remaining_tokens = tokens;

    while let Ok(result) = apply_term(remaining_tokens, *sub_term) {
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

fn apply_optional(tokens: &str, sub_term: Box<ebnf_ast::Term>) -> Result<OkResult, ErrResult> {
    match apply_term(tokens, *sub_term) {
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
