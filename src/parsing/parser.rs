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
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        tokens_remaining: usize,
        while_parsing: &'static str,
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
    program: String,
    grammar: ebnf_ast::EbnfAst,
) -> Result<RuleNode<'prog, 'bnf>, ErrResult> {
    if grammar.rules.is_empty() {
        return Err(ErrResult::Error(SimpleError::new("No rules in grammar")));
    }

    let primary_rule = grammar.rules.get(0).expect("Grammar must have rules");
    let result = apply_rule(&program, primary_rule)?;

    if let ParseNode::Rule(rule_node) = result.val {
        return Ok(rule_node);
    } else {
        return Err(ErrResult::InternalError(SimpleError::new(
            "Primary rule was no rule",
        )));
    }
}

// apply rule on tokens
fn apply_rule<'prog, 'bnf>(tokens: &'prog str, rule: &ebnf_ast::Rule) -> ParseResult<'prog, 'bnf> {
    let found_rule = apply_term(tokens, &rule.pattern)?;

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

fn apply_term<'prog, 'bnf>(tokens: &'prog str, term: &ebnf_ast::Term) -> ParseResult<'prog, 'bnf> {
    match term {
        ebnf_ast::Term::Optional(sub_term) => apply_optional(tokens, sub_term),
        ebnf_ast::Term::Repetition(sub_term) => apply_repeptition(tokens, sub_term),
        ebnf_ast::Term::Concatenation(sub_terms) => apply_concatenation(tokens, sub_terms),
        ebnf_ast::Term::Alternation(sub_terms) => apply_alternation(tokens, sub_terms),
        ebnf_ast::Term::Terminal(literal) => todo!(),
        ebnf_ast::Term::Identifier(rule) => todo!(),
    }
}

fn apply_alternation<'prog, 'bnf>(
    tokens: &'prog str,
    sub_terms: &'bnf [ebnf_ast::Term],
) -> ParseResult<'prog, 'bnf> {
    let mut problems = Vec::new();

    for term in sub_terms {
        let result = apply_term(tokens, term);

        match &result {
            Ok(ok_result) => {
                let num_bytes_parsed = tokens.len() - ok_result.remaining_tokens.len();
                // if the remaining_tokens is the same as the input tokens, that means that this term evaluated to nothing
                if num_bytes_parsed > 0 {
                    return result;
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
    let mut furthest_err = &ErrResult::OutOfTokens;

    for err in problems {
        match err {
            ErrResult::UnclosedGroup { tokens_remaining }
            | ErrResult::UnexpectedToken {
                tokens_remaining,
                while_parsing: _,
            } => {
                if *tokens_remaining < least_remaining {
                    least_remaining = *tokens_remaining;
                    furthest_err = err;
                }
            }
            ErrResult::OutOfTokens => {
                return Err(*err);
            }
            _ => (),
        }
    }

    Err(*furthest_err)
}

fn apply_concatenation<'prog, 'bnf>(
    tokens: &str,
    sub_terms: &'bnf [ebnf_ast::Term],
) -> ParseResult<'prog, 'bnf> {
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

fn apply_repeptition<'prog, 'bnf>(
    tokens: &str,
    sub_term: &ebnf_ast::Term,
) -> ParseResult<'prog, 'bnf> {
    let mut nodes = Vec::new();
    let mut remaining_tokens = tokens;

    while let Ok(result) = apply_term(remaining_tokens, sub_term) {
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
    tokens: &str,
    sub_term: &ebnf_ast::Term,
) -> ParseResult<'prog, 'bnf> {
    match apply_term(tokens, sub_term) {
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
