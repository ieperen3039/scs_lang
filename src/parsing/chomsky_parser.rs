use std::{collections::VecDeque, io::Write};

use simple_error::SimpleError;

use crate::transforming::chomsker::Chomsky;

use super::{token::{Token, TokenClass}, parser::*, rule_nodes::RuleNode};

pub struct Parser {
    grammar: Chomsky,
    xml_out: Option<std::fs::File>,
}

struct ParseTable {

}

impl ParseTable {
    pub fn get()
}

impl<'bnf> Parser {
    pub fn new(
        grammar: Chomsky,
        xml_out: Option<std::fs::File>,
    ) -> Parser {
        Parser { grammar, xml_out }
    }

    fn log(&'bnf self, string: &str) {
        if let Some(mut file) = self.xml_out.as_ref() {
            let _ = write!(file, "{string}");
        }
    }

    pub fn parse_program<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
    ) -> Result<RuleNode<'prog, 'bnf>, Vec<Failure<'bnf>>> {
        let primary_rule = self.grammar.rules.get(&self.grammar.start);

        let parse_table = construct_parse_table(&self.grammar);

        let interpretations = self.apply_rule(&tokens, primary_rule);

        let mut longest_success = Interpretation {
            val: ParseNode::EmptyNode,
            remaining_tokens: tokens,
        };
        let mut failures = VecDeque::new();

        for result in interpretations {
            match result {
                Err(failure) => {
                    let search_result =
                        failures.binary_search_by(|other| compare_err_result(other, &failure));
                    let index = match search_result {
                        Ok(v) => v,
                        Err(v) => v,
                    };
                    if failures.len() < MAX_ERRORS_PER_RULE {
                        failures.insert(index, failure);
                    } else if index > 0 {
                        failures.insert(index, failure);
                        failures.pop_front();
                    }
                }
                Ok(interpretation) => {
                    let num_tokens_remaining = interpretation.remaining_tokens.len();
                    if num_tokens_remaining == 0 {
                        // shortcut
                        longest_success = interpretation;
                        break;
                    }

                    if num_tokens_remaining < longest_success.remaining_tokens.len() {
                        longest_success = interpretation;
                    }
                }
            }
        }

        if let ParseNode::EmptyNode = longest_success.val {
            return Err(failures.into());
        }

        if !longest_success.remaining_tokens.is_empty() {
            let mut furthest_failures = vec![Failure::IncompleteParse {
                char_idx: longest_success
                    .remaining_tokens
                    .first()
                    .map(|t| t.char_idx - 1)
                    .ok_or_else(|| {
                        vec![Failure::InternalError(SimpleError::new(
                            "incomplete parse, but no remaining tokens",
                        ))]
                    })?,
            }];

            for failure in failures {
                let minimum = get_err_significance(furthest_failures.last().unwrap())
                    - MAX_NUM_TOKENS_BACKTRACE_ON_ERROR;
                if get_err_significance(&failure) > minimum {
                    furthest_failures.push(failure);
                    furthest_failures.sort_by(compare_err_result);
                }
            }

            return Err(furthest_failures);
        }

        if let ParseNode::Rule(rule_node) = longest_success.val {
            return Ok(rule_node);
        } else {
            return Err(vec![Failure::InternalError(SimpleError::new(
                "Primary rule was no rule",
            ))]);
        }
    }
}

fn construct_parse_table(grammar: &Chomsky) -> _ {
    todo!()
}