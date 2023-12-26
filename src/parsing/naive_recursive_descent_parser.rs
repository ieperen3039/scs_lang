use std::{collections::VecDeque, io::Write};

use simple_error::SimpleError;

use super::{ebnf_ast, token::{Token, TokenClass}, parser::*, rule_nodes::RuleNode};



pub struct Parser {
    grammar: ebnf_ast::EbnfAst,
    xml_out: Option<std::fs::File>,
}

impl<'bnf> Parser {
    pub fn new(
        grammar: ebnf_ast::EbnfAst,
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
        let primary_rule = self.grammar.rules.get(0).expect("Grammar must have rules");

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

    // apply rule on tokens
    fn apply_rule<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        rule: &'bnf ebnf_ast::Rule,
    ) -> ParseResult<'prog, 'bnf> {
        let is_transparent = rule.identifier.as_bytes()[0] == b'_';

        let result_of_term = self.apply_term(tokens, &rule.pattern);

        if is_transparent {
            return result_of_term;
        }

        // not transparent: wrap every interpretation into a rulenode
        Box::new(result_of_term.map(move |result| match result {
            Ok(interpretation) => {
                let remaining_tokens = interpretation.remaining_tokens;

                match tokens.len() - remaining_tokens.len() {
                    0 => Ok(Interpretation {
                        val: ParseNode::EmptyNode,
                        remaining_tokens: tokens,
                    }),
                    num_tokens => Ok(Interpretation {
                        val: ParseNode::Rule(RuleNode {
                            rule_name: &rule.identifier,
                            tokens: &tokens[..num_tokens],
                            sub_rules: unwrap_to_rulenodes(interpretation.val),
                        }),
                        remaining_tokens: &tokens[num_tokens..],
                    }),
                }
            }
            Err(err) => Err(err),
        }))
    }

    fn apply_term<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        match term {
            ebnf_ast::Term::Optional(sub_term) => self.apply_optional(tokens, sub_term),
            ebnf_ast::Term::Repetition(sub_term) => self.apply_repetition(tokens, sub_term),
            ebnf_ast::Term::Concatenation(sub_terms) => self.apply_concatenation(tokens, sub_terms),
            ebnf_ast::Term::Alternation(sub_terms) => self.apply_alternation(tokens, sub_terms),
            ebnf_ast::Term::Literal(literal) => {
                Box::new(std::iter::once(self.parse_literal(tokens, literal)))
            }
            ebnf_ast::Term::Identifier(rule_name) => self.parse_rule_identifier(tokens, rule_name),
            ebnf_ast::Term::Token(token) => {
                Box::new(std::iter::once(self.parse_token(tokens, token)))
            }
        }
    }

    fn apply_alternation<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        Box::new(
            sub_terms
                .into_iter()
                .flat_map(|term| self.apply_term(tokens, term)),
        )
    }

    fn apply_concatenation<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let term = sub_terms.first().unwrap();

        let all_results = self.apply_term(tokens, term);

        if sub_terms.len() > 1 {
            Box::new(all_results.flat_map(|term_result| match term_result {
                Ok(term_interp) => self.continue_concatenation(term_interp, sub_terms),
                Err(failure) => Box::new(std::iter::once(Err(failure))),
            }))
        } else {
            all_results
        }
    }

    fn continue_concatenation<'prog: 'bnf>(
        &'bnf self,
        prev_interp: Interpretation<'prog, 'bnf>,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        Box::new(
            self.apply_concatenation(prev_interp.remaining_tokens, &sub_terms[1..])
                .map(move |further_result| match further_result {
                    Ok(further_interp) => Ok(Interpretation {
                        val: ParseNode::Pair(
                            Box::new(prev_interp.val.clone()),
                            Box::new(further_interp.val),
                        ),
                        remaining_tokens: further_interp.remaining_tokens,
                    }),
                    Err(err) => Err(err),
                }),
        )
    }

    fn apply_repetition<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        // evaluate the term once on `tokens`
        Box::new(
            self.apply_term(tokens, sub_term)
                // for all evaluation attempts
                .flat_map(|term_result| {
                    if let Ok(term_interp) = term_result {
                        if term_interp.remaining_tokens.len() != tokens.len() {
                            self.continue_repetition(term_interp, sub_term)
                        } else {
                            Box::new(std::iter::empty())
                        }
                    } else {
                        Box::new(std::iter::once(term_result))
                    }
                })
                // we also return the interpretation of 0 repetitions
                .chain(std::iter::once(Ok(Interpretation {
                    val: ParseNode::EmptyNode,
                    remaining_tokens: tokens,
                }))),
        )
    }

    fn continue_repetition<'prog: 'bnf>(
        &'bnf self,
        prev_interp: Interpretation<'prog, 'bnf>,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        Box::new(
            self.apply_repetition(prev_interp.remaining_tokens, sub_term)
                // this includes the interpretation of 0 further repetitions (= prev_interp)
                .into_iter()
                // each further interpretation creates a unique pair of 'this' and 'the further interpretation'
                .map(move |further_result| {
                    if let Ok(further_interp) = further_result {
                        Ok(Interpretation {
                            val: ParseNode::Pair(
                                Box::new(prev_interp.val.clone()),
                                Box::new(further_interp.val),
                            ),
                            remaining_tokens: further_interp.remaining_tokens,
                        })
                    } else {
                        further_result
                    }
                }),
        )
    }

    fn apply_optional<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        // the result is all successful evaluations of the term, and the result of not applying the term.
        let results = self.apply_term(tokens, sub_term);
        Box::new(results.chain(std::iter::once(Ok(Interpretation {
            val: ParseNode::EmptyNode,
            remaining_tokens: tokens,
        }))))
    }

    fn parse_literal<'prog>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        literal: &'bnf str,
    ) -> Result<Interpretation<'prog, 'bnf>, Failure<'bnf>> {
        let first_token = tokens
            .first()
            .ok_or(Failure::EndOfFile { expected: literal })?;

        if first_token.slice.starts_with(literal) {
            Ok(Interpretation {
                val: ParseNode::Terminal(first_token),
                remaining_tokens: &tokens[1..],
            })
        } else {
            Err(Failure::UnexpectedToken {
                char_idx: first_token.char_idx,
                expected: literal,
            })
        }
    }

    fn parse_token<'prog>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        expected: &TokenClass,
    ) -> Result<Interpretation<'prog, 'bnf>, Failure<'bnf>> {
        let first_token = tokens.first().ok_or(Failure::EndOfFile {
            expected: expected.str(),
        })?;

        if first_token.class == *expected {
            Ok(Interpretation {
                val: ParseNode::Terminal(first_token),
                remaining_tokens: &tokens[1..],
            })
        } else {
            Err(Failure::UnexpectedToken {
                char_idx: first_token.char_idx,
                expected: expected.str(),
            })
        }
    }

    fn parse_rule_identifier<'prog: 'bnf>(
        &'bnf self,
        tokens: &'prog [Token<'prog>],
        rule_name: &'bnf str,
    ) -> ParseResult<'prog, 'bnf> {
        self.grammar
            .rules
            .iter() // note: we iterate over the rules of the bnf
            .find(|rule| rule.identifier == rule_name)
            .map(|rule| self.apply_rule(tokens, rule))
            .unwrap_or_else(|| {
                Box::new(std::iter::once(Err(Failure::InternalError(
                    SimpleError::new(format!("Unknown rule {rule_name}")),
                ))))
            })
    }
}