use std::cmp;

use regex::Regex;
use simple_error::SimpleError;

use super::ebnf_ast;

type ParseResult<'prog, 'bnf> = Vec<Result<Interpretation<'prog, 'bnf>, Failure<'bnf>>>;

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
pub enum Failure<'bnf> {
    // many near-equal failures
    // often multiple interpretations would almost be possible
    Multiple {
        errors: Vec<Failure<'bnf>>,
    },
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        tokens_remaining: usize,
        expected: &'bnf str,
    },
    // the end of the program was reached, but more was expected (equivalent to an UnexpectedToken at the end of the program)
    OutOfTokens {
        expected: &'bnf str,
    },
    // a group was opened, but never closed (equivalent to an early-evaluated OutOfTokens)
    UnclosedGroup {
        tokens_remaining: usize,
    },
    EmptyEvaluation,
    // the program or grammar has some unspecified syntax error
    Error(SimpleError),
    // the parser has a bug
    InternalError(SimpleError),
}

pub fn error_string(error: &Failure, source: &str) -> String {
    match error {
        Failure::UnexpectedToken {
            tokens_remaining, ..
        }
        | Failure::UnclosedGroup { tokens_remaining } => {
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
        Failure::Multiple { errors } => errors
            .into_iter()
            .map(|err| error_string(&err, source) + "\n")
            .collect::<String>(),
        _ => format!("{:?}", error),
    }
}

#[derive(PartialEq, Eq)]
struct Interpretation<'prog, 'bnf> {
    val: ParseNode<'prog, 'bnf>,
    remaining_tokens: &'prog str,
}

impl<'prog, 'bnf> std::fmt::Debug for Interpretation<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParseNode<'prog, 'bnf> {
    Rule(RuleNode<'prog, 'bnf>),
    EmptyNode,
    Pair(Box<ParseNode<'prog, 'bnf>>, Box<ParseNode<'prog, 'bnf>>),
    Terminal(&'prog str),
}

pub fn parse_program_with_grammar<'prog, 'bnf>(
    program: &'prog str,
    grammar: &'bnf ebnf_ast::EbnfAst,
) -> Result<RuleNode<'prog, 'bnf>, Vec<Failure<'bnf>>> {
    if grammar.rules.is_empty() {
        return Err(vec![Failure::Error(SimpleError::new(
            "No rules in grammar",
        ))]);
    }

    let primary_rule = grammar.rules.get(0).expect("Grammar must have rules");

    let (mut interpretations, failures) = grammar
        .apply_rule(program, primary_rule)
        .into_iter()
        .partition::<ParseResult, _>(|result| result.is_ok());

    if interpretations.is_empty() {
        return Err(failures
            .into_iter()
            .map(|reason| reason.unwrap_err())
            .collect());
    }

    if interpretations.len() > 1 {
        todo!();
    }

    let result = interpretations.pop().unwrap().unwrap();

    if !result.remaining_tokens.is_empty() {
        return Err(vec![Failure::UnclosedGroup {
            tokens_remaining: result.remaining_tokens.len(),
        }]);
    }

    if let ParseNode::Rule(rule_node) = result.val {
        return Ok(rule_node);
    } else {
        return Err(vec![Failure::InternalError(SimpleError::new(
            "Primary rule was no rule",
        ))]);
    }
}

impl<'bnf> ebnf_ast::EbnfAst {
    // apply rule on tokens
    fn apply_rule<'prog>(
        &'bnf self,
        tokens: &'prog str,
        rule: &'bnf ebnf_ast::Rule,
    ) -> ParseResult<'prog, 'bnf> {
        let is_transparent = rule.identifier.as_bytes()[0] == b'_';

        if !is_transparent {
            println!("<{}>", rule.identifier);
        }

        let result_of_term = self.apply_term(tokens, &rule.pattern);

        if is_transparent {
            return result_of_term;
        }

        let results = result_of_term
            .into_iter()
            .flat_map(|result| {
                result.map(|interpretation| {
                    let remaining_tokens = interpretation.remaining_tokens;
                    match tokens.len() - remaining_tokens.len() {
                        0 => Ok(Interpretation {
                            val: ParseNode::EmptyNode,
                            remaining_tokens: tokens,
                        }),
                        num_tokens => Ok(Interpretation {
                            val: ParseNode::Rule(RuleNode {
                                rule: &rule.identifier,
                                tokens: &tokens[..num_tokens],
                                sub_rules: unwrap_to_rulenodes(interpretation.val),
                            }),
                            remaining_tokens,
                        }),
                    }
                })
            })
            .collect();

        for val in &results {
            println!("<val>{:?}</val>", val);
        }
        println!("</{}>", rule.identifier);
        results
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
            ebnf_ast::Term::Literal(literal) => vec![self.parse_literal(tokens, literal)],
            ebnf_ast::Term::Identifier(rule_name) => self.parse_rule_identifier(tokens, rule_name),
            ebnf_ast::Term::Regex(ebnf_ast::RegexWrapper { regex, .. }) => {
                vec![self.parse_regex(tokens, regex)]
            }
        }
    }

    fn apply_alternation<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        sub_terms
            .into_iter()
            .flat_map(|term| self.apply_term(tokens, term))
            .collect()
    }

    fn apply_concatenation<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let term = sub_terms.first().unwrap();

        let all_results = self.apply_term(tokens, term);

        if sub_terms.len() >= 2 {
            all_results
                .into_iter()
                .flat_map(|term_result| match term_result {
                    Ok(term_interp) => self
                        .apply_concatenation(term_interp.remaining_tokens, &sub_terms[1..])
                        .into_iter()
                        .map(move |further_result| match further_result {
                            Ok(further_interp) => Ok(Interpretation {
                                val: ParseNode::Pair(
                                    Box::new(term_interp.val.clone()),
                                    Box::new(further_interp.val),
                                ),
                                remaining_tokens: further_interp.remaining_tokens,
                            }),
                            Err(_) => further_result,
                        })
                        .collect(),
                    Err(failure) => vec![Err(failure)],
                })
                .collect()
        } else {
            all_results
        }
    }

    fn apply_repetition<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        // evaluate the term once on `tokens`
        self.apply_term(tokens, sub_term)
            // for all evaluation attempts
            .into_iter()
            // we don't care about parse errors in a repetition (parse failure is an ok return)
            .filter_map(|term_result| term_result.ok())
            // remove all interpretations that consume no tokens
            .filter(|term_interp| term_interp.remaining_tokens != tokens)
            // for all successful interpretations, recursively evaluate further repetitions
            .flat_map(|term_interp| {
                self.apply_repetition(term_interp.remaining_tokens, sub_term)
                    // this includes the interpretation of 0 further repetitions (= term_interp)
                    .into_iter()
                    // we don't care about parse errors in a repetition (parse failure is an ok return)
                    .filter_map(|further_result| further_result.ok())
                    // remove all interpretations that consume no tokens
                    .filter(|further_interp| further_interp.remaining_tokens != tokens)
                    // each further interpretation creates a unique pair of 'this' and 'the further interpretation'
                    .map(move |further_interp| {
                        Ok(Interpretation {
                            val: ParseNode::Pair(
                                Box::new(term_interp.val.clone()),
                                Box::new(further_interp.val),
                            ),
                            remaining_tokens: further_interp.remaining_tokens,
                        })
                    })
            })
            // we also return the interpretation of 0 repetitions
            .chain(std::iter::once(Ok(Interpretation {
                val: ParseNode::EmptyNode,
                remaining_tokens: tokens,
            })))
            .collect()
    }

    fn apply_optional<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        // the result is all successful evaluations of the term, and the result of not applying the term.
        let mut results = self.apply_term(tokens, sub_term);
        results.push(Ok(Interpretation {
            val: ParseNode::EmptyNode,
            remaining_tokens: tokens,
        }));
        results
    }

    fn parse_regex<'prog>(
        &'bnf self,
        tokens: &'prog str,
        regex: &'bnf Regex,
    ) -> Result<Interpretation<'prog, 'bnf>, Failure<'bnf>> {
        if let Some(found) = regex.find(tokens) {
            let slice = &tokens[..found.end()];
            if slice.is_empty() {
                Err(Failure::EmptyEvaluation)
            } else {
                Ok(Interpretation {
                    val: ParseNode::Terminal(slice),
                    remaining_tokens: &tokens[found.end()..],
                })
            }
        } else {
            Err(Failure::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: regex.as_str(),
            })
        }
    }

    fn parse_literal<'prog>(
        &'bnf self,
        tokens: &'prog str,
        literal: &'bnf str,
    ) -> Result<Interpretation<'prog, 'bnf>, Failure<'bnf>> {
        if tokens.starts_with(literal) {
            Ok(Interpretation {
                val: ParseNode::Terminal(&tokens[..literal.len()]),
                remaining_tokens: &tokens[literal.len()..],
            })
        } else {
            Err(Failure::UnexpectedToken {
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
        self.rules
            .iter() // note: we iterate over the rules of the bnf
            .find(|rule| rule.identifier == rule_name)
            .map(|rule| self.apply_rule(tokens, rule))
            .unwrap_or_else(|| {
                vec![Err(Failure::InternalError(SimpleError::new(format!(
                    "Unknown rule {rule_name}"
                ))))]
            })
    }
}

fn unwrap_to_rulenodes<'prog, 'bnf>(node: ParseNode<'prog, 'bnf>) -> Vec<RuleNode<'prog, 'bnf>> {
    match node {
        ParseNode::Rule(rule) => vec![rule],
        ParseNode::Pair(sub_rule_1, sub_rule_2) => {
            let mut first_bit = unwrap_to_rulenodes(*sub_rule_1);
            let mut second_bit = unwrap_to_rulenodes(*sub_rule_2);
            first_bit.append(&mut second_bit);
            first_bit
        }
        ParseNode::Terminal(_) | ParseNode::EmptyNode => Vec::new(),
    }
}
