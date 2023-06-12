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
        _ => format!("{:?}", error),
    }
}

#[derive(Debug)]
struct Interpretation<'prog, 'bnf> {
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
) -> Result<RuleNode<'prog, 'bnf>, Failure<'bnf>> {
    if grammar.rules.is_empty() {
        return Err(Failure::Error(SimpleError::new("No rules in grammar")));
    }

    let primary_rule = grammar.rules.get(0).expect("Grammar must have rules");

    let all_parse_results = grammar.apply_rule(program, primary_rule);

    let mut interpretations: Vec<Interpretation> = all_parse_results
        .into_iter()
        .filter_map(|result| result.ok())
        .collect();

    if interpretations.is_empty() {
        todo!();
    }

    if interpretations.len() > 1 {
        todo!();
    }

    let result = interpretations.pop().unwrap();

    if !result.remaining_tokens.is_empty() {
        return Err(Failure::UnclosedGroup {
            tokens_remaining: result.remaining_tokens.len(),
        });
    }

    if let ParseNode::Rule(rule_node) = result.val {
        return Ok(rule_node);
    } else {
        return Err(Failure::InternalError(SimpleError::new(
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
        let result_of_term = self.apply_term(tokens, &rule.pattern);

        let is_transparent = rule.identifier.as_bytes()[0] == b'_';
        if is_transparent {
            return result_of_term;
        }

        result_of_term
            .into_iter()
            .map(|rule_result| {
                if let Ok(interpretation) = rule_result {
                    let num_tokens = tokens.len() - interpretation.remaining_tokens.len();
                    if num_tokens == 0 {
                        Err(Failure::EmptyEvaluation)
                    } else {
                        let sub_rules = match interpretation.val {
                            ParseNode::Rule(node) => vec![node],
                            ParseNode::Group(sub_rules) => sub_rules,
                            ParseNode::Terminal(_) => Vec::new(),
                        };

                        Ok(Interpretation {
                            val: ParseNode::Rule(RuleNode {
                                rule: &rule.identifier,
                                tokens: &tokens[..num_tokens],
                                sub_rules,
                            }),
                            remaining_tokens: interpretation.remaining_tokens,
                        })
                    }
                } else {
                    rule_result
                }
            })
            .collect()
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
        sub_terms
            .iter()
            .flat_map(|term| self.apply_term(tokens, term))
            .collect()
    }

    fn apply_concatenation<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let our_term = sub_terms.first();

        match our_term {
            Some(term) => {
                let all_results = self.apply_term(tokens, term);

                let (all_ok, all_err) = all_results
                    .into_iter()
                    .partition::<ParseResult, _>(|result| result.is_ok());

                if all_ok.is_empty() {
                    all_err
                } else {
                    all_ok
                        .into_iter()
                        .map(|rule| rule.unwrap())
                        .flat_map(|rule| {
                            self.apply_concatenation(rule.remaining_tokens, &sub_terms[1..])
                        })
                        .collect()
                }
            }
            // if we ran out of terms, then we return a empty (but positive) result
            None => Vec::new(),
        }
    }

    fn apply_repetition<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        self.apply_term(tokens, sub_term)
            .into_iter()
            .flat_map(|maybe_result| {
                maybe_result.map(|result| self.apply_repetition(result.remaining_tokens, sub_term))
            })
            .flatten()
            .collect()
    }

    fn apply_optional<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
        self.apply_term(tokens, sub_term)
            .into_iter()
            .map(|result| match result {
                Err(Failure::UnexpectedToken { .. }) => Ok(Interpretation {
                    val: ParseNode::Group(vec![]),
                    remaining_tokens: tokens,
                }),
                _ => result,
            })
            .collect()
    }

    fn parse_regex<'prog>(
        &'bnf self,
        tokens: &'prog str,
        regex: &'bnf Regex,
    ) -> ParseResult<'prog, 'bnf> {
        vec![if let Some(found) = regex.find(tokens) {
            let slice = &tokens[..found.end()];
            Ok(Interpretation {
                val: ParseNode::Terminal(slice),
                remaining_tokens: &tokens[found.end()..],
            })
        } else {
            Err(Failure::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: regex.as_str(),
            })
        }]
    }

    fn parse_literal<'prog>(
        &'bnf self,
        tokens: &'prog str,
        literal: &'bnf str,
    ) -> ParseResult<'prog, 'bnf> {
        vec![if tokens.starts_with(literal) {
            Ok(Interpretation {
                val: ParseNode::Terminal(&tokens[..literal.len()]),
                remaining_tokens: &tokens[literal.len()..],
            })
        } else {
            Err(Failure::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: literal,
            })
        }]
    }

    fn parse_rule_identifier<'prog>(
        &'bnf self,
        tokens: &'prog str,
        rule_name: &'bnf str,
    ) -> ParseResult<'prog, 'bnf> {
        let rule = self.rules.iter().find(|rule| rule.identifier == rule_name);

        match rule {
            Some(rule) => self.apply_rule(tokens, rule),
            None => vec![Err(Failure::UnexpectedToken {
                tokens_remaining: tokens.len(),
                expected: rule_name,
            })],
        }
    }
}
