use std::{
    cmp,
    collections::{HashSet, VecDeque},
    io::Write,
};

use regex::Regex;
use simple_error::SimpleError;

use super::{ebnf_ast, rule_nodes::RuleNode};

type ParseResult<'prog, 'bnf> = Vec<Result<Interpretation<'prog, 'bnf>, Failure<'bnf>>>;

const MAX_NUM_TOKENS_BACKTRACE_ON_ERROR: i32 = 5;
const MAX_NUM_TOKENS_BACKTRACE_ON_SUCCESS: usize = 100;
const MAX_ERRORS_PER_RULE: usize = 50;

#[derive(Debug, Clone)]
pub enum Failure<'bnf> {
    // the evaluation of this function could not complete for these tokens
    // this is a normal negative return value
    UnexpectedToken {
        tokens_remaining: usize,
        expected: &'bnf str,
    },
    // the parser finished before the end of the file
    IncompleteParse {
        tokens_remaining: usize,
    },
    EmptyEvaluation,
    // the program or grammar has some unspecified syntax error
    Error(SimpleError),
    // the parser has 'crashed' due to a compiler bug
    InternalError(SimpleError),
}

impl Failure<'_> {
    pub fn error_string(&self, source: &str) -> String {
        match self {
            Failure::UnexpectedToken {
                tokens_remaining, ..
            }
            | Failure::IncompleteParse { tokens_remaining } => {
                let offset = source.len() - tokens_remaining;
                let line_number = source[..offset].bytes().filter(|c| c == &b'\n').count() + 1;
                let lower_newline = source[..offset].rfind("\n").map(|v| v + 1).unwrap_or(0);
                let upper_newline = source[offset..]
                    .find("\n")
                    .map(|v| v + offset)
                    .unwrap_or(source.len());
                let lb = cmp::max(offset as i64 - 40, lower_newline as i64) as usize;
                let ub = cmp::min(offset as i64 + 40, upper_newline as i64) as usize;
                format!(
                    "{:?} on line {line_number}:\n\n{:>40}{:<40}\n{:>40}^ when parsing here",
                    self,
                    &source[lb..offset],
                    &source[offset..ub],
                    ""
                )
            }
            _ => format!("{:?}", self),
        }
    }
}

struct Interpretation<'prog, 'bnf> {
    val: ParseNode<'prog, 'bnf>,
    remaining_tokens: &'prog str,
}

impl<'prog, 'bnf> std::fmt::Debug for Interpretation<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

#[derive(Debug, Clone)]
enum ParseNode<'prog, 'bnf> {
    Rule(RuleNode<'prog, 'bnf>),
    EmptyNode,
    Pair(Box<ParseNode<'prog, 'bnf>>, Box<ParseNode<'prog, 'bnf>>),
    Terminal(&'prog str),
}

pub struct Parser {
    grammar: ebnf_ast::EbnfAst,
    xml_out: Option<std::fs::File>,
}

impl<'bnf> Parser {
    pub fn new(
        grammar: ebnf_ast::EbnfAst,
        xml_out: Option<std::fs::File>,
    ) -> Result<Parser, SimpleError> {
        if grammar.rules.is_empty() {
            Err(SimpleError::new("No rules in grammar"))
        } else {
            Ok(Parser { grammar, xml_out })
        }
    }

    fn log(&'bnf self, string: String) {
        if let Some(mut file) = self.xml_out.as_ref() {
            let _ = write!(file, "{string}");
        }
    }

    pub fn parse_program<'prog>(
        &'bnf self,
        program: &'prog str,
    ) -> Result<RuleNode<'prog, 'bnf>, Vec<Failure<'bnf>>> {
        let primary_rule = self.grammar.rules.get(0).expect("Grammar must have rules");

        let mut interpretations = self.apply_rule(program, primary_rule);

        // rules always sort their results (but lets check this)
        for ele in interpretations.windows(2) {
            if compare_result(&ele[0], &ele[1]) == cmp::Ordering::Greater {
                panic!("{:?} was before {:?}", ele[0], ele[1]);
            }
        }

        let partition_point = interpretations.partition_point(|r| r.is_ok());

        if interpretations[..partition_point].is_empty() {
            return Err(interpretations
                .into_iter()
                .map(|reason| reason.unwrap_err())
                .collect());
        }

        let result_borrowed = interpretations.first().unwrap().as_ref().unwrap();

        if !result_borrowed.remaining_tokens.is_empty() {
            let mut furthest_failures = vec![Failure::IncompleteParse {
                tokens_remaining: result_borrowed.remaining_tokens.len(),
            }];

            let mut iter = interpretations.into_iter().rev();

            while let Some(Err(failure)) = iter.next() {
                let minimum = get_err_significance(furthest_failures.last().unwrap())
                    - MAX_NUM_TOKENS_BACKTRACE_ON_ERROR;
                if get_err_significance(&failure) > minimum {
                    furthest_failures.push(failure);
                    furthest_failures.sort_by(compare_err_result);
                }
            }

            return Err(furthest_failures);
        }

        let result_removed = interpretations.swap_remove(0).unwrap();

        if let ParseNode::Rule(rule_node) = result_removed.val {
            return Ok(rule_node);
        } else {
            return Err(vec![Failure::InternalError(SimpleError::new(
                "Primary rule was no rule",
            ))]);
        }
    }

    // apply rule on tokens
    fn apply_rule<'prog>(
        &'bnf self,
        tokens: &'prog str,
        rule: &'bnf ebnf_ast::Rule,
    ) -> ParseResult<'prog, 'bnf> {
        let is_transparent = rule.identifier.as_bytes()[0] == b'_';

        let result_of_term = self.apply_term(tokens, &rule.pattern);

        if is_transparent {
            return result_of_term;
        }

        let mut interpretations = HashSet::new();
        let mut failures = VecDeque::new();
        let mut any_empty_result = false;

        for result in result_of_term {
            match result {
                Ok(interpretation) => {
                    let remaining_tokens = interpretation.remaining_tokens;
                    match tokens.len() - remaining_tokens.len() {
                        0 => any_empty_result = true,
                        num_tokens => {
                            interpretations.insert(RuleNode {
                                rule_name: &rule.identifier,
                                tokens: &tokens[..num_tokens],
                                sub_rules: unwrap_to_rulenodes(interpretation.val),
                            });
                        }
                    }
                }
                Err(_) => {
                    let search_result =
                        failures.binary_search_by(|other| compare_result(other, &result));
                    let index = match search_result {
                        Ok(v) => v,
                        Err(v) => v,
                    };
                    if failures.len() < MAX_ERRORS_PER_RULE {
                        failures.insert(index, result);
                    } else if index > 0 {
                        failures.insert(index, result);
                        failures.pop_front();
                    }
                }
            }
        }

        let mut results = Vec::new();

        for node in interpretations {
            let num_tokens = node.tokens.len();
            results.push(Ok(Interpretation {
                val: ParseNode::Rule(node),
                remaining_tokens: &tokens[num_tokens..],
            }))
        }

        if any_empty_result {
            results.push(Ok(Interpretation {
                val: ParseNode::EmptyNode,
                remaining_tokens: tokens,
            }))
        }

        failures.into_iter().for_each(|fail| results.push(fail));

        // make sure the longest solutions are first
        results.sort_unstable_by(compare_result);

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
        if sub_terms.len() > 2 && tokens.len() > MAX_NUM_TOKENS_BACKTRACE_ON_SUCCESS { 
            return self.apply_alternation_with_sortcut(tokens, sub_terms); 
        }

        let mut combined_results = Vec::new();
        for term in sub_terms {
            combined_results.append(&mut self.apply_term(tokens, term));
        }
        combined_results
    }

    fn apply_alternation_with_sortcut<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let mut combined_results = Vec::new();
        
        for term in sub_terms {
            let mut results = self.apply_term(tokens, term);

            let least_remaining = results
                .iter()
                .filter_map(|r| r.as_ref().ok())
                .map(|i| i.remaining_tokens.len())
                .min()
                .unwrap_or(tokens.len());

            combined_results.append(&mut results);

            if least_remaining + MAX_NUM_TOKENS_BACKTRACE_ON_SUCCESS < tokens.len() {
                return combined_results;
            }
        }
        combined_results
    }

    fn apply_concatenation<'prog>(
        &'bnf self,
        tokens: &'prog str,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        let term = sub_terms.first().unwrap();

        let all_results = self.apply_term(tokens, term);

        if sub_terms.len() > 1 {
            all_results
                .into_iter()
                .flat_map(|term_result| match term_result {
                    Ok(term_interp) => self.continue_concatenation(term_interp, sub_terms),
                    Err(failure) => vec![Err(failure)],
                })
                .collect()
        } else {
            all_results
        }
    }

    fn continue_concatenation<'prog>(
        &'bnf self,
        prev_interp: Interpretation<'prog, 'bnf>,
        sub_terms: &'bnf [ebnf_ast::Term],
    ) -> ParseResult<'prog, 'bnf> {
        self.apply_concatenation(prev_interp.remaining_tokens, &sub_terms[1..])
            .into_iter()
            .map(move |further_result| match further_result {
                Ok(further_interp) => Ok(Interpretation {
                    val: ParseNode::Pair(
                        Box::new(prev_interp.val.clone()),
                        Box::new(further_interp.val),
                    ),
                    remaining_tokens: further_interp.remaining_tokens,
                }),
                Err(_) => further_result,
            })
            .collect()
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
            .map(|term_result| {
                if let Ok(term_interp) = term_result {
                    if term_interp.remaining_tokens.len() != tokens.len() {
                        self.continue_repetition(term_interp, sub_term)
                    } else {
                        vec![Err(Failure::EmptyEvaluation)]
                    }
                } else {
                    vec![term_result]
                }
            })
            .flatten()
            // we also return the interpretation of 0 repetitions
            .chain(std::iter::once(Ok(Interpretation {
                val: ParseNode::EmptyNode,
                remaining_tokens: tokens,
            })))
            .collect()
    }

    fn continue_repetition<'prog>(
        &'bnf self,
        prev_interp: Interpretation<'prog, 'bnf>,
        sub_term: &'bnf ebnf_ast::Term,
    ) -> ParseResult<'prog, 'bnf> {
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
            })
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
        self.grammar
            .rules
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

// ok < err, see compare_ok_result and compare_err_result
fn compare_result(
    a: &Result<Interpretation<'_, '_>, Failure<'_>>,
    b: &Result<Interpretation<'_, '_>, Failure<'_>>,
) -> cmp::Ordering {
    match (a.is_ok(), b.is_ok()) {
        (true, false) => cmp::Ordering::Less,
        (false, true) => cmp::Ordering::Greater,
        (true, true) => compare_ok_result(a.as_ref().unwrap(), b.as_ref().unwrap()),
        (false, false) => compare_err_result(a.as_ref().unwrap_err(), b.as_ref().unwrap_err()),
    }
}

// return whether a has more tokens than b
fn compare_ok_result(a: &Interpretation<'_, '_>, b: &Interpretation<'_, '_>) -> cmp::Ordering {
    let a_len = a.remaining_tokens.len();
    let b_len = b.remaining_tokens.len();
    // smallest number of remaining tokens first
    a_len.cmp(&b_len)
}

// return whether a is more or less significant than b
fn compare_err_result(a: &Failure<'_>, b: &Failure<'_>) -> cmp::Ordering {
    get_err_significance(a).cmp(&get_err_significance(b))
}

fn get_err_significance(a: &Failure<'_>) -> i32 {
    match a {
        Failure::UnexpectedToken {
            tokens_remaining, ..
        }
        | Failure::IncompleteParse { tokens_remaining } => -(*tokens_remaining as i32),
        Failure::EmptyEvaluation => 1,
        Failure::Error(_) => 2,
        Failure::InternalError(_) => 3,
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

fn sanitize(original: String) -> String {
    original
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace("\r\n", "\\n")
        .replace("\r", "\\r")
        .replace("\n", "\\n")
}
