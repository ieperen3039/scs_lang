use std::{
    collections::{hash_map::Keys, HashMap, HashSet},
    io::Write,
};

use crate::transforming::grammar::{Grammar, RuleId, Terminal, Term};

use super::{
    parser::*,
    rule_nodes::RuleNode,
    token::{Token, TokenClass},
};

pub struct Parser {
    start_rule: RuleId,
    parse_table: ParseTable,
    xml_out: Option<std::fs::File>,
}

struct ParseTable {
    rules : Vec<Term>,
    literals: HashMap<RuleId, HashMap<String, Vec<usize>>>,
    tokens: HashMap<RuleId, HashMap<TokenClass, Vec<usize>>>,
}

impl<'c> ParseTable {
    pub fn get(&'c self, parse_stack: &str, look_ahead: &Token) -> Vec<&'c Term> {
        let indices_from_literals: Vec<usize> = self
            .literals
            .get(parse_stack)
            .iter()
            .flat_map(|m| m.get(look_ahead.slice))
            .flat_map(|&i| i)
            .collect();

        let indices_from_tokens: Vec<usize> = self
            .tokens
            .get(parse_stack)
            .iter()
            .flat_map(|m| m.get(&look_ahead.class))
            .flat_map(|&i| i)
            .collect();

        indices_from_literals.into_iter().chain(indices_from_tokens).map(|idx| &self.rules[idx]).collect()
    }

    pub fn get_expected(&'c self, parse_stack: &str) -> Vec<&'c str> {
        let expected_literals: Vec<_> = self
            .literals
            .get(parse_stack)
            .iter()
            .map(|&m| m)
            .flat_map(HashMap::keys)
            .map(String::as_str)
            .collect();

        let expected_tokens: Vec<_> = self
            .tokens
            .get(parse_stack)
            .iter()
            .map(|&m| m)
            .flat_map(HashMap::keys)
            .map(TokenClass::str)
            .collect();

        expected_tokens
            .into_iter()
            .chain(expected_literals)
            .collect()
    }
}

impl<'c> Parser {
    pub fn new(grammar: Grammar, xml_out: Option<std::fs::File>) -> Parser {
        let start_rule = grammar.start_rule.clone();
        let parse_table = construct_parse_table(grammar);
        Parser {
            start_rule,
            parse_table,
            xml_out,
        }
    }

    pub fn parse_program<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        let interpretation = self.apply_rule_with_log(0, tokens, &self.start_rule);

        match interpretation {
            Err(failures) => Err(failures),
            Ok(parsed_program) => {
                if !parsed_program.tokens.len() == tokens.len() {
                    Err(vec![Failure::IncompleteParse {
                        char_idx: parsed_program.tokens.len(),
                    }])
                } else {
                    Ok(parsed_program)
                }
            }
        }
    }

    fn apply_rule_with_log<'prog: 'c>(
        &'c self,
        token_index: usize,
        tokens: &'prog [Token<'prog>],
        rule_name: &'c str,
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        self.log_enter(rule_name);
        let result = self.apply_rule(token_index, tokens, rule_name);
        self.log_exit(rule_name);

        return result;
    }

    fn apply_rule<'prog: 'c>(
        &'c self,
        token_index: usize,
        tokens: &'prog [Token<'prog>],
        rule_name: &'c str,
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        if token_index == tokens.len() {
            return Err(vec![Failure::EndOfFile {
                expected: rule_name,
            }]);
        }

        let next_token = &tokens[token_index];
        let mut all_failures = Vec::new();

        let possible_patterns = self.parse_table.get(rule_name, next_token);

        if possible_patterns.is_empty() {
            return Err(self
                .parse_table
                .get_expected(rule_name)
                .into_iter()
                .map(|expected| Failure::UnexpectedToken {
                    char_idx: token_index,
                    expected,
                })
                .collect());
        }

        let mut pattern_nr = 1;
        let max = possible_patterns.len();

        // this for-loop is for the case where the given grammar is not an LL(1) grammar
        for pattern in possible_patterns {
            if max > 1 {
                if let Some(mut file) = self.xml_out.as_ref() {
                    let _ = writeln!(
                        file,
                        "{:20} {:>2}/{:<2}: {:?}",
                        rule_name, pattern_nr, max, pattern
                    );
                }
            }

            pattern_nr += 1;

            match pattern {
                Term::Terminal(terminal) => {
                    let has_match = match terminal {
                        Terminal::Literal(str) => str == next_token.slice,
                        Terminal::Token(class) => *class == next_token.class,
                    };

                    if has_match {
                        self.log_consume_token(&tokens[token_index]);

                        return Ok(RuleNode {
                            rule_name,
                            tokens: &tokens[token_index..=token_index],
                            sub_rules: Vec::new(),
                        });
                    }
                }
                Term::Concatenation(sub_rule_names) => {
                    let result = self.apply_non_terminal(rule_name, token_index, tokens, sub_rule_names);
                    match result {
                        Ok(n) => return Ok(n),
                        Err(failures) => all_failures.extend(failures),
                    }
                }
                Term::Alternation(_) => todo!(),
                Term::Identifier(_) => todo!(),
                Term::Empty => todo!(),
            };
        }

        return Err(all_failures);
    }

    fn apply_non_terminal<'prog: 'c>(
        &'c self,
        current_rule_name: &'c str,
        token_index: usize,
        tokens: &'prog [Token<'prog>],
        sub_rule_names: &'c [RuleId],
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        let mut sub_rules = Vec::new();
        let mut sub_rule_token_index = token_index;

        for new_rule_name in sub_rule_names {
            let applied_rule =
                self.apply_rule_with_log(sub_rule_token_index, tokens, new_rule_name);
            match applied_rule {
                Ok(rule) => {
                    sub_rule_token_index += rule.tokens.len();
                    if is_transparent(rule.rule_name) {
                        sub_rules.extend(rule.sub_rules);
                    } else {
                        sub_rules.push(rule);
                    }
                }
                Err(failures) => return Err(failures),
            }
        }

        Ok(RuleNode {
            rule_name: current_rule_name,
            tokens: &tokens[token_index..sub_rule_token_index],
            sub_rules,
        })
    }

    fn log_consume_token(&'c self, token: &Token<'_>) {
        if let Some(mut file) = self.xml_out.as_ref() {
            if token.class == TokenClass::WHITESPACE {
                return;
            }
            if !token.slice.contains(|c: char| !c.is_whitespace()) {
                return;
            }

            let new_slice = token.slice.replace(char::is_whitespace, " ");
            let _ = writeln!(file, "\"{}\"", &new_slice);
        }
    }

    fn log_enter(&'c self, rule_name: &str) {
        if let Some(mut file) = self.xml_out.as_ref() {
            if !is_transparent(rule_name) {
                let _ = writeln!(file, "<{rule_name}>");
            }
        }
    }

    fn log_exit(&'c self, rule_name: &str) {
        if let Some(mut file) = self.xml_out.as_ref() {
            if !is_transparent(rule_name) {
                let _ = writeln!(file, "</{rule_name}>");
            }
        }
    }
}

fn is_transparent(rule_name: &str) -> bool {
    rule_name.as_bytes()[0] == b'_'
}

// T[A,a] contains the rule "A => w" if and only if `w` may start with an `a`
fn construct_parse_table(grammar: Grammar) -> ParseTable {
    let mut literals: HashMap<RuleId, HashMap<String, Vec<usize>>> = HashMap::new();
    let mut tokens: HashMap<RuleId, HashMap<TokenClass, Vec<usize>>> = HashMap::new();
    let mut rules: Vec<Term> = Vec::new();

    let mut idx = 0;
    for (rule_id, pattern) in grammar.rules {
        for rule in pattern {
            for terminal in rule.first_terminals {
                match terminal {
                    Terminal::Literal(str) => literals
                        .entry(rule_id.clone())
                        .or_insert(HashMap::new())
                        .entry(str)
                        .or_insert(Vec::new())
                        .push(idx),
                    Terminal::Token(class) => tokens
                        .entry(rule_id.clone())
                        .or_insert(HashMap::new())
                        .entry(class)
                        .or_insert(Vec::new())
                        .push(idx),
                };
            }
            idx += 1;
            rules.push(rule);
        }
    }

    // note that the original grammar is destroyed
    ParseTable { literals, tokens, rules }
}
