use std::{
    collections::{HashMap, VecDeque},
    io::Write,
    iter,
};

use simple_error::SimpleError;

use crate::transforming::{
    chomsker::{Chomsky, ChomskyPattern, ChomskyRule},
    grammar::{RuleId, Terminal},
};

use super::{
    parser::*,
    rule_nodes::RuleNode,
    token::{Token, TokenClass},
};

pub struct Parser<'c> {
    grammar: &'c Chomsky,
    parse_table: ParseTable<'c>,
    xml_out: Option<std::fs::File>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum LookAheadToken<'c> {
    Literal(&'c str),
    Token(TokenClass),
}

struct ParseTable<'c> {
    table: HashMap<RuleId, HashMap<LookAheadToken<'c>, Vec<&'c ChomskyPattern>>>,
}

impl<'c> ParseTable<'c> {
    pub fn get(&self, parse_stack: &str, look_ahead: &Token) -> Vec<&'c ChomskyPattern> {
        let maybe_row = self.table.get(parse_stack);

        if let Some(row) = maybe_row {
            let as_literal_iter = row
                .get(&LookAheadToken::Literal(look_ahead.slice))
                .into_iter()
                .flat_map(|v| v.iter())
                .map(|&p| p);

            let as_token_iter = row
                .get(&LookAheadToken::Token(look_ahead.class))
                .into_iter()
                .flat_map(|v| v.iter())
                .map(|&p| p);

            return as_literal_iter.chain(as_token_iter).collect();
        } else {
            return Vec::new();
        }
    }
}

impl<'c> Parser<'c> {
    pub fn new(grammar: &'c Chomsky, xml_out: Option<std::fs::File>) -> Parser<'c> {
        let parse_table = construct_parse_table(grammar);
        Parser {
            grammar,
            parse_table,
            xml_out,
        }
    }

    fn log(&'c self, string: &str) {
        if let Some(mut file) = self.xml_out.as_ref() {
            let _ = write!(file, "{string}");
        }
    }

    pub fn parse_program<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        let interpretation = self.apply_rule(tokens, &self.grammar.start);

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

    fn apply_rule<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        rule_name: &'c str,
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        if tokens.is_empty() {
            return Err(vec![Failure::EndOfFile{ expected: rule_name }]);
        }

        let next_token = &tokens[0];
        let mut all_failures = Vec::new();

        let possible_patterns = self.parse_table.get(rule_name, next_token);

        // this for-loop is for the case where the given grammar is not an LL(1) grammar
        for pattern in possible_patterns {
            match pattern {
                ChomskyPattern::Terminal(terminal) => {
                    let has_match = match terminal {
                        Terminal::Literal(str) => str == next_token.slice,
                        Terminal::Token(class) => *class == next_token.class,
                    };

                    if has_match {
                        return Ok(RuleNode {
                            rule_name,
                            tokens: &tokens[..1],
                            sub_rules: Vec::new(),
                        });
                    }
                }
                ChomskyPattern::NonTerminal(sub_rule_names) => {
                    let result = self.apply_non_terminal(rule_name, tokens, sub_rule_names);
                    match result {
                        Ok(n) => return Ok(n),
                        Err(failures) => all_failures.extend(failures),
                    }
                }
            };
        }

        return Err(all_failures);
    }

    fn apply_non_terminal<'prog: 'c>(
        &'c self,
        current_rule_name: &'c str,
        tokens: &'prog [Token<'prog>],
        sub_rule_names: &'c [RuleId],
    ) -> Result<RuleNode<'prog, 'c>, Vec<Failure<'c>>> {
        let mut sub_rules = Vec::new();
        let mut token_offset = 0;

        for new_rule_name in sub_rule_names {
            let applied_rule = self.apply_rule(&tokens[token_offset..], new_rule_name);
            match applied_rule {
                Ok(rule) => {
                    token_offset += rule.tokens.len();
                    let is_transparent = rule.rule_name.as_bytes()[0] == b'_';
                    if is_transparent {
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
            tokens: &tokens[..token_offset],
            sub_rules,
        })
    }
}

// T[A,a] contains the rule "A => w" if and only if `w` may start with an `a`
fn construct_parse_table<'c>(grammar: &'c Chomsky) -> ParseTable<'c> {
    let mut table: HashMap<RuleId, HashMap<LookAheadToken<'c>, Vec<&'c ChomskyPattern>>> =
        HashMap::new();

    for (rule_id, rules) in &grammar.rules {
        for rule in rules {
            let hashtable_of_rule = table.entry(rule_id.clone()).or_insert(HashMap::new());
            for terminal in &rule.first_terminals {
                let list = match terminal {
                    Terminal::Literal(str) => {
                        hashtable_of_rule.entry(LookAheadToken::Literal(&str))
                    }
                    Terminal::Token(class) => {
                        hashtable_of_rule.entry(LookAheadToken::Token(*class))
                    }
                };

                list.or_insert(Vec::new()).push(&rule.pattern);
            }
        }
    }

    ParseTable { table }
}
