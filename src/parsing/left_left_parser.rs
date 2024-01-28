use std::{
    collections::{
        hash_map::{DefaultHasher, Keys},
        HashMap, HashSet,
    },
    hash::Hash,
    io::Write,
};

use crate::transforming::{
    grammar::{Grammar, RuleId, RuleStorage, Term, Terminal},
    rule_name_generator::RuleNameGenerator,
};

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
    rules: Vec<Term>,
    // rows are vectors, to avoid string allocations on `get`
    lookup_table: HashMap<RuleId, Vec<(Terminal, usize)>>,
}

impl<'c> ParseTable {
    pub fn get(&'c self, parse_stack: &str, look_ahead: &Token<'_>) -> Vec<&'c Term> {
        self.lookup_table
            .get(parse_stack)
            .into_iter()
            .flatten()
            .filter(|(expected, _)| ParseTable::equal(expected, look_ahead))
            .map(|(_, index)| &self.rules[*index])
            .collect()
    }

    fn equal(expected: &Terminal, actual: &Token) -> bool {
        match expected {
            Terminal::Literal(s) => s == actual.slice,
            Terminal::Token(t) => t == &actual.class,
        }
    }

    pub fn get_expected(&'c self, parse_stack: &str) -> Vec<&'c str> {
        self.lookup_table
            .get(parse_stack)
            .into_iter()
            .flatten()
            .map(|(expected, _)| expected.as_str())
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
                    let result = self.apply_non_terminal(rule_name, token_index, tokens, todo!());
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

/// `T[A,a]` contains the rule "A => w" if and only if
/// (`w` may start with an `a`) &&
/// (`w` may be empty and `A` may be followed by an `a`)
fn construct_parse_table(grammar: Grammar) -> ParseTable {
    let mut lookup_table: HashMap<RuleId, Vec<(Terminal, usize)>> = HashMap::new();
    let mut patterns: Vec<Term> = Vec::new();

    let grammar = remove_alterations(grammar);

    let follow_sets = get_follow_terminals(&grammar);

    // first calculate all first terminals before moving out of grammar
    let mut first_terminal_map = HashMap::new();
    for (rule_id, rule_patterns) in &grammar.rules {
        first_terminal_map.insert(
            rule_id.clone(),
            rule_patterns
                .iter()
                .map(|t| get_first_terminals_of_term(t, &grammar))
                .collect::<Vec<_>>(),
        );
    }

    for (rule_id, rule_patterns) in grammar.rules {
        let follow_set = follow_sets.get(&rule_id).unwrap();
        let first_terminals_of_rule = first_terminal_map.remove(&rule_id).unwrap();

        for (pattern, first_terminals) in rule_patterns.into_iter().zip(first_terminals_of_rule) {
            let this_idx = get_index_or_add(&mut patterns, pattern);

            let row = lookup_table.entry(rule_id.clone()).or_insert(Vec::new());

            for terminal in first_terminals {
                if let Some(terminal) = terminal {
                    row.push((terminal, this_idx));
                } else {
                    for additional_terminal in follow_set {
                        row.push((additional_terminal.clone(), this_idx));
                    }
                };
            }
        }
    }

    // note that the original grammar is destroyed
    ParseTable {
        lookup_table,
        rules: patterns,
    }
}

fn get_index_or_add(patterns: &mut Vec<Term>, pattern: Term) -> usize {
    let index_of_pattern = patterns.iter().position(|p| p == &pattern);
    match index_of_pattern {
        Some(idx) => idx,
        None => {
            patterns.push(pattern);
            patterns.len() - 1
        }
    }
}

fn get_follow_terminals(grammar: &Grammar) -> HashMap<RuleId, HashSet<Terminal>> {
    // initialize Fo(S) with { EOF } and every other Fo(A) with the empty set
    // (we do not initialize Fo(S) however, because our lexer does not append the EOF symbol)
    let mut follow_terminals = HashMap::new();

    loop {
        let mut has_change = false;

        for (rule_a, _) in &grammar.rules {
            // let follow_set = follow_terminals
            //     .entry(rule_a.clone())
            //     .or_insert(HashSet::new());
            let mut follow_set = follow_terminals.remove(rule_a).unwrap_or(HashSet::new());
            let len = follow_set.len();

            for (rule_b, pattern) in &grammar.rules {
                for term in pattern {
                    // if there is a rule of the form `B = vAw`, then
                    find_terms_containing_rule(rule_a, term, &mut |mut w_terms| {
                        if let Some(term) = w_terms.next() {
                            let first_terminals = get_first_terminals_of_term(term, &grammar);
                            //     if `empty` is in Fi(w), then add Fo(B) to Fo(A)
                            if first_terminals.contains(&None) {
                                if let Some(follow_set_of_b) = follow_terminals.get(rule_b) {
                                    follow_set.extend(follow_set_of_b.clone());
                                }
                            }
                            //     if the terminal a is in Fi(w), then add a to Fo(A)
                            follow_set.extend(first_terminals.iter().filter_map(Option::to_owned));
                        } else {
                            //     if w has length 0, then add Fo(B) to Fo(A)
                            if let Some(follow_set_of_b) = follow_terminals.get(rule_b) {
                                follow_set.extend(follow_set_of_b.clone());
                            }
                        }
                    })
                }
            }
            if len != follow_set.len() {
                has_change = true;
            }

            follow_terminals.insert(rule_a.clone(), follow_set);
        }

        // repeat step 2 until all Fo sets stay the same.
        if !has_change {
            return follow_terminals;
        }
    }
}

// finds all rules of the form `B = vAw`, where `A` is `rule_id`, and runs `action` on `w`
fn find_terms_containing_rule<'t, Action>(rule_id: &str, term: &'t Term, action: &mut Action)
where
    Action: FnMut(std::slice::Iter<'t, Term>),
{
    match term {
        Term::Concatenation(sub_terms) => {
            let mut sub_term_iter = sub_terms.iter();
            while let Some(sub_term) = sub_term_iter.next() {
                if let Term::Identifier(id) = sub_term {
                    if id.as_ref() == rule_id {
                        // pass an iterator to the remaining sub_terms of this term
                        action(sub_term_iter.clone())
                    }
                } else {
                    find_terms_containing_rule(rule_id, sub_term, action)
                }
            }
        }
        Term::Alternation(terms) => {
            for t in terms {
                find_terms_containing_rule(rule_id, t, action)
            }
        }
        Term::Identifier(id) if id.as_ref() == rule_id => {
            action([].iter());
        }
        Term::Identifier(_) | Term::Terminal(_) | Term::Empty => {}
    }
}

fn get_first_terminals<'g>(terms: &'g [Term], grammar: &'g Grammar) -> HashSet<Option<Terminal>> {
    terms
        .iter()
        .flat_map(|t| get_first_terminals_of_term(t, grammar))
        .collect()
}

fn get_first_terminals_of_term<'g>(term: &Term, grammar: &Grammar) -> HashSet<Option<Terminal>> {
    match term {
        Term::Concatenation(terms) => get_first_terminals_of_term(&terms[0], grammar),
        Term::Alternation(terms) => get_first_terminals(terms, grammar),
        Term::Identifier(id) => {
            let rule_patterns = grammar
                .rules
                .get(id)
                .expect("Rule refers to a rule that does not exist");
            get_first_terminals(rule_patterns, grammar)
        }
        Term::Terminal(t) => HashSet::from([Some(t.clone())]),
        Term::Empty => HashSet::from([None]),
    }
}

fn remove_alterations(mut grammar: Grammar) -> Grammar {
    let mut new_rules = RuleStorage::new();
    for (rule_id, pattern) in grammar.rules {
        let mut new_patterns = Vec::new();
        for term in pattern {
            let new_term =
                remove_term_alterations(term, &mut grammar.name_generator, &mut new_rules);
            new_patterns.push(new_term);
        }
        new_rules.insert(rule_id, new_patterns);
    }
    grammar.rules = new_rules;
    return grammar;
}

// extract all alterations to top-level
fn remove_term_alterations(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> Term {
    match term {
        Term::Alternation(sub_terms) => {
            let new_rule_name = name_generator.generate_rule_name();
            other_rules.insert(new_rule_name.clone(), sub_terms);
            return Term::Identifier(new_rule_name);
        }
        _ => return term,
    };
}
