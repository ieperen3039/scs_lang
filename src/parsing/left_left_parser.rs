use std::{
    collections::{HashMap, HashSet, VecDeque},
    convert::identity,
    io::Write,
};

use simple_error::SimpleError;

use crate::transformation::{
    grammar::{Grammar, RuleId, RuleStorage, Term, Terminal},
    rule_name_generator::RuleNameGenerator,
};

use super::{
    parser::*,
    rule_nodes::RuleNode,
    token::{Token, TokenClass},
};

pub type ParseResult<'prog, 'c> =
    Box<dyn Iterator<Item = Result<ParseNode<'prog, 'c>, Failure<'c>>> + 'c>;

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
    pub fn get(&'c self, parse_stack: &str, look_ahead: Option<&Token>) -> Vec<&'c Term> {
        self.lookup_table
            .get(parse_stack)
            .into_iter()
            .flatten()
            .filter(|(expected, _)| ParseTable::equal(expected, look_ahead))
            .map(|(_, index)| &self.rules[*index])
            .collect()
    }

    pub fn equal(expected: &Terminal, actual: Option<&Token>) -> bool {
        match (expected, actual) {
            (Terminal::Literal(s), Some(token)) => s == token.slice,
            (Terminal::Token(t), Some(token)) => t == &token.class,
            (Terminal::EndOfFile, None) => true,
            _ => false,
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
        let interpretations = self.apply_rule(&tokens, 0, &self.start_rule);

        let mut longest_success = ParseNode::EmptyNode;
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
                },
                Ok(interpretation) if interpretation.num_tokens() == tokens.len() => {
                    if let ParseNode::Rule(rule_node) = interpretation {
                        // SUCCESS
                        return Ok(rule_node);
                    } else {
                        return Err(vec![Failure::InternalError(SimpleError::new(
                            "Primary rule was no rule",
                        ))]);
                    }
                },
                Ok(interpretation) => {
                    longest_success = interpretation;
                },
            }
        }

        assert_ne!(longest_success.num_tokens(), tokens.len());

        if let ParseNode::EmptyNode = longest_success {
            return Err(failures.into());
        }

        let past_the_end_token = &tokens[longest_success.num_tokens()];
        let mut furthest_failures = vec![Failure::IncompleteParse {
            char_idx: past_the_end_token.char_idx,
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

    #[allow(dead_code)]
    fn apply_rule_with_log<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        rule_name: &'c str,
    ) -> ParseResult<'prog, 'c> {
        self.log_enter(rule_name);
        let result = self.apply_rule(tokens, token_index, rule_name);
        self.log_exit(rule_name);

        return result;
    }

    fn apply_rule<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        rule_name: &'c str,
    ) -> ParseResult<'prog, 'c> {
        let next_token = tokens.get(token_index);

        let possible_patterns = self.parse_table.get(rule_name, next_token);

        if possible_patterns.is_empty() {
            return Box::new(self.parse_table.get_expected(rule_name).into_iter().map(
                move |expected| {
                    Err(Failure::UnexpectedToken {
                        char_idx: char_idx_of(next_token, tokens),
                        expected,
                    })
                },
            ));
        }

        let result_of_terms = possible_patterns
            .into_iter()
            .flat_map(move |term| self.apply_term(tokens, token_index, term));

        if is_transparent(rule_name) {
            return Box::new(result_of_terms);
        }

        // not transparent: wrap every ParseNode into a RuleNode
        // replace every empty parse with an EmptyNode
        Box::new(result_of_terms.map(move |result| {
            result.map(|parse_node| match parse_node.num_tokens() {
                0 => ParseNode::EmptyNode,
                num_tokens => ParseNode::Rule(RuleNode {
                    rule_name,
                    tokens: &tokens[token_index..(token_index + num_tokens)],
                    sub_rules: parse_node.unwrap_to_rulenodes(),
                }),
            })
        }))
    }

    fn apply_term<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        term: &'c Term,
    ) -> ParseResult<'prog, 'c> {
        match term {
            Term::Identifier(rule_name) => self.apply_rule(tokens, token_index, rule_name),
            Term::Concatenation(sub_terms) => {
                self.apply_concatenation(tokens, token_index, sub_terms)
            },
            Term::Alternation(sub_terms) => self.apply_alternation(tokens, token_index, sub_terms),
            Term::Terminal(terminal) => Box::new(std::iter::once(self.parse_terminal(
                tokens,
                token_index,
                terminal,
            ))),
            Term::Empty => Box::new(std::iter::once(Ok(ParseNode::EmptyNode))),
        }
    }

    fn apply_alternation<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        sub_terms: &'c [Term],
    ) -> ParseResult<'prog, 'c> {
        Box::new(
            sub_terms
                .into_iter()
                .flat_map(move |term| self.apply_term(tokens, token_index, term)),
        )
    }

    fn apply_concatenation<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        sub_terms: &'c [Term],
    ) -> ParseResult<'prog, 'c> {
        let term = &sub_terms[0];

        let all_results = self.apply_term(tokens, token_index, term);

        if sub_terms.len() > 1 {
            Box::new(all_results.flat_map(move |term_result| match term_result {
                Ok(term_interp) => {
                    self.continue_concatenation(tokens, token_index, term_interp, &sub_terms[1..])
                },
                Err(failure) => Box::new(std::iter::once(Err(failure))),
            }))
        } else {
            all_results
        }
    }

    fn continue_concatenation<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        term_interp: ParseNode<'prog, 'c>,
        sub_terms: &'c [Term],
    ) -> ParseResult<'prog, 'c> {
        let new_token_index = token_index + term_interp.num_tokens();
        Box::new(
            self.apply_concatenation(tokens, new_token_index, sub_terms)
                .map(move |result| {
                    result.map(|postfix| ParseNode::Vec(vec![term_interp.clone(), postfix]))
                }),
        )
    }

    fn parse_terminal<'prog: 'c>(
        &'c self,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
        expected: &'c Terminal,
    ) -> Result<ParseNode<'prog, 'c>, Failure<'c>> {
        let actual = tokens.get(token_index);
        let has_match = ParseTable::equal(expected, actual);

        if let Some(token) = actual {
            if has_match {
                self.log_consume_token(token);
                Ok(ParseNode::Terminal(token))
            } else {
                Err(Failure::UnexpectedToken {
                    char_idx: token.char_idx,
                    expected: expected.as_str(),
                })
            }
        } else {
            if has_match {
                Ok(ParseNode::EmptyNode)
            } else {
                Err(Failure::EndOfFile {
                    expected: expected.as_str(),
                })
            }
        }
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

fn char_idx_of(next_token: Option<&Token>, tokens: &[Token]) -> usize {
    match next_token {
        Some(t) => t.char_idx,
        None => {
            let last_token = tokens.last().unwrap();
            last_token.char_idx + last_token.slice.len()
        },
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

    let mut converted_out = std::fs::File::create("grammar.ebnf").unwrap();
    write!(converted_out, "{}\n\n", Grammar::write(&grammar)).unwrap();

    let follow_sets = get_follow_terminals(&grammar);

    // first calculate all first terminals before moving out of grammar
    let mut first_terminal_map = HashMap::new();
    for (rule_id, rule_patterns) in &grammar.rules {
        first_terminal_map.insert(
            rule_id.clone(),
            rule_patterns
                .iter()
                .map(|t| get_first_terminals_of_term(t, &grammar, rule_id))
                .collect::<Vec<_>>(),
        );
    }

    let first_terminal_map_copy = first_terminal_map.clone();

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
    
    if false
    {
        println!("");
        println!("first_sets = {:?}", first_terminal_map_copy);
        println!("");
        println!("follow_sets = {:?}", follow_sets);
        println!("");
        println!("lookup_table = {:?}", lookup_table);
        println!("");
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
        },
    }
}

fn get_follow_terminals(grammar: &Grammar) -> HashMap<RuleId, HashSet<Terminal>> {
    // initialize Fo(S) with { EOF } and every other Fo(A) with the empty set
    let mut follow_terminals = HashMap::new();
    follow_terminals.insert(
        grammar.start_rule.clone(),
        HashSet::from([Terminal::EndOfFile]),
    );

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
                    find_terms_containing_rule(rule_a, term, &mut |w_terms| {
                        let first_terminals =
                            get_first_terminals_of_concatenation(w_terms, &grammar, rule_a);
                        // if `empty` is in Fi(w), then add Fo(B) to Fo(A)
                        if first_terminals.contains(&None) {
                            if let Some(follow_set_of_b) = follow_terminals.get(rule_b) {
                                follow_set.extend(follow_set_of_b.clone());
                            }
                        }
                        // if the terminal a is in Fi(w), then add a to Fo(A)
                        follow_set.extend(first_terminals.into_iter().filter_map(identity));
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
    Action: FnMut(&[Term]),
{
    match term {
        Term::Concatenation(sub_terms) => {
            let mut sub_term_iter = sub_terms.iter();
            while let Some(sub_term) = sub_term_iter.next() {
                if let Term::Identifier(id) = sub_term {
                    if id.as_ref() == rule_id {
                        // converting a slice iterator to a slice creates no copy
                        action(sub_term_iter.as_slice())
                    }
                } else {
                    find_terms_containing_rule(rule_id, sub_term, action)
                }
            }
        },
        Term::Alternation(terms) => {
            for t in terms {
                find_terms_containing_rule(rule_id, t, action)
            }
        },
        Term::Identifier(id) if id.as_ref() == rule_id => {
            action(&[]);
        },
        Term::Identifier(_) | Term::Terminal(_) | Term::Empty => {},
    }
}

fn get_first_terminals_of_alternation(
    terms: &[Term],
    grammar: &Grammar,
    original_rule: &RuleId
) -> HashSet<Option<Terminal>> {
    terms
        .iter()
        .flat_map(|t| get_first_terminals_of_term(t, grammar, original_rule))
        .collect()
}

fn get_first_terminals_of_term(term: &Term, grammar: &Grammar, original_rule: &RuleId) -> HashSet<Option<Terminal>> {
    match term {
        Term::Concatenation(terms) => get_first_terminals_of_concatenation(terms, grammar, original_rule),
        Term::Alternation(terms) => get_first_terminals_of_alternation(terms, grammar, original_rule),
        Term::Identifier(id) => {
            assert!(id != original_rule, "the grammar contains left-recursion");
            let rule_patterns = grammar
                .rules
                .get(id)
                .expect("Rule refers to a rule that does not exist");
            get_first_terminals_of_alternation(rule_patterns, grammar, original_rule)
        },
        Term::Terminal(t) => HashSet::from([Some(t.clone())]),
        Term::Empty => HashSet::from([None]),
    }
}

fn get_first_terminals_of_concatenation(
    terms: &[Term],
    grammar: &Grammar,
    original_rule: &RuleId
) -> HashSet<Option<Terminal>> {
    if terms.is_empty() {
        return HashSet::from([None]);
    }

    let mut first_terminals = get_first_terminals_of_term(&terms[0], grammar, original_rule);
    let did_have_none = first_terminals.remove(&None);
    if did_have_none {
        first_terminals.extend(get_first_terminals_of_concatenation(&terms[1..], grammar, original_rule))
    }
    first_terminals
}

fn remove_alterations(mut grammar: Grammar) -> Grammar {
    let mut new_rules = RuleStorage::new();
    for (rule_id, pattern) in grammar.rules {
        let mut new_patterns: Vec<Term> = Vec::new();
        for term in pattern {
            let new_term = extract_alterations(term, &mut grammar.name_generator, &mut new_rules);
            new_patterns.push(new_term);
        }
        new_rules.insert(rule_id, new_patterns);
    }
    grammar.rules = new_rules;
    return grammar;
}

// extract all alterations to new rules
fn extract_alterations(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> Term {
    match term {
        Term::Alternation(sub_terms) => {
            let new_rule_name = name_generator.generate_rule_name();
            other_rules.insert(new_rule_name.clone(), sub_terms);
            Term::Identifier(new_rule_name)
        },
        Term::Concatenation(sub_terms) => Term::Concatenation(
            sub_terms
                .into_iter()
                .map(|t| extract_alterations(t, name_generator, other_rules))
                .collect(),
        ),
        _ => term,
    }
}
