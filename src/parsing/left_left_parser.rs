use std::{
    collections::{HashMap, HashSet},
    io::Write,
};

use simple_error::SimpleError;

use crate::transforming::{
    grammar::{Grammar, RuleId, RuleStorage, Term, Terminal},
    rule_name_generator::RuleNameGenerator,
};

use super::{
    parser::*,
    rule_nodes::RuleNode,
    token::{Token, TokenClass},
};

type ParseResult<'prog, 'g> = Result<ParseNode<'prog, 'g>, Vec<Failure<'g>>>;

pub struct Parser {
    start_rule: RuleId,
    parse_table: ParseTable,
    xml_out: Option<std::fs::File>,
}

struct ParseTable {
    rules: Vec<Term>,
    // rows are vectors, to avoid string allocations on `get`
    lookup_table: HashMap<RuleId, Vec<(Vec<Terminal>, usize)>>,
}

impl<'c> ParseTable {
    pub fn get(&'c self, parse_stack: &str, look_ahead: &[Token<'_>]) -> Option<&'c Term> {
        let row = self.lookup_table.get(parse_stack)?;
        for (prefix, index) in row {
            let mut comparison_iter = prefix.iter().zip(look_ahead);
            if comparison_iter.all(|(expected, actual)| ParseTable::equal(expected, actual)) {
                return Some(&self.rules[*index]);
            }
        }
        None
    }

    fn equal(expected: &Terminal, actual: &Token) -> bool {
        match expected {
            Terminal::Literal(s) => s == actual.slice,
            Terminal::Token(t) => t == &actual.class,
        }
    }

    pub fn get_expected(&'c self, parse_stack: &str) -> Vec<&'c str> {
        let row = &self.lookup_table.get(parse_stack);

        if row.is_none() {
            return Vec::new();
        }

        row.unwrap()
            .iter()
            .map(|(lh, _)| lh)
            .flatten()
            .map(|t| t.as_str())
            .collect()
    }
}

impl<'g> Parser {
    pub fn new(grammar: Grammar, xml_out: Option<std::fs::File>) -> Parser {
        let start_rule = grammar.start_rule.clone();

        let parse_table = construct_parse_table(grammar);
        Parser {
            start_rule,
            parse_table,
            xml_out,
        }
    }

    pub fn parse_program<'prog: 'g>(
        &'g self,
        tokens: &'prog [Token<'prog>],
    ) -> Result<RuleNode<'prog, 'g>, Vec<Failure<'g>>> {
        let interpretation = self.apply_rule_with_log(0, tokens, &self.start_rule)?;

        match interpretation {
            ParseNode::Rule(parsed_program) => {
                if !parsed_program.tokens.len() == tokens.len() {
                    Err(vec![Failure::IncompleteParse {
                        char_idx: parsed_program.tokens.len(),
                    }])
                } else {
                    Ok(parsed_program)
                }
            }
            _ => Err(vec![Failure::InternalError(SimpleError::new(
                "top-level node was not a rule node",
            ))]),
        }
    }

    fn apply_rule_with_log<'prog: 'g>(
        &'g self,
        token_index: usize,
        tokens: &'prog [Token<'prog>],
        rule_name: &'g str,
    ) -> ParseResult<'prog, 'g> {
        self.log_enter(rule_name);
        let result = self.apply_rule(token_index, tokens, rule_name);
        self.log_exit(rule_name);

        return result;
    }

    fn apply_rule<'prog: 'g>(
        &'g self,
        token_index: usize,
        tokens: &'prog [Token<'prog>],
        rule_name: &'g str,
    ) -> ParseResult<'prog, 'g> {
        if token_index == tokens.len() {
            return Err(vec![Failure::EndOfFile {
                expected: rule_name,
            }]);
        }

        let is_transparent = rule_name.as_bytes()[0] == b'_';
        let pattern = self.parse_table.get(rule_name, &tokens[token_index..]);

        if pattern.is_some() {
            let result_of_term = self.apply_term(pattern.unwrap(), tokens, token_index)?;

            if is_transparent {
                return Ok(result_of_term);
            }

            let tokens_end = token_index + result_of_term.num_tokens();
            Ok(ParseNode::Rule(RuleNode {
                rule_name,
                tokens: &tokens[token_index..tokens_end],
                sub_rules: result_of_term.unwrap_to_rulenodes(),
            }))
        } else {
            let expected = self.parse_table.get_expected(rule_name);

            if expected.is_empty() {
                return Err(vec![Failure::InternalError(SimpleError::new(format!(
                    "No parse table tokens for rule {}",
                    rule_name
                )))]);
            } else {
                return Err(expected
                    .into_iter()
                    .map(|expected| Failure::UnexpectedToken {
                        char_idx: token_index,
                        expected,
                    })
                    .collect());
            }
        }
    }

    fn apply_term<'prog: 'g>(
        &'g self,
        term: &'g Term,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
    ) -> ParseResult<'prog, 'g> {
        match term {
            Term::Terminal(terminal) => self.apply_terminal(terminal, tokens, token_index),
            Term::Concatenation(sub_terms) => {
                self.apply_concatenation(sub_terms, tokens, token_index)
            }
            Term::Alternation(_) => Err(vec![Failure::InternalError(SimpleError::new(
                "Alterations should have been removed",
            ))]),
            Term::Identifier(rule_name) => self.apply_rule(token_index, tokens, rule_name),
            Term::Empty => Ok(ParseNode::EmptyNode),
        }
    }

    fn apply_terminal<'prog: 'g>(
        &'g self,
        terminal: &'g Terminal,
        tokens: &'prog [Token<'prog>],
        token_index: usize,
    ) -> ParseResult<'prog, 'g> {
        let next_token = &tokens[token_index];
        let has_match = match terminal {
            Terminal::Literal(str) => str == next_token.slice,
            Terminal::Token(class) => *class == next_token.class,
        };

        if has_match {
            self.log_consume_token(next_token);
            return Ok(ParseNode::Terminal(&tokens[token_index]));
        } else {
            return Err(vec![Failure::UnexpectedToken {
                char_idx: token_index,
                expected: terminal.as_str(),
            }]);
        }
    }

    fn apply_concatenation<'prog: 'g>(
        &'g self,
        terms: &'g [Term],
        tokens: &'prog [Token<'prog>],
        token_index: usize,
    ) -> ParseResult<'prog, 'g> {
        let mut new_sub_rules = Vec::new();
        let mut sub_rule_token_index = token_index;

        for term in terms {
            let applied_rule = self.apply_term(term, tokens, sub_rule_token_index);

            match applied_rule {
                Ok(rule) => {
                    sub_rule_token_index += rule.num_tokens();
                    new_sub_rules.push(rule);
                }
                Err(failures) => return Err(failures),
            }
        }

        Ok(ParseNode::Vec(new_sub_rules))
    }

    fn log_consume_token(&'g self, token: &Token<'_>) {
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

    fn log_enter(&'g self, rule_name: &str) {
        if let Some(mut file) = self.xml_out.as_ref() {
            if !is_transparent(rule_name) {
                let _ = writeln!(file, "<{rule_name}>");
            }
        }
    }

    fn log_exit(&'g self, rule_name: &str) {
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
    let mut table: HashMap<RuleId, Vec<(Vec<Terminal>, usize)>> = HashMap::new();
    let mut patterns: Vec<Term> = Vec::new();

    let maximum_lookahead = 2;
    let follow_sets = get_follow_terminals(&grammar, maximum_lookahead);

    // first calculate all first terminals before moving out of grammar
    let mut first_terminal_map = HashMap::new();
    for (rule_id, rule_patterns) in &grammar.rules {
        first_terminal_map.insert(
            rule_id.clone(),
            rule_patterns
                .iter()
                .map(|t| get_first_n_terminals_of_term(t, maximum_lookahead, &grammar))
                .collect::<Vec<_>>(),
        );
    }
    println!("first_terminal_map = {:?}", first_terminal_map);

    for (rule_id, rule_patterns) in grammar.rules {
        let follow_set = follow_sets.get(&rule_id).unwrap();
        let first_terminals_of_rule = first_terminal_map.remove(&rule_id).unwrap();

        for (pattern, first_terminals) in rule_patterns.into_iter().zip(first_terminals_of_rule) {
            let this_idx = get_index_or_add(&mut patterns, pattern);

            let table_row = table.entry(rule_id.clone()).or_insert(Vec::new());

            for terminal in first_terminals {
                if terminal.is_empty() {
                    // there can only be one None, because first_terminals is a HashSet
                    for additional_terminal in follow_set {
                        table_row.push((additional_terminal.clone(), this_idx));
                    }
                } else {
                    table_row.push((terminal, this_idx));
                }
            }
        }
    }

    for (rule, row) in &table {
        for (terminal1, follows1) in row {
            for (terminal2, follows2) in row {
                if terminal1 == terminal2 && follows1 != follows2 {
                    println!(
                        "Rule {} strting with '{:?}' could be both '{:?}' and '{:?}'",
                        rule, terminal1, patterns[*follows1], patterns[*follows2]
                    );
                }
            }
        }
    }

    // note that the original grammar is destroyed
    ParseTable {
        lookup_table: table,
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

fn add_terminal(
    terminal: Terminal,
    this_idx: usize,
    literals_row: &mut HashMap<String, Vec<usize>>,
    tokens_row: &mut HashMap<TokenClass, Vec<usize>>,
) {
    match terminal {
        Terminal::Literal(str) => literals_row.entry(str).or_insert(Vec::new()).push(this_idx),
        Terminal::Token(class) => tokens_row.entry(class).or_insert(Vec::new()).push(this_idx),
    }
}

fn get_follow_terminals(
    grammar: &Grammar,
    maximum_lookahead: usize,
) -> HashMap<RuleId, HashSet<Vec<Terminal>>> {
    // "initialize Fo(S) with { EOF } and every other Fo(A) with the empty set"
    // (we do not initialize Fo(S) however, because our lexer does not append the EOF symbol)
    let mut follow_terminals = HashMap::new();

    loop {
        let mut has_change = false;

        for (rule_a, _) in &grammar.rules {
            let mut follow_set = follow_terminals.remove(rule_a).unwrap_or(HashSet::new());
            let len = follow_set.len();

            for (rule_b, pattern) in &grammar.rules {
                for term in pattern {
                    // if there is a rule of the form `B = vAw`, then
                    find_terms_containing_rule(rule_a, term, &mut |mut w_terms| {
                        if let Some(term) = w_terms.next() {
                            let first_terminals =
                                get_first_n_terminals_of_term(term, maximum_lookahead, &grammar);
                            // if `empty` is in Fi(w), then add Fo(B) to Fo(A)
                            if first_terminals.contains(&Vec::new()) {
                                if let Some(follow_set_of_b) = follow_terminals.get(rule_b) {
                                    follow_set.extend(follow_set_of_b.clone());
                                }
                            }
                            // if the terminal a is in Fi(w), then add a to Fo(A)
                            follow_set.extend(first_terminals);
                        } else {
                            // if w has length 0, then add Fo(B) to Fo(A)
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

fn get_first_n_terminals<'g>(
    terms: &'g [Term],
    size: usize,
    grammar: &'g Grammar,
) -> HashSet<Vec<Terminal>> {
    terms
        .iter()
        .flat_map(|t| get_first_n_terminals_of_term(t, size, grammar))
        .collect()
}

fn get_first_n_terminals_of_term<'g>(
    term: &Term,
    size: usize,
    grammar: &Grammar,
) -> HashSet<Vec<Terminal>> {
    match term {
        Term::Concatenation(terms) => {
            let mut prefixes: HashSet<Vec<Terminal>> = HashSet::from([Vec::new()]);

            for term in terms {
                let new_postfix = get_first_n_terminals_of_term(term, size, grammar);
                prefixes = limited_carthesian_product(&prefixes, &new_postfix, size);

                if prefixes.iter().all(|p| p.len() >= size) {
                    break;
                }
            }
            return prefixes;
        }
        Term::Alternation(terms) => get_first_n_terminals(terms, size, grammar),
        Term::Identifier(id) => {
            let rule_patterns = grammar
                .rules
                .get(id)
                .expect("Rule refers to a rule that does not exist");
            return get_first_n_terminals(rule_patterns, size, grammar);
        }
        Term::Terminal(t) => HashSet::from([vec![t.clone()]]),
        Term::Empty => HashSet::from([Vec::new()]),
    }
}

// returns the carthesian product of all prefixes and postfixes, limited to a size of `max_size`
fn limited_carthesian_product(
    prefixes: &HashSet<Vec<Terminal>>,
    postfixes: &HashSet<Vec<Terminal>>,
    max_size: usize,
) -> HashSet<Vec<Terminal>> {
    let mut new_prefixes = HashSet::new();
    for prefix in prefixes {
        if prefix.len() == max_size {
            continue;
        }

        for postfix in postfixes {
            // add `(prefix ++ postfix)`, limited to a size of `max_size`
            let mut new_prefix = prefix.clone();
            let max_elements_from_postfix = std::cmp::min(postfix.len(), max_size - prefix.len());
            for i in 0..max_elements_from_postfix {
                new_prefix.push(postfix[i].clone());
            }
            new_prefixes.insert(new_prefix);
        }
    }

    return new_prefixes;
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
