use std::collections::{HashMap, HashSet};
use std::io::Write;

use super::{grammar::*, grammar_util, rule_name_generator::RuleNameGenerator};

pub enum ChomskyPattern {
    NonTerminal(Vec<RuleId>),
    Terminal(Terminal),
    Rule(RuleId),
}
pub struct ChomskyRule {
    pub identifier: RuleId,
    pub first_terminal: Terminal,
    pub pattern: ChomskyPattern,
}

pub struct Chomsky {
    pub start: RuleId,
    pub rules: Vec<ChomskyRule>,
}

impl Chomsky {
    pub fn from(grammar: Grammar) -> Chomsky {
        let normal_grammar = convert_to_normal_form(grammar);

        let start = normal_grammar.start_rule;

        // first collect all terminal and non-terminal rules
        let mut non_terminal_rules = Vec::new();
        let mut terminal_rules = Vec::new();
        for (identifier, patterns) in normal_grammar.rules {
            for pattern in patterns {
                match pattern {
                    Term::Concatenation(terms) => {
                        non_terminal_rules
                            .push((identifier.to_owned(), unwrap_identifiers(terms)));
                    }
                    Term::Terminal(t) => {
                        terminal_rules.push((identifier.to_owned(), t));
                    }
                    Term::Identifier(rule) if !is_transparent_rule(&rule) => {
                        non_terminal_rules.push((identifier.to_owned(), vec![rule]));
                    }
                    Term::Identifier(_) => {
                        panic!("Non-transparent identifier");
                    }
                    Term::Empty => {
                        panic!("Empty rule");
                    }
                    Term::Alternation(_) => {
                        panic!("Top-level alternations should have been removed");
                    }
                }
            }
        }

        // now calculate for each rule the first terminal
        let mut rules = Vec::new();
        for (id, concatenation) in &non_terminal_rules {
            let mut other = concatenation
                .first()
                .expect(&format!("rule {id} has no rules"));

            loop {
                let maybe_other_rule = non_terminal_rules.iter().find(|(id, r)| id == other);
                match maybe_other_rule {
                    Some((id, referred)) => {
                        other = referred
                            .first()
                            .expect(&format!("rule {other} has no rules"))
                    }
                    None => break,
                }
            }
            let (id, terminal) = terminal_rules
                .iter()
                .find(|(id, r)| id == other)
                .expect(&format!("rule {other} not found"));

            rules.push(
                ChomskyRule {
                    identifier: id.clone(),
                    first_terminal: terminal.to_owned(),
                    pattern: ChomskyPattern::NonTerminal(concatenation.clone()),
                },
            );
        }

        for (id, t) in terminal_rules {
            rules.push(
                ChomskyRule {
                    identifier: id,
                    first_terminal: t.clone(),
                    pattern: ChomskyPattern::Terminal(t),
                },
            );
        }

        Chomsky { start, rules }
    }
}

fn unwrap_identifiers(terms: Vec<Term>) -> Vec<RuleId> {
    let mut pattern = Vec::new();

    for sub_term in terms {
        if let Term::Identifier(id) = sub_term {
            pattern.push(id);
        } else {
            panic!("Nested non-terminals");
        }
    }

    pattern
}

pub fn chomsky_write(chomsky_grammar: &Chomsky) -> String {
    let mut output_string = String::new();

    for rule in &chomsky_grammar.rules {
        output_string.push_str(&format!("{:30} = ", &rule.identifier));

        match &rule.pattern {
            ChomskyPattern::NonTerminal(terms) => {
                output_string.push_str(&terms[0]);
                for t in &terms[1..] {
                    output_string.push_str(", ");
                    output_string.push_str(t)
                }
            }
            ChomskyPattern::Terminal(Terminal::Literal(literal)) => {
                output_string.push('"');
                output_string.push_str(literal);
                output_string.push('"');
            }
            ChomskyPattern::Terminal(Terminal::Token(i)) => {
                output_string.push_str("? ");
                output_string.push_str(i.str());
                output_string.push_str(" ?");
            }
            ChomskyPattern::Rule(identifier) => {
                output_string.push_str(identifier);
            }
        }
        output_string.push_str(";\n");
    }

    output_string
}

pub fn convert_to_normal_form(old_grammar: Grammar) -> Grammar {
    let mut name_generator = old_grammar.name_generator;
    let rules = old_grammar.rules;

    let mut converted_out = std::fs::File::create(format!("chomsky_START.ebnf")).unwrap();
    write!(
        converted_out,
        "{}",
        grammar_util::grammar_write_rules(&rules)
    )
    .unwrap();

    let rules = chomsky_bin_term(rules, &mut name_generator);

    // log
    let mut converted_out = std::fs::File::create(format!("chomsky_BIN_TERM.ebnf")).unwrap();
    write!(
        converted_out,
        "{}",
        grammar_util::grammar_write_rules(&rules)
    )
    .unwrap();

    let rules = chomsky_del(rules);

    // log
    let mut converted_out = std::fs::File::create(format!("chomsky_DEL.ebnf")).unwrap();
    write!(
        converted_out,
        "{}",
        grammar_util::grammar_write_rules(&rules)
    )
    .unwrap();

    let rules = chomsky_unit(rules);

    // log
    let mut converted_out = std::fs::File::create(format!("chomsky_UNIT.ebnf")).unwrap();
    write!(
        converted_out,
        "{}",
        grammar_util::grammar_write_rules(&rules)
    )
    .unwrap();

    let rules = unwrap_top_level_elements(rules);
    let rules = deduplicate(rules);

    // log
    let mut converted_out = std::fs::File::create(format!("chomsky_DEDUP.ebnf")).unwrap();
    write!(
        converted_out,
        "{}",
        grammar_util::grammar_write_rules(&rules)
    )
    .unwrap();

    return Grammar {
        start_rule: old_grammar.start_rule,
        rules,
        name_generator,
    };
}

// find and remove all rules that are renames
fn chomsky_unit(rules: RuleStorage) -> RuleStorage {
    let mut old_rules = rules;

    loop {
        let mut new_rules: RuleStorage = RuleStorage::new();
        let mut any_change = false;

        for (rule_id, terms) in &old_rules {
            let mut new_terms = Vec::new();
            for term in terms {
                match term {
                    Term::Identifier(alias) if is_transparent_rule(alias) => {
                        // For each rule B -> X..., add A -> X...
                        let alias_terms = new_rules
                            .get(alias)
                            .or_else(|| old_rules.get(alias))
                            .unwrap();
                        new_terms.extend(alias_terms.clone());
                        any_change = true;
                    }
                    other => new_terms.push(other.to_owned()),
                }
            }
            new_rules.insert(rule_id.to_owned(), new_terms);
        }

        if !any_change {
            return new_rules;
        }

        old_rules = new_rules;
    }
}

// eliminate all empty rules
fn chomsky_del(rules: RuleStorage) -> RuleStorage {
    let mut nullable_rules = HashSet::new();
    let mut null_rules = HashSet::new();

    for (rule_id, terms) in &rules {
        if terms.iter().all(|t| matches!(t, Term::Empty)) {
            null_rules.insert(rule_id.to_owned());
        } else if terms.iter().any(|t| is_nullable(t)) {
            nullable_rules.insert(rule_id.to_owned());
        }
    }

    let mut new_rules = RuleStorage::new();
    for (rule_id, terms) in rules {
        let mut new_terms = Vec::new();
        for term in terms.clone() {
            match term {
                Term::Empty => {
                    // all empty terms are removed from the grammar
                }
                Term::Concatenation(terms) => {
                    let inlined_terms = inline_nullable(terms, &nullable_rules, &null_rules);
                    for mut concatenation in inlined_terms {
                        if concatenation.len() > 1 {
                            new_terms.push(Term::Concatenation(concatenation));
                        } else if concatenation.len() == 1 {
                            // unwrap the element
                            new_terms.push(concatenation.remove(0));
                        }
                    }
                }
                other => new_terms.push(other),
            }
        }

        assert!(!new_terms.is_empty());
        new_rules.insert(rule_id, new_terms);
    }

    new_rules
}

fn inline_nullable(
    mut concatenation: Vec<Term>,
    nullable_rules: &HashSet<RuleId>,
    null_rules: &HashSet<RuleId>,
) -> Vec<Vec<Term>> {
    if concatenation.is_empty() {
        // base case: we have one prefix: the empty string
        return vec![Vec::new()];
    }

    let last = concatenation.remove(concatenation.len() - 1);

    // remove_nullable will return all possible prefixes
    let mut new_terms = inline_nullable(concatenation, nullable_rules, null_rules);

    match last {
        Term::Empty => {
            // all empty terms are removed from the grammar
            // we do not change the prefix
        }
        Term::Identifier(other_rule) if null_rules.contains(&other_rule) => {
            // this identifier is of an empty rule. Effectively, this is thus an empty term.
            // we do not change the prefix
        }
        Term::Identifier(other_rule) if nullable_rules.contains(&other_rule) => {
            // nullable term, create all prefixes with this term and all prefixes without
            let new_terms_copy = new_terms.clone();
            for t in &mut new_terms {
                t.push(Term::Identifier(other_rule.clone()))
            }
            new_terms.extend(new_terms_copy);
        }
        other => {
            // non-nullable term, append this term to all prefixes
            for t in &mut new_terms {
                t.push(other.clone())
            }
        }
    }

    new_terms
}

fn is_nullable(term: &Term) -> bool {
    match term {
        Term::Empty => true,
        Term::Concatenation(sub_terms) => sub_terms.iter().all(|t| is_nullable(t)),
        Term::Alternation(sub_terms) => sub_terms.iter().any(|t| is_nullable(t)),
        _ => false,
    }
}

// extract all terminals, reduce all concatenations to two
fn chomsky_bin_term(rules: RuleStorage, name_generator: &mut RuleNameGenerator) -> RuleStorage {
    let mut new_rules = rules.clone();
    for (rule_id, terms) in rules {
        let mut new_terms = Vec::new();
        for term in terms {
            // normalize
            match term {
                Term::Terminal(_) | Term::Empty => new_terms.push(term),
                Term::Alternation(terms) => {
                    for t in terms {
                        new_terms.push(normalize_to_concatenation(
                            t,
                            name_generator,
                            &mut new_rules,
                        ))
                    }
                }
                other => new_terms.push(normalize_to_concatenation(
                    other,
                    name_generator,
                    &mut new_rules,
                )),
            };
        }
        assert!(!new_terms.is_empty());
        new_rules.insert(rule_id, new_terms);
    }
    new_rules
}

// normalize to a concatenations of 2 identifiers
fn normalize_to_concatenation(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> Term {
    match term {
        Term::Concatenation(terms) => subdivide_to_two(&terms, name_generator, other_rules),
        other => normalize_to_identifier(other, name_generator, other_rules),
    }
}

// split a concatenation of multiple terms into a series of rules with a concatenation of 2 elememnts
fn subdivide_to_two(
    terms: &[Term],
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> Term {
    assert!(!terms.is_empty());

    // end-condition
    if terms.len() == 2 {
        let normalized_first_term =
            normalize_to_identifier(terms[0].to_owned(), name_generator, other_rules);
        let normalized_second_term =
            normalize_to_identifier(terms[1].to_owned(), name_generator, other_rules);
        return Term::Concatenation(vec![normalized_first_term, normalized_second_term]);
    }

    // edge case (should never happen), unwrap the element
    if terms.len() == 1 {
        return normalize_to_identifier(terms[0].to_owned(), name_generator, other_rules);
    }

    let subdivided_term = subdivide_to_two(&terms[1..], name_generator, other_rules);
    let new_rule_name = name_generator.generate_rule_name();
    other_rules.insert(new_rule_name.clone(), vec![subdivided_term]);

    let normalized_first_term =
        normalize_to_identifier(terms[0].to_owned(), name_generator, other_rules);
    Term::Concatenation(vec![normalized_first_term, Term::Identifier(new_rule_name)])
}

// if this is not an identifier, extract to a new rule
fn normalize_to_identifier(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> Term {
    let mut new_terms = Vec::new();

    match term {
        Term::Identifier(_) | Term::Empty => return term,
        Term::Terminal(_) => new_terms.push(term),
        Term::Alternation(sub_terms) => {
            // if the new rule would be an alteration, unwrap the alteration
            for sub_term in sub_terms {
                new_terms.push(normalize_to_concatenation(
                    sub_term,
                    name_generator,
                    other_rules,
                ))
            }
        }
        Term::Concatenation(terms) => {
            new_terms.push(subdivide_to_two(&terms, name_generator, other_rules))
        }
    };

    assert!(!new_terms.is_empty());
    let new_rule_name = name_generator.generate_rule_name();
    other_rules.insert(new_rule_name.clone(), new_terms);
    Term::Identifier(new_rule_name)
}

// inline all empty rules, top-level alterations and single-element concatenations
fn unwrap_top_level_elements(rules: RuleStorage) -> RuleStorage {
    let mut new_rules = RuleStorage::new();
    for (rule_id, terms) in rules {
        let mut new_terms = Vec::new();
        for term in terms {
            match term {
                Term::Concatenation(sub_terms) if sub_terms.len() < 2 => {
                    println!("Unwrap Concatenation");
                    for t in sub_terms {
                        new_terms.push(t)
                    }
                }
                Term::Alternation(sub_terms) => {
                    println!("Unwrap Alternation");
                    new_terms.extend(sub_terms);
                }

                other => new_terms.push(other),
            }
        }
        if !new_terms.is_empty() {
            new_rules.insert(rule_id, new_terms);
        } else {
            println!("Removed empty rule {}", rule_id);
        }
    }
    new_rules
}

impl std::hash::Hash for Term {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Term::Concatenation(t) => t.hash(state),
            Term::Alternation(t) => t.hash(state),
            Term::Identifier(s) => s.hash(state),
            Term::Terminal(t) => core::mem::discriminant(t).hash(state),
            Term::Empty => core::mem::discriminant(self).hash(state),
        }
    }
}

fn deduplicate(rules: RuleStorage) -> RuleStorage {
    let mut renames: HashMap<RuleId, RuleId> = HashMap::new();
    let mut inverse_rules: HashMap<&[Term], RuleId> = HashMap::new();

    for (rule_id, terms) in &rules {
        if !is_transparent_rule(rule_id) {
            // never rename a transparent rule
            if let Some(other) = inverse_rules.get(terms.as_slice()) {
                if is_transparent_rule(&other) {
                    renames.insert(other.clone(), rule_id.clone());
                }
            } else {
                inverse_rules.insert(&terms, rule_id.clone());
            }
        } else if let Some(other) = inverse_rules.get(terms.as_slice()) {
            renames.insert(rule_id.clone(), other.clone());
        } else {
            inverse_rules.insert(&terms, rule_id.clone());
        }
    }

    println!("Removing {}/{} rules", renames.len(), rules.len());

    apply_renames(rules, renames)
}

fn is_transparent_rule(rule_id: &str) -> bool {
    rule_id.starts_with('_')
}

fn apply_renames(rules: RuleStorage, renames: HashMap<RuleId, RuleId>) -> RuleStorage {
    let mut new_rules = HashMap::new();
    for (rule_id, mut terms) in rules {
        if renames.contains_key(&rule_id) {
            assert!(is_transparent_rule(&rule_id));
            continue;
        }

        for term in &mut terms {
            grammar_util::transform_terminals(term, &|t| rename_or_this(t, &renames))
        }

        assert!(!terms.is_empty());
        new_rules.insert(rule_id, terms);
    }

    new_rules
}

fn rename_or_this(original_terminal: Term, renames: &HashMap<RuleId, RuleId>) -> Term {
    match original_terminal {
        Term::Identifier(id) if renames.contains_key(&id) => {
            let new_rule_name = renames.get(&id).unwrap();
            Term::Identifier(new_rule_name.clone())
        }
        _ => original_terminal,
    }
}
