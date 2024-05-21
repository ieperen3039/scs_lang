use std::collections::{HashMap, HashSet};
use std::io::Write;

use super::{grammar::*, grammar_util, rule_name_generator::RuleNameGenerator};

// True chomsky normal forms has non-terminal rules of at most 2 elements.
// we support other rule lengths as well
const TERM_LENGTH: usize = 2;

#[derive(Debug, Clone)]
pub enum ChomskyPattern {
    NonTerminal(Vec<RuleId>),
    Terminal(Terminal),
}

#[derive(Debug, Clone)]
pub struct ChomskyRule {
    pub pattern: ChomskyPattern,
    pub first_terminals: Vec<Terminal>,
}

pub struct Chomsky {
    pub start: RuleId,
    pub rules: HashMap<RuleId, Vec<ChomskyRule>>,
}

impl Chomsky {
    pub fn from(grammar: Grammar) -> Chomsky {
        let normal_grammar = Chomsky::convert_to_normal_form(grammar);

        let start = normal_grammar.start_rule;

        let mut raw_rules = HashMap::new();

        for (identifier, patterns) in normal_grammar.rules {
            let chomsky_terms = to_chomsky_terms(patterns);
            raw_rules.insert(identifier, chomsky_terms);
        }

        // now calculate for each rule the list of terminals
        let mut rules = HashMap::new();

        for (id, patterns) in &raw_rules {
            let mut pattern_rule = Vec::new();

            for pattern in patterns {
                let mut open_set: Vec<&ChomskyPattern> = vec![pattern];
                let mut these_terminals = Vec::new();

                while !open_set.is_empty() {
                    let next_to_read = open_set.pop().unwrap();

                    match next_to_read {
                        ChomskyPattern::NonTerminal(other_rules) => {
                            let first_rule =
                                other_rules.first().expect("Some rule contains no terms");
                            let value = raw_rules
                                .get(first_rule)
                                .expect(&format!("Rule {first_rule} not found"));
                            for rule in value {
                                open_set.push(rule)
                            }
                        },
                        ChomskyPattern::Terminal(t) => {
                            if these_terminals.iter().find(|&other| other == t).is_none() {
                                these_terminals.push(t.clone())
                            }
                        },
                    }
                }

                assert!(!these_terminals.is_empty());
                pattern_rule.push(ChomskyRule {
                    pattern: pattern.clone(),
                    first_terminals: these_terminals,
                })
            }
            rules.insert(id.clone(), pattern_rule);
        }

        Chomsky { start, rules }
    }

    pub fn convert_to_normal_form(old_grammar: Grammar) -> Grammar {
        let mut name_generator = old_grammar.name_generator;
        let rules = old_grammar.rules;

        let mut converted_out = std::fs::File::create(format!("chomsky_START.ebnf")).unwrap();
        write!(converted_out, "{}", Grammar::write_rules(&rules)).unwrap();

        let rules = chomsky_bin_term(rules, &mut name_generator);

        // log
        let mut converted_out = std::fs::File::create(format!("chomsky_BIN_TERM.ebnf")).unwrap();
        write!(converted_out, "{}", Grammar::write_rules(&rules)).unwrap();

        let rules = chomsky_del(rules);

        // log
        let mut converted_out = std::fs::File::create(format!("chomsky_DEL.ebnf")).unwrap();
        write!(converted_out, "{}", Grammar::write_rules(&rules)).unwrap();

        let rules = chomsky_unit(rules);

        // log
        let mut converted_out = std::fs::File::create(format!("chomsky_UNIT.ebnf")).unwrap();
        write!(converted_out, "{}", Grammar::write_rules(&rules)).unwrap();

        let rules = unwrap_top_level_elements(rules);
        let rules = deduplicate(rules);

        // log
        let mut converted_out = std::fs::File::create(format!("chomsky_DEDUP.ebnf")).unwrap();
        write!(converted_out, "{}", Grammar::write_rules(&rules)).unwrap();

        return Grammar {
            start_rule: old_grammar.start_rule,
            rules,
            name_generator,
        };
    }

    pub fn write(grammar: &Chomsky) -> String {
        let mut output_string = String::new();
        for (identifier, rules) in &grammar.rules {
            // output_string.push_str(&format!("(* Starts with {:?} *)\n", first_terminals));
            output_string.push_str(&format!("{:30} = ", &identifier));
            Chomsky::to_string(&rules[0].pattern, &mut output_string);
            for sub_term in &rules[1..] {
                output_string.push_str(&format!("\n{:30} | ", ""));
                Chomsky::to_string(&sub_term.pattern, &mut output_string);
            }
            output_string.push_str(";\n");
        }
        output_string
    }

    fn to_string(pattern: &ChomskyPattern, output_string: &mut String) {
        match &pattern {
            ChomskyPattern::NonTerminal(terms) => {
                output_string.push_str(&terms[0]);
                for t in &terms[1..] {
                    output_string.push_str(", ");
                    output_string.push_str(t)
                }
            },
            ChomskyPattern::Terminal(Terminal::Literal(literal)) => {
                output_string.push('"');
                output_string.push_str(literal);
                output_string.push('"');
            },
            ChomskyPattern::Terminal(Terminal::Token(i)) => {
                output_string.push_str("? ");
                output_string.push_str(i.as_str());
                output_string.push_str(" ?");
            },
            ChomskyPattern::Terminal(Terminal::EndOfFile) => output_string.push_str("EOF"),
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
}

fn to_chomsky_terms(patterns: Vec<Term>) -> Vec<ChomskyPattern> {
    let mut terminals = Vec::new();
    let mut non_terminals = Vec::new();

    for pattern in patterns {
        match pattern {
            Term::Concatenation(terms) => {
                non_terminals.push(Chomsky::unwrap_identifiers(terms));
            },
            Term::Terminal(t) => {
                terminals.push(t);
            },
            Term::Identifier(rule) if !is_transparent_rule(&rule) => {
                non_terminals.push(vec![rule]);
            },
            Term::Identifier(_) => {
                panic!("Non-transparent rename rules are not allowed");
            },
            Term::Empty => {
                panic!("Empty rules are not allowed");
            },
            Term::Alternation(_) => {
                panic!("Top-level alternations should have been removed");
            },
        }
    }

    for idx_a in 0..non_terminals.len() {
        let mut duplicate_indices = Vec::new();
        let a_terms = &non_terminals[idx_a];

        for idx_b in (idx_a + 1)..non_terminals.len() {
            let b_terms = &non_terminals[idx_b];
            if a_terms[0] == b_terms[0] {
                duplicate_indices.push(idx_b);
            }
        }

        if !duplicate_indices.is_empty() {}
    }

    // for terms in &non_terminals {
    //     for other_terms in &non_terminals {
    //         if (terms.as_ptr() != other_terms.as_ptr()) && (terms[0] == other_terms[0]) {

    //         }
    //     }
    // }

    // this also sorts the terminals to be first
    terminals
        .into_iter()
        .map(ChomskyPattern::Terminal)
        .chain(non_terminals.into_iter().map(ChomskyPattern::NonTerminal))
        .collect()
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
                    },
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
                },
                Term::Concatenation(terms) => {
                    let inlined_terms = inline_nullable(terms, &nullable_rules, &null_rules);
                    for mut concatenation in inlined_terms {
                        if concatenation.len() > 1 {
                            new_terms.push(Term::Concatenation(concatenation));
                        } else if concatenation.len() == 1 {
                            // concatenation of 1 term: unwrap to the term itself
                            new_terms.push(concatenation.pop().unwrap());
                        }
                    }
                },
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

    let last = concatenation.pop().unwrap();

    // remove_nullable will return all possible prefixes
    let mut new_terms = inline_nullable(concatenation, nullable_rules, null_rules);

    match last {
        Term::Empty => {
            // all empty terms are removed from the grammar
            // we do not change the prefix
        },
        Term::Identifier(other_rule) if null_rules.contains(&other_rule) => {
            // this identifier is of an empty rule. Effectively, this is thus an empty term.
            // we do not change the prefix
        },
        Term::Identifier(other_rule) if nullable_rules.contains(&other_rule) => {
            // nullable term, create all prefixes with this term and all prefixes without
            let new_terms_copy = new_terms.clone();
            for t in &mut new_terms {
                t.push(Term::Identifier(other_rule.clone()))
            }
            new_terms.extend(new_terms_copy);
        },
        other => {
            // non-nullable term, append this term to all prefixes
            for t in &mut new_terms {
                t.push(other.clone())
            }
        },
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
                },
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
        Term::Concatenation(terms) => subdivide(terms, TERM_LENGTH, name_generator, other_rules),
        other => normalize_to_identifier(other, name_generator, other_rules),
    }
}

// split a concatenation of multiple terms into a series of rules with a concatenation of 2 elememnts
fn subdivide(
    mut terms: Vec<Term>,
    max_num_elements: usize,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> Term {
    assert!(!terms.is_empty());

    // end-condition
    if terms.len() <= max_num_elements {
        return Term::Concatenation(
            terms
                .into_iter()
                .map(|t| normalize_to_identifier(t, name_generator, other_rules))
                .collect(),
        );
    }

    let new_rule_name = name_generator.generate_rule_name();
    let remaining_terms = terms.split_off(max_num_elements - 1);

    let subdivided_terms: Vec<_> = terms
        .into_iter()
        .map(|t| normalize_to_identifier(t, name_generator, other_rules))
        // append the rule containing the remaining therms
        .chain(std::iter::once(Term::Identifier(new_rule_name.clone())))
        .collect();

    let subdivided_term = subdivide(
        remaining_terms,
        max_num_elements,
        name_generator,
        other_rules,
    );

    other_rules.insert(new_rule_name, vec![subdivided_term]);

    Term::Concatenation(subdivided_terms)
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
        },
        Term::Concatenation(terms) => {
            new_terms.push(subdivide(terms, TERM_LENGTH, name_generator, other_rules))
        },
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
                },
                Term::Alternation(sub_terms) => {
                    println!("Unwrap Alternation");
                    new_terms.extend(sub_terms);
                },

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
            grammar_util::transform_bottom_up(term, &|t| rename_or_this(t, &renames))
        }

        assert!(!terms.is_empty());
        new_rules.insert(rule_id, terms);
    }

    new_rules
}

fn rename_or_this(terminal: &mut Term, renames: &HashMap<RuleId, RuleId>) {
    match terminal {
        Term::Identifier(id) if renames.contains_key(id) => {
            *id = renames.get(id).unwrap().to_owned();
            // recursively resolve renames, there may be chains of renames in the hashmap
            rename_or_this(terminal, renames);
        },
        _ => {},
    }
}
