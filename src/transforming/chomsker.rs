use std::collections::{HashMap, HashSet};
use std::io::Write;

use simple_error::SimpleError;

use super::{grammar::*, grammar_util, rule_name_generator::RuleNameGenerator};

pub enum ChomskyPattern {
    NonTerminal(Vec<String>),
    Terminal(Terminal),
}
pub struct ChomskyRule {
    pub first_terminal: Terminal,
    pub pattern: ChomskyPattern,
}

pub struct Chomsky {
    pub start: String,
    pub rules: HashMap<String, ChomskyRule>,
}

impl Chomsky {
    pub fn from(grammar: Grammar) -> Result<Chomsky, SimpleError> {
        let normal_grammar = convert_to_normal_form(grammar);

        let start = normal_grammar.start_rule;

        // first collect all terminal and non-terminal rules
        let mut non_terminal_rules = HashMap::new();
        let mut terminal_rules = HashMap::new();
        for (identifier, patterns) in normal_grammar.rules {
            for pattern in patterns {
                match pattern {
                    Term::Concatenation(terms) => {
                        non_terminal_rules.insert(identifier.to_owned(), create_pattern(terms)?);
                    }
                    Term::Alternation(a_terms) => {
                        for a_term in a_terms {
                            if let Term::Concatenation(c_terms) = a_term {
                                non_terminal_rules
                                    .insert(identifier.to_owned(), create_pattern(c_terms)?);
                            } else {
                                return Err(SimpleError::new("this aint chomsky"));
                            }
                        }
                    }
                    Term::Terminal(t) => {
                        terminal_rules.insert(identifier.to_owned(), t);
                    }
                    Term::Identifier(_) | Term::Empty => return Err(SimpleError::new("this aint chomsky")),
                }
            }
        }

        // now calculate for each rule the first terminal
        let mut rules = HashMap::new();
        for (id, concatenation) in &non_terminal_rules {
            let mut other = concatenation
                .first()
                .ok_or_else(|| SimpleError::new(format!("{id} has no rules")))?;
            loop {
                match non_terminal_rules.get(other) {
                    Some(referred) => {
                        other = referred
                            .first()
                            .ok_or_else(|| SimpleError::new(format!("{other} has no rules")))?
                    }
                    None => break,
                }
            }
            let terminal = terminal_rules
                .get(other)
                .ok_or_else(|| SimpleError::new(format!("rule {other} not found")))?;

            rules.insert(
                id.clone(),
                ChomskyRule {
                    first_terminal: terminal.to_owned(),
                    pattern: ChomskyPattern::NonTerminal(concatenation.clone()),
                },
            );
        }

        for (id, t) in terminal_rules {
            rules.insert(
                id,
                ChomskyRule {
                    first_terminal: t.clone(),
                    pattern: ChomskyPattern::Terminal(t),
                },
            );
        }

        Ok(Chomsky { start, rules })
    }
}

pub fn chomsky_write(chomsky_grammar: &Chomsky) -> String {
    let mut target = String::new();
    let chomsky_rule = chomsky_grammar
        .rules
        .get(&chomsky_grammar.start)
        .expect("Start must point to an existing rule");

    match &chomsky_rule.pattern {
        ChomskyPattern::NonTerminal(terms) => {
            target.push_str(&terms[0]);
            for t in &terms[1..] {
                target.push_str(", ");
                target.push_str(t)
            }
        }
        ChomskyPattern::Terminal(Terminal::Literal(i)) => {
            target.push('"');
            target.push_str(i);
            target.push('"');
        }
        ChomskyPattern::Terminal(Terminal::Token(i)) => {
            target.push_str("? ");
            target.push_str(i.str());
            target.push_str(" ?");
        }
    }

    target
}

fn create_pattern(terms: Vec<Term>) -> Result<Vec<String>, SimpleError> {
    let mut pattern = Vec::new();

    for sub_term in terms {
        if let Term::Identifier(id) = sub_term {
            pattern.push(id);
        } else {
            return Err(SimpleError::new("this aint chomsky"));
        }
    }
    Ok(pattern)
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

    return Grammar {
        start_rule: old_grammar.start_rule,
        rules,
        name_generator,
    };
}

// find and remove all rules that are renames
// also inline all top-level alterations
fn chomsky_unit(rules: HashMap<String, Vec<Term>>) -> HashMap<String, Vec<Term>> {
    let mut renames = HashMap::new();

    let mut new_rules = HashMap::new();
    for (rule_id, terms) in rules {
        let mut new_terms = Vec::new();
        for term in terms {
            match term {
                Term::Identifier(b) if b.starts_with('_') => {
                    renames.insert(b, rule_id.clone());
                }
                Term::Alternation(terms) => {
                    new_terms.extend(terms);
                }
                other => new_terms.push(other),
            }
        }
        new_rules.insert(rule_id, new_terms);
    }

    // apply renames
    let mut renamed_rules = HashMap::new();
    for (identifier, patterns) in &new_rules {
        if let Some(rename_target) = renames.get(identifier) {
            renamed_rules.insert(rename_target.clone(), patterns.clone());
        }
    }

    new_rules.extend(renamed_rules);

    new_rules
}

fn chomsky_del(rules: HashMap<String, Vec<Term>>) -> HashMap<String, Vec<Term>> {
    let mut nullable_rules = HashSet::new();
    let mut null_rules = HashSet::new();

    for (rule_id, terms) in &rules {
        if terms.iter().all(|t| { matches!(t, Term::Empty)}) {
            null_rules.insert(rule_id.to_owned());
        }
        else if terms.iter().any(|t| is_nullable(t)) {
            nullable_rules.insert(rule_id.to_owned());
        }
    }

    let mut new_rules = HashMap::new();
    for (rule_id, terms) in rules {
        let mut new_terms = Vec::new();
        for term in terms.clone() {
            match term {
                Term::Empty => {
                    // all empty terms are removed from the grammar
                }
                Term::Concatenation(terms) => {
                    let inlined_terms = inline_nullable(terms, &nullable_rules, &null_rules);
                    for concatenation in inlined_terms {
                        assert!(!concatenation.is_empty());
                        new_terms.push(Term::Concatenation(concatenation));
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
    nullable_rules: &HashSet<String>,
    null_rules: &HashSet<String>,
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

fn chomsky_bin_term(
    rules: HashMap<String, Vec<Term>>,
    name_generator: &mut RuleNameGenerator,
) -> HashMap<String, Vec<Term>> {
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
    other_rules: &mut HashMap<String, Vec<Term>>,
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
    other_rules: &mut HashMap<String, Vec<Term>>,
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
    other_rules: &mut HashMap<String, Vec<Term>>,
) -> Term {
    let mut new_terms = Vec::new();

    match term {
        Term::Identifier(_) | Term::Empty => return term,
        Term::Terminal(_) => new_terms.push(term),
        Term::Alternation(sub_terms) => {
            // if the new rule would be an alteration, unwrap the alteration
            for sub_term in sub_terms {
                new_terms.push(normalize_to_concatenation(sub_term, name_generator, other_rules))
            }
        }
        Term::Concatenation(terms) => new_terms.push(subdivide_to_two(&terms, name_generator, other_rules)),
    };

    assert!(!new_terms.is_empty());
    let new_rule_name = name_generator.generate_rule_name();
    other_rules.insert(new_rule_name.clone(), new_terms);
    Term::Identifier(new_rule_name)
}
