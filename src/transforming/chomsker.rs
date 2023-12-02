use std::collections::{HashMap, VecDeque};
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

        let mut converted_out = std::fs::File::create("chomsky_.ebnf").unwrap();
        write!(converted_out, "{}\n\n", grammar_util::grammar_write(&normal_grammar)).unwrap();

        let start = normal_grammar
            .rules
            .first()
            .map(|r| r.identifier.to_owned())
            .expect("No rules in grammar");

        let mut non_terminal_rules = HashMap::new();
        let mut terminal_rules = HashMap::new();
        for r in normal_grammar.rules {
            let Rule {
                identifier,
                pattern,
            } = r;
            match pattern {
                Term::Concatenation(terms) => {
                    non_terminal_rules.insert(identifier, create_pattern(terms)?);
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
                    terminal_rules.insert(identifier, t);
                }
                Term::Identifier(_) => return Err(SimpleError::new("this aint chomsky")),
            }
        }

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
        ChomskyPattern::Terminal(Terminal::Empty) => {
            target.push_str("? EMPTY ?");
        }
    }

    todo!()
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

    let primary_rule_id = old_grammar
        .rules
        .get(0)
        .expect("Grammar must have rules")
        .identifier
        .clone();
    let mut rules = VecDeque::from(old_grammar.rules);

    loop {
        let num_old_rules = rules.len();

        for _ in 0..num_old_rules {
            let mut rule = rules.pop_front().unwrap();
            rule.pattern = normalize_to_alteration(rule.pattern, &mut name_generator, &mut rules);
            rules.push_back(rule);
        }

        // all rules that are keys of this map will be inlined and removed
        let mut renames = HashMap::new();
        // pairs in this Vec will be swapped, before applying the inverse rename
        let mut swaps = Vec::new();

        // find all rules that are renames
        for r in &rules {
            if let Rule {
                identifier: a,
                pattern: Term::Identifier(b),
            } = r
            {
                if a.starts_with('_') {
                    renames.insert(a.to_owned(), b.to_owned());
                } else if b.starts_with('_') {
                    // rules that are not transparent are always retained
                    swaps.push((a.to_owned(), b.to_owned()));
                }
            }
        }

        for (a, b) in swaps {
            let mut a_rule = rules
                .remove(rules.iter().position(|r| r.identifier == a).expect(&a))
                .unwrap();
            let b_rule = rules
                .remove(rules.iter().position(|r| r.identifier == b).expect(&b))
                .unwrap();

            a_rule.pattern = b_rule.pattern;
            rules.push_back(a_rule);

            // schedule the inverse rename
            renames.insert(b, a);
        }

        // apply renames
        for r in &mut rules {
            grammar_util::transform_terminals(&mut r.pattern, &|t| match t {
                Term::Identifier(id) => {
                    Term::Identifier(renames.get(&id).map(|t| t.to_owned()).unwrap_or(id))
                }
                other => other,
            });
        }

        rules.retain(|r| !renames.contains_key(&r.identifier));

        if rules.len() == num_old_rules {
            let old_primary_loc = rules.iter().position(|r| &r.identifier == &primary_rule_id);
            let old_primary = rules.remove(old_primary_loc.unwrap()).unwrap();
            rules.push_front(old_primary);

            return Grammar {
                rules: Vec::from(rules),
                name_generator,
            };
        }
    }
}

// normalize to an alteration of concatenation of identifiers
fn normalize_to_alteration(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut VecDeque<Rule>,
) -> Term {
    match term {
        Term::Alternation(terms) => Term::Alternation(
            terms
                .into_iter()
                .map(|t| normalize_to_concatenation(t, name_generator, other_rules))
                .collect(),
        ),
        Term::Terminal(_) => term,
        other => normalize_to_concatenation(other, name_generator, other_rules),
    }
}

// normalize to a concatenations of identifiers
fn normalize_to_concatenation(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut VecDeque<Rule>,
) -> Term {
    match term {
        Term::Concatenation(terms) => Term::Concatenation(
            terms
                .into_iter()
                .map(|t| normalize_to_identifier(t, name_generator, other_rules))
                .collect(),
        ),
        other => normalize_to_identifier(other, name_generator, other_rules),
    }
}

// if this is not an identifier, extract to a new rule
fn normalize_to_identifier(
    t: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut VecDeque<Rule>,
) -> Term {
    if let Term::Identifier(_) = &t {
        return t;
    }

    let normalized_sub_term = normalize_to_alteration(t, name_generator, other_rules);
    let new_rule = find_or_create_rule(normalized_sub_term, name_generator, other_rules);
    Term::Identifier(new_rule)
}

fn find_or_create_rule(
    with_pattern: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut VecDeque<Rule>,
) -> String {
    for ele in &*other_rules {
        if ele.pattern == with_pattern {
            return ele.identifier.clone();
        }
    }

    // no such rule exists, make a new one
    let new_rule_name = name_generator.generate_rule_name();
    other_rules.push_back(Rule {
        identifier: new_rule_name.clone(),
        pattern: with_pattern,
    });
    new_rule_name
}
