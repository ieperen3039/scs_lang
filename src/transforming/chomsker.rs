use std::collections::{HashMap, VecDeque};

use simple_error::SimpleError;

use super::{grammar::*, grammar_util, rule_name_generator::RuleNameGenerator};

enum ChomskyPattern {
    NonTerminal(Vec<String>),
    Terminal(Terminal)
}
struct ChomskyRule {
    first_terminal : Terminal,
    pattern : ChomskyPattern
}

struct Chomsky {
    start : String,
    rules : HashMap<String, ChomskyRule>
}

impl Chomsky {
    pub fn from(grammar : Grammar) -> Result<Chomsky, SimpleError> {
        let normal_grammar = convert_to_normal_form(grammar);
        let start = normal_grammar.rules.first().map(|r| r.identifier.to_owned()).expect("No rules in grammar");

        let mut non_terminal_rules = HashMap::new();
        let mut terminal_rules = Vec::new();
        for r in normal_grammar.rules {
            let Rule { identifier, pattern } = r;
            match pattern {
                Term::Concatenation(terms) => {
                    let mut pattern = Vec::new();
                    for sub_term in terms {
                        if let Term::Identifier(id) = sub_term {
                            pattern.push(id);
                        }
                    }
                    non_terminal_rules.insert(identifier, pattern);
                },
                Term::Alternation(a_terms) => {
                    for a_term in a_terms {
                        if let Term::Concatenation(c_terms) = a_term {
                            let mut pattern = Vec::new();
                            for c_term in c_terms {
                                if let Term::Identifier(id) = c_term {
                                    pattern.push(id);
                                }
                            }
                            non_terminal_rules.insert(identifier.to_owned(), pattern);
                        }
                    }
                },
                Term::Identifier(id) => terminal_rules.push(id),
                _ => return Err(SimpleError::new("this aint chomsky")),
            }
        }

        let rules = HashMap::new();

        todo!();

        Ok(Chomsky { start, rules })
    }
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
            let mut a_rule = rules.remove(rules.iter().position(|r| r.identifier == a).expect(&a)).unwrap();
            let b_rule = rules.remove(rules.iter().position(|r| r.identifier == b).expect(&b)).unwrap();

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
