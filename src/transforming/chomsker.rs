use std::collections::HashMap;
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
                    Term::Identifier(_) => return Err(SimpleError::new("this aint chomsky")),
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

    let mut rules = old_grammar.rules;
    let mut num_old_rules = 0;

    let mut i = 0;

    let mut converted_out = std::fs::File::create(format!("chomsky_{i}.ebnf")).unwrap();
    write!(
        converted_out,
        "{}",
        grammar_util::grammar_write_rules(&rules)
    )
    .unwrap();

    while rules.len() != num_old_rules {
        num_old_rules = rules.len();

        let keys: Vec<String> = rules.keys().map(String::to_owned).collect();
        for rule_id in &keys {
            let terms = rules.get_mut(rule_id).unwrap();
            for term in terms {
                *term = normalize_to_alteration(*term, &mut name_generator, &mut rules);
            }
        }

        let mut renames = HashMap::new();

        // find all rules that are renames
        // also inline all top-level alterations
        for rule_id in keys {
            let terms = rules.remove(&rule_id).unwrap();
            let mut new_terms = Vec::new();
            for term in terms {
                match term {
                    Term::Identifier(b) if b.starts_with('_') => {
                        renames.insert(b, rule_id.clone());
                    }
                    Term::Alternation(terms) => {
                        new_terms.extend(terms);
                    }
                    other => new_terms.push(other)
                }
            }
            rules.insert(rule_id, new_terms);
        }

        println!("{:?}", renames);

        // apply renames
        let mut new_rules = HashMap::new();
        for (identifier, patterns) in &rules {
            if let Some(rename_target) = renames.get(identifier) {
                new_rules.insert(rename_target.clone(), patterns.clone());
            }
        }

        rules.extend(new_rules);

        // log
        i = i + 1;
        let mut converted_out = std::fs::File::create(format!("chomsky_{i}.ebnf")).unwrap();
        write!(
            converted_out,
            "{}",
            grammar_util::grammar_write_rules(&rules)
        )
        .unwrap();
    }

    return Grammar {
        start_rule: old_grammar.start_rule,
        rules,
        name_generator,
    };
}

// normalize to an alteration of concatenation of identifiers
fn normalize_to_alteration(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut HashMap<String, Vec<Term>>,
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
    other_rules: &mut HashMap<String, Vec<Term>>,
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
    other_rules: &mut HashMap<String, Vec<Term>>,
) -> Term {
    if let Term::Identifier(_) = &t {
        return t;
    }

    let normalized_sub_term = normalize_to_alteration(t, name_generator, other_rules);
    let new_rule_name = name_generator.generate_rule_name();
    other_rules.insert(new_rule_name.clone(), vec![normalized_sub_term]);
    Term::Identifier(new_rule_name)
}
