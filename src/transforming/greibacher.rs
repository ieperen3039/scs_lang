use std::collections::VecDeque;

use super::{grammar::*, rule_name_generator::RuleNameGenerator};

pub fn convert(ast: Grammar) -> Grammar {
    let mut name_generator = ast.name_generator;

    let primary_rule_id = ast
        .rules
        .get(0)
        .expect("Grammar must have rules")
        .identifier
        .clone();
    
    let mut rules = VecDeque::from(ast.rules);

    let mut max_iterations = 65535;

    loop {
        let num_old_rules = rules.len();
        let mut is_done = true;

        for _ in 0..num_old_rules {
            let mut rule = rules.pop_front().unwrap();
            let new_pattern = normalize_top_level(rule.pattern.clone(), &mut name_generator, &mut rules);

            if rule.pattern != new_pattern {
                is_done = false;
                rule.pattern = new_pattern;
            }

            rules.push_back(rule);
            max_iterations -= 1;
        }

        if is_done || max_iterations <= 0 {
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

fn normalize_top_level(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut VecDeque<Rule>,
) -> Term {
    match term {
        Term::Alternation(terms) => Term::Alternation(
            terms
                .into_iter()
                .map(|t| normalize(t, name_generator, other_rules))
                .collect(),
        ),
        other => normalize(other, name_generator, other_rules),
    }
}

fn normalize(
    term: Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut VecDeque<Rule>,
) -> Term {
    match term {
        Term::Concatenation(mut terms) => {
            terms[0] = normalize(terms[0].clone(), name_generator, other_rules);
            Term::Concatenation(terms)
        }
        Term::Alternation(terms) => {
            let new_rule_name = name_generator.generate_rule_name();
            other_rules.push_back(Rule {
                identifier: new_rule_name.clone(),
                pattern: Term::Alternation(terms),
            });
            Term::Identifier(new_rule_name)
        }
        Term::Identifier(id) => {
            // inline this rule if it is not an alternation
            let referenced_rule = other_rules
                .iter()
                .find(|r| &r.identifier == &id)
                .expect(&id);

            match referenced_rule.pattern {
                Term::Alternation(_) => Term::Identifier(id),
                _ => referenced_rule.pattern.clone(),
            }
        }
        other => other,
    }
}