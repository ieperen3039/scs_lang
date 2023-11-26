use std::collections::VecDeque;

use crate::parsing::ebnf_ast::*;

struct GreibachNormalFormConverter {
    next_id: u32,
}

pub fn convert_to_normal_form(ast: EbnfAst) -> EbnfAst {
    let mut converter = GreibachNormalFormConverter { next_id: 0 };

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
            let new_pattern = converter.normalize_top_level(rule.pattern.clone(), &mut rules);

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

            return EbnfAst {
                rules: Vec::from(rules),
                ignore_rule: ast.ignore_rule,
            };
        }
    }
}

impl GreibachNormalFormConverter {
    pub fn normalize_top_level(&mut self, term: Term, other_rules: &mut VecDeque<Rule>) -> Term {
        match term {
            Term::Alternation(terms) => Term::Alternation(
                terms
                    .into_iter()
                    .map(|t| self.normalize(t, other_rules))
                    .collect(),
            ),
            other => self.normalize(other, other_rules),
        }
    }

    pub fn normalize(&mut self, term: Term, other_rules: &mut VecDeque<Rule>) -> Term {
        match term {
            Term::Optional(t) => Term::Optional(Box::new(self.normalize(*t, other_rules))),
            Term::Repetition(t) => Term::Repetition(Box::new(self.normalize(*t, other_rules))),
            Term::Concatenation(mut terms) => {
                terms[0] = self.normalize(terms[0].clone(), other_rules);
                Term::Concatenation(terms)
            }
            Term::Alternation(terms) => {
                let new_rule_name = self.generate_rule_name();
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
                // if referenced_rule.identifier.starts_with('_') {
                //     referenced_rule.pattern.clone()
                // } else {
                //     Term::Identifier(id)
                // }
            }
            other => other,
        }
    }

    fn find_or_create_rule(
        &mut self,
        with_pattern: Term,
        other_rules: &mut VecDeque<Rule>,
    ) -> String {
        for ele in &*other_rules {
            if ele.pattern == with_pattern {
                return ele.identifier.clone();
            }
        }

        // no such rule exists, make a new one
        let new_rule_name = self.generate_rule_name();
        other_rules.push_back(Rule {
            identifier: new_rule_name.clone(),
            pattern: with_pattern,
        });
        new_rule_name
    }

    fn generate_rule_name(&mut self) -> String {
        let id = self.next_id;
        self.next_id = id + 1;
        format!("__{:03}", id)
    }
}
