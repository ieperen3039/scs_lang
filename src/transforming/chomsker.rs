use std::collections::VecDeque;

use crate::parsing::ebnf_ast::*;

struct ChomskyNormalFormConverter {
    next_id: u32,
}

pub fn convert_to_normal_form(ast: EbnfAst) -> EbnfAst {
    let mut converter = ChomskyNormalFormConverter { next_id: 0 };

    let primary_rule_id = ast.rules.get(0).expect("Grammar must have rules").identifier.clone();
    let mut rules = VecDeque::from(ast.rules);

    loop {
        let num_old_rules = rules.len();

        for _ in 0..num_old_rules {
            let mut rule = rules.pop_front().unwrap();
            rule.pattern = converter.normalize_to_alteration(rule.pattern, &mut rules);
            rules.push_back(rule);
        }

        if rules.len() == num_old_rules {
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

impl ChomskyNormalFormConverter {
    // normalize to an alteration of concatenation of identifiers
    fn normalize_to_alteration(&mut self, term: Term, other_rules: &mut VecDeque<Rule>) -> Term {
        match term {
            Term::Optional(t) => {
                let normalized_sub_term = self.normalize_to_concatenation(*t, other_rules);
                Term::Alternation(vec![normalized_sub_term, Term::Empty])
            }
            Term::Repetition(t) => {
                // normalized_sub_term is guaranteed to be a Term::Identifier
                let normalized_sub_term = self.normalize_to_identifier(*t, other_rules);
                let new_rule_name = self.generate_rule_name();
                other_rules.push_back(Rule {
                    identifier: new_rule_name.clone(),
                    pattern: Term::Alternation(vec![
                        Term::Concatenation(vec![
                            normalized_sub_term,
                            Term::Identifier(new_rule_name.clone()),
                        ]),
                        Term::Empty,
                    ]),
                });
                Term::Identifier(new_rule_name)
            }
            Term::Alternation(terms) => Term::Alternation(
                terms
                    .into_iter()
                    .map(|t| self.normalize_to_concatenation(t, other_rules))
                    .collect(),
            ),
            Term::Concatenation(terms) => {
                self.normalize_to_concatenation(Term::Concatenation(terms), other_rules)
            }
            other => other,
        }
    }

    // normalize to a concatenations of identifiers
    fn normalize_to_concatenation(&mut self, term: Term, other_rules: &mut VecDeque<Rule>) -> Term {
        match term {
            Term::Concatenation(terms) => Term::Concatenation(
                terms
                    .into_iter()
                    .map(|t| self.normalize_to_identifier(t, other_rules))
                    .collect(),
            ),
            other => self.normalize_to_identifier(other, other_rules),
        }
    }

    fn normalize_to_identifier(&mut self, t: Term, other_rules: &mut VecDeque<Rule>) -> Term {
        if let Term::Identifier(id) = &t {
            // ad-hoc rename resolution
            let rule = other_rules.iter().find(|r| &r.identifier == id);
            if let Some(Rule { pattern : Term::Identifier(new_id), ..}) = &rule {
                return Term::Identifier(new_id.clone());
            }
            return t;
        }

        let normalized_sub_term = self.normalize_to_alteration(t, other_rules);
        let new_rule = self.find_or_create_rule(normalized_sub_term, other_rules);
        Term::Identifier(new_rule)
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
