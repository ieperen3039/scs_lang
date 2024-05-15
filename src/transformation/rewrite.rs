use std::collections::HashSet;

use crate::symbolization::ast::Identifier;

use super::{
    grammar::Term,
    grammar_util::{self, iterate_recursively},
};

pub struct RewriteRule {
    pub left: Term,
    pub right: Term,
}

impl RewriteRule {
    pub fn new(left: Term, right: Term) -> RewriteRule {
        assert!(Self::verify(&left, &right));
        RewriteRule { left, right }
    }

    fn verify(left: &Term, right: &Term) -> bool {
        let mut left_names = Vec::new();
        iterate_recursively(&left, &mut |term| {
            if let Term::Identifier(id) = term {
                left_names.push(id.clone());
            }
        });

        let mut right_names = Vec::new();
        iterate_recursively(&right, &mut |term| {
            if let Term::Identifier(id) = term {
                right_names.push(id.clone());
            }
        });

        // every left name occurs exactly once in the right names
        return left_names.iter().all(|i| right_names.iter().filter(|o| o.as_ref() == i.as_ref()).count() == 1)
        // ... and vice versa
            && right_names.iter().all(|i| left_names.iter().filter(|o| o.as_ref() == i.as_ref()).count() == 1);
        // this also implies that every name occurs only once
    }

    // if left is the left side of a transformation, returns whether left can be applied to right.
    fn corresponds(left: &Term, right: &Term) -> bool {
        match (left, right) {
            (Term::Concatenation(l), Term::Concatenation(r))
            | (Term::Alternation(l), Term::Alternation(r)) => {
                l.len() == r.len() && (0..l.len()).all(|i| Self::corresponds(&l[i], &r[i]))
            }
            (Term::Identifier(_), _) => true,
            (a, b) => a == b,
        }
    }

    // attempt to apply this rewrite rule to the given terms, or one of its sub terms, recursively.
    // the transformation is applied bottom-up: this prevents recursive (infinite) applications of the rewrite rule.
    pub fn transform(&self, term: &mut Term) {
        grammar_util::transform_bottom_up(term, &|sub_term|{
            if Self::corresponds(&self.left, sub_term) {
                // old_term := sub_term, sub_term := self.right
                let mut old_term = self.right.clone();
                std::mem::swap(&mut old_term, sub_term);
                // now replace all identifiers of sub_term with their corresponding original sub_terms.
                self.transform_recursively(&self.left, sub_term, old_term);
            }
        });
    }

    fn transform_recursively<'s>(&'s self, left: &'s Term, right: &mut Term, term: Term) {
        match (left, term) {
            (Term::Concatenation(l), Term::Concatenation(r))
            | (Term::Alternation(l), Term::Alternation(r)) => {
                let mut idx = 0;
                for elt in r {
                    self.transform_recursively(&l[idx], right, elt);
                    idx += 1;
                }
            }
            (Term::Identifier(id_l), term) => {
                // find the corresponding id in right, and paste it there
                // (there should be only one, but we cannot guarantee this to the borrow checker)
                let right_id = Self::find(id_l, right).expect("Every id in Left must occur in Right");
                *right_id = term;
            }
            (_, _) => {},
        }
    }

    fn find<'r>(identifier_with_name: &str, right: &'r mut Term) -> Option<&'r mut Term> {
        match right {
            Term::Concatenation(terms) | Term::Alternation(terms) => {
                for t in terms {
                    if let Some(e) = Self::find(identifier_with_name, t) {
                        return Some(e);
                    }
                }
            }
            Term::Identifier(id_r) if identifier_with_name == id_r.as_ref() => {
                return Some(right);
            }
            _ => {}
        }

        None
    }
}
