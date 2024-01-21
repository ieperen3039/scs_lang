use std::collections::HashMap;

use crate::parsing::token::Token;

use super::grammar::*;

struct TermBottomUpIter<'c> {
    node: Option<&'c mut Term>,
    internal: Option<Box<TermBottomUpIter<'c>>>,
    sub_iter: std::slice::IterMut<'c, Term>,
}

impl<'c> TermBottomUpIter<'c> {
    fn new(target: &'c mut Term) -> TermBottomUpIter<'c> {
        match target {
            Term::Concatenation(ts) | Term::Alternation(ts) => {
                let mut sub_iter = ts.iter_mut();
                let first = sub_iter
                    .next()
                    .expect("Concatenations and Alterations may never be empty");
                let internal = Some(Box::new(TermBottomUpIter::new(first)));
                TermBottomUpIter {
                    node: Some(target),
                    internal,
                    sub_iter,
                }
            }
            Term::Identifier(_) | Term::Terminal(_) | Term::Empty => TermBottomUpIter {
                node: Some(target),
                internal: None,
                sub_iter: [].iter_mut(),
            },
        }
    }

    fn next_nested(&mut self, internal: &mut TermBottomUpIter<'c>) -> Option<&mut Term> {
        let mut next = internal.next();
        
        if let None = next {
            if let Some(next_to_iter) = self.sub_iter.next() {
                *internal = TermBottomUpIter::new(next_to_iter);
                next = internal.next();
            }
        }

        match next {
            Some(_) => next,
            None => self.next_self(),
        }
    }

    fn next_self(&mut self) -> Option<&mut Term> {
        let result = self.node;
        self.node = None;
        result
    }
}

impl<'c> Iterator for TermBottomUpIter<'c> {
    type Item = &'c mut Term;

    fn next(&mut self) -> Option<Self::Item> {
        if let None = self.node {
            return None
        }

        match &mut self.internal {
            Some(iter) => self.next_nested(iter),
            None => self.next_self(),
        }
    }
}

impl Term {
    // the returned iterator recursively returns all child nodes of this, then this itself.
    pub fn bottom_up_iter<'c>(&'c mut self) -> TermBottomUpIter<'c> {
        TermBottomUpIter::new(self)
    }
}

// removes all rules from `rules`, by renaming its occurences in other rules using the given mapping
pub fn remove_rename(rules: RuleStorage, renames: HashMap<RuleId, RuleId>) -> RuleStorage {
    let mut new_rules = HashMap::new();
    for (rule_id, mut terms) in rules {
        if renames.contains_key(&rule_id) {
            continue;
        }

        for term in &mut terms {
            for t in term.bottom_up_iter() {
                rename_identifier(t, &renames)
            }
        }

        assert!(!terms.is_empty());
        new_rules.insert(rule_id, terms);
    }

    new_rules
}

fn rename_identifier(terminal: &mut Term, renames: &HashMap<RuleId, RuleId>) {
    match terminal {
        Term::Identifier(id) if renames.contains_key(id) => {
            *id = renames.get(id).unwrap().to_owned();
            // recursively resolve renames, there may be chains of renames in the hashmap
            rename_identifier(terminal, renames)
        }
        _ => {},
    }
}

impl Grammar {
    pub fn write(grammar: &Grammar) -> String {
        Grammar::write_rules(&grammar.rules)
    }

    pub fn write_rules(rules: &RuleStorage) -> String {
        let mut output_string = String::new();
        for (identifier, terms) in rules {
            output_string.push_str(&format!("{:30} = ", &identifier));
            Grammar::to_string(&terms[0], &mut output_string);
            for sub_term in &terms[1..] {
                output_string.push_str(&format!("\n{:30} | ", ""));
                Grammar::to_string(sub_term, &mut output_string);
            }
            output_string.push_str(";\n");
        }
        output_string
    }

    fn to_string(term: &Term, target: &mut String) {
        match term {
            Term::Concatenation(terms) => {
                target.push_str("( ");
                Grammar::to_string(&terms[0], target);
                for t in &terms[1..] {
                    target.push_str(", ");
                    Grammar::to_string(t, target);
                }
                target.push_str(" )");
            }
            Term::Alternation(terms) => {
                target.push_str("( ");
                Grammar::to_string(&terms[0], target);
                for t in &terms[1..] {
                    target.push_str(" | ");
                    Grammar::to_string(t, target);
                }
                target.push_str(" )");
            }
            Term::Identifier(i) => target.push_str(i),
            Term::Terminal(Terminal::Literal(i)) => {
                target.push('"');
                target.push_str(i);
                target.push('"');
            }
            Term::Terminal(Terminal::Token(i)) => {
                target.push_str("? ");
                target.push_str(i.as_str());
                target.push_str(" ?");
            }
            Term::Empty => {
                target.push_str("? EMPTY ?");
            }
        };
    }
}

impl Terminal {
    pub fn as_str(&self) -> &str {
        match self {
            Terminal::Literal(s) => s,
            Terminal::Token(t) => t.as_str(),
        }
    }
}