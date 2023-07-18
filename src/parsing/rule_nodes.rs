use std::{hash::Hash, rc::Rc};

use simple_error::SimpleError;

// the entire resulting syntax tree consists of these nodes
#[derive(Eq, Clone)]
pub struct RuleNode<'prog, 'bnf> {
    pub rule_name: &'bnf str,
    pub tokens: &'prog str,
    pub sub_rules: Vec<RuleNode<'prog, 'bnf>>,
}

impl<'prog, 'bnf> Hash for RuleNode<'prog, 'bnf> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.rule_name.hash(state);
        self.sub_rules.hash(state);

        if self.sub_rules.is_empty() {
            self.tokens.hash(state);
        }
    }
}

impl<'prog, 'bnf> PartialEq for RuleNode<'prog, 'bnf> {
    fn eq(&self, other: &Self) -> bool {
        if self.sub_rules.is_empty() {
            other.sub_rules.is_empty()
                && self.rule_name == other.rule_name
                && self.tokens == other.tokens
        } else {
            self.rule_name == other.rule_name && self.sub_rules == other.sub_rules
        }
    }
}

impl<'prog, 'bnf> std::fmt::Debug for RuleNode<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sub_rules.is_empty() {
            f.write_fmt(format_args!("{{{}, \"{}\"}}", self.rule_name, self.tokens))
        } else {
            f.write_fmt(format_args!("{{{}, {:?}}}", self.rule_name, self.sub_rules))
        }
    }
}

impl<'prog, 'bnf> RuleNode<'prog, 'bnf> {
    pub fn as_identifier(&self) -> Rc<str> {
        Rc::from(self.tokens)
    }

    pub fn expect_node<'r>(
        &'r self,
        expected: &str,
    ) -> Result<&'r RuleNode, SimpleError> {
        self.find_node(expected)
            .ok_or_else(|| SimpleError::new(format!("Expected node {expected}")))
    }

    pub fn find_node<'r>(&'r self, expected: &str) -> Option<&'r RuleNode> {
        self.sub_rules
            .iter()
            .find(|rule| rule.rule_name == expected)
    }

    pub fn find_nodes<'r>(&'r self, expected: &str) -> Vec<&'r RuleNode> {
        self.sub_rules
            .iter()
            .filter(|rule| rule.rule_name == expected)
            .collect()
    }
}
