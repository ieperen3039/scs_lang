use std::hash::Hash;

use simple_error::{SimpleError, SimpleResult};

use crate::symbolization::ast::Identifier;

use super::token::Token;

// the entire resulting syntax tree consists of these nodes
#[derive(Eq, Clone)]
pub struct RuleNode<'prog, 'bnf> {
    pub rule_name: &'bnf str,
    pub tokens: &'prog [Token<'prog>],
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
            f.write_fmt(format_args!(
                "{{{}, \"{:?}\"}}",
                self.rule_name, self.tokens
            ))
        } else {
            f.write_fmt(format_args!("{{{}, {:?}}}", self.rule_name, self.sub_rules))
        }
    }
}

impl<'prog, 'bnf> RuleNode<'prog, 'bnf> {
    pub fn as_identifier(&self) -> Identifier {
        Identifier::from(self.tokens_as_string())
    }

    pub fn tokens_as_string(&self) -> String {
        self.tokens
            .iter()
            .map(|t| t.slice)
            .fold(String::new(), |s, t| s + t)
    }

    // returns true if this rule node is syntactically equivalent.
    // this check is more expensive than eq
    #[allow(dead_code)]
    pub fn is_similar_to(&self, other: &Self) -> bool {
        if self.rule_name != other.rule_name {
            return false;
        }

        // self.sub_rules ~= other.sub_rules
        let mut this_sub_rules = self.sub_rules.iter();
        let mut other_sub_rules = other.sub_rules.iter();

        match (this_sub_rules.next(), other_sub_rules.next()) {
            (None, None) => {},
            (Some(this_sub_rule), Some(other_sub_rule)) => {
                if !this_sub_rule.is_similar_to(other_sub_rule) {
                    return false
                }
            }
            _ => return false,
        };

        // if the sub rules are similar, then our tokens are similar as well
        // this simplifies writing tests
        if self.sub_rules.is_empty() {
            // self.tokens ~= other.tokens
            let mut this_tokens = self.tokens.iter();
            let mut other_tokens = other.tokens.iter();
    
            match (this_tokens.next(), other_tokens.next()) {
                (None, None) => {},
                (Some(this_token), Some(other_token)) => {
                    if this_token.class != other_token.class || this_token.slice != other_token.slice {
                        return false
                    }
                }
                _ => return false,
            };
        }

        true
    }

    pub fn expect_node<'r>(&'r self, expected: &str) -> SimpleResult<&'r RuleNode> {
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

