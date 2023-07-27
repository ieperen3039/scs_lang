use std::{hash::Hash, rc::Rc};

use simple_error::SimpleError;

// the entire resulting syntax tree consists of these nodes
#[derive(Eq, Clone)]
pub enum RuleNode<'prog, 'bnf> {
    Node{
        rule_name: &'bnf str,
        sub_rules: Vec<RuleNode<'prog, 'bnf>>,
    },
    Leaf {
        rule_name: &'bnf str,
        tokens: &'prog str,
    }
}

impl<'prog, 'bnf> Hash for RuleNode<'prog, 'bnf> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                true.hash(state);
                rule_name.hash(state);
                sub_rules.hash(state);
            },
            RuleNode::Leaf { rule_name, tokens } => {
                false.hash(state);
                rule_name.hash(state);
                tokens.hash(state);
            },
        }
    }
}

impl<'prog, 'bnf> PartialEq for RuleNode<'prog, 'bnf> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                match other {
                    RuleNode::Node { rule_name : other_rule_name, sub_rules : other_sub_rules } => {
                        return rule_name == other_rule_name && sub_rules == other_sub_rules
                    }
                    _ => false
                }
            },
            RuleNode::Leaf { rule_name, tokens } => {
                match other {
                    RuleNode::Leaf { rule_name : other_rule_name, tokens : other_tokens } => {
                        return rule_name == other_rule_name && tokens == other_tokens
                    }
                    _ => false
                }
            },
        }
    }
}

impl<'prog, 'bnf> std::fmt::Debug for RuleNode<'prog, 'bnf> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                f.write_fmt(format_args!("{{{}, {:?}}}", rule_name, sub_rules))
            },
            RuleNode::Leaf { rule_name, tokens } => {
                f.write_fmt(format_args!("{{{}, \"{}\"}}", rule_name, tokens))
            },
        }
    }
}

impl<'prog, 'bnf> RuleNode<'prog, 'bnf> {
    pub fn get_rule_name(&self) -> &'bnf str {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                rule_name
            },
            RuleNode::Leaf { rule_name, tokens } => {
                rule_name
            },
        }
    }

    pub fn as_identifier(&self) -> Rc<str> {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                Rc::from(*rule_name)
            },
            RuleNode::Leaf { rule_name, tokens } => {
                Rc::from(*tokens)
            },
        }
    }

    pub fn expect_node<'r>(
        &'r self,
        expected: &str,
    ) -> Result<&'r RuleNode, SimpleError> {
        self.find_node(expected)
            .ok_or_else(|| SimpleError::new(format!("Expected node {expected}")))
    }

    pub fn find_node<'r>(&'r self, expected: &str) -> Option<&'r RuleNode> {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                sub_rules
                .iter()
                .find(|rule| rule.get_rule_name() == expected)
            },
            RuleNode::Leaf { rule_name, tokens } => {
                None
            },
        }
    }

    pub fn find_nodes<'r>(&'r self, expected: &str) -> Vec<&'r RuleNode> {
        match self {
            RuleNode::Node { rule_name, sub_rules } => {
                sub_rules
                .iter()
                .filter(|rule| rule.get_rule_name() == expected)
                .collect()
            },
            RuleNode::Leaf { rule_name, tokens } => {
                Vec::new()
            },
        }
    }
}
