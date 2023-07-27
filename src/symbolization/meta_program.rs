use crate::parsing::rule_nodes::RuleNode;
use std::path::PathBuf;

pub fn extract_includes(tree: &RuleNode<'_, '_>, file_path : &std::path::Path) -> Vec<PathBuf> {
    assert_eq!(tree.rule_name, "faux_program");
    tree.sub_rules.iter()
        .filter(|r| r.rule_name == "include_declaration")
        .flat_map(|r| &r.sub_rules)
        .filter(|r| r.rule_name == "include_file")
        .map(|r| std::path::PathBuf::from(r.tokens_as_string()))
        .collect()
}