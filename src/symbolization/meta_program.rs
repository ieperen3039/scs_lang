use crate::parsing::rule_nodes::RuleNode;
use std::path::PathBuf;

pub fn extract_includes(tree: &RuleNode<'_, '_>, file_path : &std::path::Path) -> Vec<PathBuf> {
    assert_eq!(tree.rule_name, "scs_program");
    tree.sub_rules.iter()
        .filter(|r| r.rule_name == "include_declaration")
        .flat_map(|r| &r.sub_rules)
        .filter(|r| r.rule_name == "include_file")
        .map(|r| to_path(file_path, &r.sub_rules))
        .collect()
}

fn to_path(file_path: &std::path::Path, sub_rules: &Vec<RuleNode<'_, '_>>) -> PathBuf {
    let mut new_path = std::path::PathBuf::from(file_path);
    for ele in sub_rules {
        new_path = new_path.join(ele.tokens);
    }
    new_path
}
