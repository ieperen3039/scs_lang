use super::grammar::RuleId;


#[derive(Debug, Clone)]
pub struct RuleNameGenerator {
    next_id: u32,
}

impl RuleNameGenerator {
    pub fn new() -> RuleNameGenerator {
        RuleNameGenerator { next_id: 0 }
    }

    pub fn generate_rule_name(&mut self) -> RuleId {
        let id = self.next_id;
        self.next_id = id + 1;
        RuleId::from(format!("__{:03}", id))
    }
}