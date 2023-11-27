
#[derive(Debug)]
pub struct RuleNameGenerator {
    next_id: u32,
}

impl RuleNameGenerator {
    pub fn new() -> RuleNameGenerator {
        RuleNameGenerator { next_id: 0 }
    }

    pub fn generate_rule_name(&mut self) -> String {
        let id = self.next_id;
        self.next_id = id + 1;
        format!("__{:03}", id)
    }
}