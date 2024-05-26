use std::io::Write;

use crate::parsing::ebnf_ast;

struct XmlParseLogger {
    xml_out: std::fs::File,
    parse_stack: Vec<String>,
    printed_depth: usize,
}

impl XmlParseLogger {
    fn log(&mut self, string: String) {
        // write all rule tags that we haven't written yet
        for rule_name in &self.parse_stack[self.printed_depth..] {
            let _ = write!(self.xml_out, "<{}>", rule_name);
        }
        self.printed_depth = self.parse_stack.len();

        let _ = write!(self.xml_out, "{string}");
    }

    fn log_pop(&mut self, rule: &ebnf_ast::Rule) {
        if self.printed_depth > self.parse_stack.len() {
            // write all closing rule tags that we haven't written yet
            let _ = write!(self.xml_out, "</{}>", rule.identifier);
            self.printed_depth -= 1;
        }

        self.parse_stack.pop();
    }

    fn log_push(&mut self, rule: &ebnf_ast::Rule) {
        self.parse_stack.push(rule.identifier.clone());
    }
}
