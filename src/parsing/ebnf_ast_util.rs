use std::fmt::Write;

use super::ebnf_ast::*;

pub fn ebnf_ast_write(ast: &EbnfAst) -> String {
    let mut output_string = String::new();
    for rule in &ast.rules {
        output_string.push_str(&format!("{:30} = ", &rule.identifier));
        if let Term::Alternation(terms) = &rule.pattern {
            ebnf_ast_term_to_string(&terms[0], &mut output_string);
            for sub_term in &terms[1..] {
                output_string.push_str(&format!("\n{:30} | ", ""));
                ebnf_ast_term_to_string(sub_term, &mut output_string);
            }
        } else {
            ebnf_ast_term_to_string(&rule.pattern, &mut output_string);
        }
        output_string.push_str(";\n");
    }
    output_string
}

fn ebnf_ast_term_to_string(term: &Term, target: &mut String) {
    match term {
        Term::Optional(t) => {
            target.push_str("[ ");
            ebnf_ast_term_to_string(t, target);
            target.push_str(" ]");
        }
        Term::Repetition(t) => {
            target.push_str("{ ");
            ebnf_ast_term_to_string(t, target);
            target.push_str(" }");
        }
        Term::Concatenation(terms) => {
            target.push_str("( ");
            ebnf_ast_term_to_string(&terms[0], target);
            for t in &terms[1..] {
                target.push_str(", ");
                ebnf_ast_term_to_string(t, target);
            }
            target.push_str(" )");
        }
        Term::Alternation(terms) => {
            target.push_str("( ");
            ebnf_ast_term_to_string(&terms[0], target);
            for t in &terms[1..] {
                target.push_str(" | ");
                ebnf_ast_term_to_string(t, target);
            }
            target.push_str(" )");
        }
        Term::Identifier(i) => target.push_str(i),
        Term::Literal(i) => {
            target.push('"');
            target.push_str(i);
            target.push('"');
        }
        Term::Token(i) => {
            target.push_str("? ");
            target.push_str(i.str());
            target.push_str(" ?");
        }
        Term::Empty => target.push_str("? EMPTY ?"),
    };
}
