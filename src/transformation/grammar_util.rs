use crate::parsing::token::Token;

use super::grammar::*;

pub fn transform_top_down<Transformation>(term: &mut Term, transformation: &Transformation)
where
    Transformation: Fn(&mut Term),
{
    transformation(term);

    match term {
        Term::Concatenation(terms) | Term::Alternation(terms) => {
            for t in terms {
                transform_top_down(t, transformation);
            }
        },
        _ => {},
    };
}

pub fn transform_bottom_up<TermReader>(term: &mut Term, function: &TermReader)
where
    TermReader: Fn(&mut Term),
{
    match term {
        Term::Concatenation(terms) | Term::Alternation(terms) => {
            for t in terms {
                transform_bottom_up(t, function);
            }
        },
        _ => {}
    };

    function(term);
}

pub fn iterate_recursively<TermReader>(term: &Term, function: &mut TermReader)
where
    TermReader: FnMut(&Term),
{
    match term {
        Term::Concatenation(terms) | Term::Alternation(terms) => {
            for t in terms {
                iterate_recursively(t, function);
            }
        },
        _ => {}
    };

    function(term);
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
            Term::Terminal(Terminal::EndOfFile) => target.push_str("EOF"),
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
            Terminal::EndOfFile => "END_OF_FILE"
        }
    }
}