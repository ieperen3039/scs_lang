use super::grammar::*;

pub fn grammar_write(ast: &Grammar) -> String {
    let mut output_string = String::new();
    for rule in &ast.rules {
        output_string.push_str(&format!("{:30} = ", &rule.identifier));
        if let Term::Alternation(terms) = &rule.pattern {
            grammar_to_string(&terms[0], &mut output_string);
            for sub_term in &terms[1..] {
                output_string.push_str(&format!("\n{:30} | ", ""));
                grammar_to_string(sub_term, &mut output_string);
            }
        } else {
            grammar_to_string(&rule.pattern, &mut output_string);
        }
        output_string.push_str(";\n");
    }
    output_string
}

pub fn transform_terminals<Transformation>(term: &mut Term, transformation: &Transformation)
where
    Transformation: Fn(Term) -> Term,
{
    match term {
        Term::Concatenation(terms) => 
        {
            for t in terms {
                transform_terminals(t, transformation);
            }
        }
        Term::Alternation(terms) => {
            for t in terms {
                transform_terminals(t, transformation);
            }
        }
        _ => {
            *term = transformation(term.clone())
        }
    };
}

pub fn apply_recursively<TermReader>(term: &Term, function: &TermReader)
where
    TermReader: Fn(&Term),
{
    function(term);

    match term {
        Term::Concatenation(terms) => {
            for t in terms {
                apply_recursively(t, function);
            }
        }
        Term::Alternation(terms) => {
            for t in terms {
                apply_recursively(t, function);
            }
        }
        _ => {}
    };
}

fn grammar_to_string(term: &Term, target: &mut String) {
    match term {
        Term::Concatenation(terms) => {
            target.push_str("( ");
            grammar_to_string(&terms[0], target);
            for t in &terms[1..] {
                target.push_str(", ");
                grammar_to_string(t, target);
            }
            target.push_str(" )");
        }
        Term::Alternation(terms) => {
            target.push_str("( ");
            grammar_to_string(&terms[0], target);
            for t in &terms[1..] {
                target.push_str(" | ");
                grammar_to_string(t, target);
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
            target.push_str(i.str());
            target.push_str(" ?");
        }
        Term::Terminal(Terminal::Empty) => {
            target.push_str("? EMPTY ?");
        }
    };
}
