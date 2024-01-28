use crate::parsing::ebnf_ast::{self, EbnfAst};

use super::grammar::{self, Grammar, RuleId, RuleStorage};
use super::rule_name_generator::RuleNameGenerator;

pub fn convert_to_grammar(ast: EbnfAst) -> Grammar {
    let mut name_generator = RuleNameGenerator::new();
    let mut rules = RuleStorage::new();

    let start_rule = ast
        .rules
        .first()
        .expect("ast must have elements")
        .identifier
        .to_owned()
        .into();

    for old_rule in ast.rules {
        let new_term = grammificate(old_rule.pattern, &mut name_generator, &mut rules);
        // make sure we don't end up with top-level alternations
        match new_term {
            grammar::Term::Alternation(terms) => {
                rules.insert(RuleId::from(old_rule.identifier), terms);
            }
            _ => {
                rules.insert(RuleId::from(old_rule.identifier), vec![new_term]);
            }
        }
    }

    Grammar {
        start_rule,
        rules,
        name_generator,
    }
}

// most of the effort lies in converting optionals and repetitions
fn grammificate(
    term: ebnf_ast::Term,
    name_generator: &mut RuleNameGenerator,
    other_rules: &mut RuleStorage,
) -> grammar::Term {
    match term {
        ebnf_ast::Term::Optional(t) => {
            let normalized_sub_term = grammificate(*t, name_generator, other_rules);
            grammar::Term::Alternation(vec![normalized_sub_term, grammar::Term::Empty])
        }
        ebnf_ast::Term::Repetition(t) => {
            // normalized_sub_term is guaranteed to be a ebnf_ast::Term::Identifier
            let normalized_sub_term = grammificate(*t, name_generator, other_rules);
            let new_rule_name = name_generator.generate_rule_name();
            other_rules.insert(
                new_rule_name.clone(),
                vec![
                    grammar::Term::Concatenation(vec![
                        normalized_sub_term,
                        grammar::Term::Identifier(new_rule_name.clone()),
                    ]),
                    grammar::Term::Empty,
                ],
            );
            grammar::Term::Identifier(new_rule_name)
        }
        ebnf_ast::Term::Alternation(terms) => grammar::Term::Alternation(
            terms
                .into_iter()
                .map(|t| grammificate(t, name_generator, other_rules))
                .collect(),
        ),
        ebnf_ast::Term::Concatenation(terms) => grammar::Term::Concatenation(
            terms
                .into_iter()
                .map(|t| grammificate(t, name_generator, other_rules))
                .collect(),
        ),
        ebnf_ast::Term::Identifier(name) => grammar::Term::Identifier(RuleId::from(name)),
        ebnf_ast::Term::Literal(lit) => grammar::Term::Terminal(grammar::Terminal::Literal(lit)),
        ebnf_ast::Term::Token(class) => grammar::Term::Terminal(grammar::Terminal::Token(class)),
    }
}
