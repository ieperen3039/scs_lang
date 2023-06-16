use crate::parsing::{ebnf_parser, parser};

pub struct ScsCompiler {
    parser: parser::Parser,
}

impl ScsCompiler {
    pub fn build(definition: &str, xml_out: Option<std::fs::File>) -> Option<ScsCompiler> {
        let grammar = ebnf_parser::parse_ebnf(definition);
        if let Err(err) = grammar {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
            return None;
        }

        let parser = parser::Parser::new(grammar.unwrap(), xml_out);
        if let Err(err) = parser {
            println!("Error creating parser: {}", err);
            return None;
        }

        Some(ScsCompiler {
            parser: parser.unwrap(),
        })
    }

    pub fn compile<'prog, 'bnf>(
        &'bnf self,
        program: &'prog str,
    ) -> Option<parser::RuleNode<'prog, 'bnf>> {
        let parse_result = self.parser.parse_program(program);

        if let Err(err) = parse_result {
            print!(
                "Error parsing program: \n{}",
                err.into_iter()
                    .map(|err| parser::error_string(&err, program) + "\n---\n\n")
                    .collect::<String>()
            );
            None
        }
        else
        {
            parse_result.ok()
        }
    }
}
