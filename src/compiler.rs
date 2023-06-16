use simple_error::SimpleError;
use crate::parsing::{ebnf_parser, parser, ebnf_ast::EbnfAst};

pub struct  ScsCompiler {
    grammar : EbnfAst,
    xml_out : Option<std::fs::File>
}

impl ScsCompiler {
    pub fn build(definition : &str, xml_out : Option<std::fs::File>) -> Option<ScsCompiler> {
        ebnf_parser::parse_ebnf(definition)
        .map_err(|err| {
            println!(
                "Error parsing EBNF definition: {}",
                ebnf_parser::error_string(&err, definition)
            );
        })
        .map( | grammar | ScsCompiler { grammar, xml_out })
        .ok()
    }

    pub fn compile<'prog, 'bnf>(&'bnf self, program : &'prog str) -> Option<parser::RuleNode<'prog, 'bnf>>{

        let parse_result = parser::parse_program_with_grammar(program, &self.grammar);

        if parse_result.is_err() {
            print!(
                "Error parsing program: \n{}",
                parse_result
                    .as_ref()
                    .unwrap_err()
                    .into_iter()
                    .map(|err| parser::error_string(&err, program) + "\n---\n\n")
                    .collect::<String>()
            );
        }

        parse_result.ok()
    }
}