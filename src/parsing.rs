pub mod ebnf_ast;
pub mod ebnf_parser;
pub mod parser;
pub mod rule_nodes;
pub mod lexer;
pub mod token;
pub mod ebnf_ast_util;
pub mod naive_recursive_descent_parser;
pub mod chomsky_parser;
pub mod left_left_parser;

pub const FIRST_CUSTOM_TYPE_ID: u32 = 4;

#[cfg(test)]
mod tests_ebnf;

#[cfg(test)]
mod tests_lexer;

#[cfg(test)]
mod tests_naive;

#[cfg(test)]
mod tests_chomsky;

#[cfg(test)]
mod tests_left_left;