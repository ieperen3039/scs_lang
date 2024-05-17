use std::fmt::Debug;
use crate::symbolization::ast::*;

use super::meta_structures::Value;

pub struct Variable {
    pub var_type: TypeRef,
    pub name: Identifier,
    pub value: Value
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionBody").finish()
    }
}