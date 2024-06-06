use crate::{
    built_in::{functions::{math::*, InternalFunction}, primitives},
    symbolization::{ast, function_collector::FunctionCollector},
};

#[test]
fn interp_math() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    // it should be noted that floating point arithmatic is (probably) not going to be supported
    let program = r#"
        add(1, 3)
            mul(4)
            = alpha

        add(5, 6)
            mul(r=alpha)
            div(l=1) // calculates 1/x
    "#;
}
