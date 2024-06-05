use crate::{
    built_in::{functions::{math::*, InternalFunction}, primitives},
    symbolization::{ast, function_collector::FunctionCollector},
};

#[test]
fn interp_math() {
    let definition = include_str!("../../doc/faux_script.ebnf");
    // it should be noted that floating point arithmatic is (probably) not going to be supported
    let program = r#"
        invsqrt(10)
            less_than(1)
            = return;

        fn invsqrt(int n) : int {
            n
                sqrt() 
                (n_sq) {
                    1 div(b=n_sq)
                }
        }
    "#;
}
