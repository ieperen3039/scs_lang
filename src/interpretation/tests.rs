use crate::compiler::FauxCompiler;

use super::interpreter::Interpreter;

#[test]
fn interp_math() {
    let definition = include_str!("../../doc/faux_script.ebnf");

    // a = 3 / 2 = 1
    // b = (3 * 4) + (5 * 6) = 42
    // c = (1 + 2 + 3 + 4 + 5) * 10 = 150
    // d = (b * b) - 4 * a * c = 1754 - 600 = 1154
    // (-b + sqrt(d)) / (2*a) = (-42 + 33) / 2 = -4
    let program = r#"
        div(3, 2) 
            = a;
        mul(3, 4) 
            add(mul(5, 6)) 
            = b;
        1 
            add(2) 
            add(3) 
            add(4) 
            add(5) 
            mul(10) 
            = c;

        mul(b, b) 
            sub((){
                mul(4, a)
                    mul(c)
            })
            = d;

        sub(0, b)
            add(sqrt(d))
            div(mul(2, a))
    "#;

    let mut faux_compiler = FauxCompiler::build(&definition, None).unwrap();
    let parse_result = faux_compiler.compile_script(&program);
    
    if let Err(err) = parse_result {
        panic!("{err}");
    }
    let program = parse_result.unwrap();

    let interpreter = Interpreter::new(program, faux_compiler.get_built_in_functions());

    let interpretation_result = interpreter.execute_main();
    match interpretation_result {
        Ok(string_rep) => {
            assert_eq!(string_rep, "-4");
        },
        Err(err) => {
            panic!("{err}");
        },
    }
}
