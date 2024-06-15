use crate::compiler::FauxCompiler;

use super::interpreter::Interpreter;

#[test]
fn sqrt_and_nested_fn() {
    let definition = include_str!("../../doc/faux_script.ebnf");

    let program = r#"
        add(sqrt(16), 7)
    "#;

    let mut faux_compiler = FauxCompiler::build(&definition, None).unwrap();
    let parse_result = faux_compiler.compile_script(&program);
    
    if let Err(err) = parse_result {
        panic!("{err}");
    }

    let interpreter = Interpreter::new(parse_result.unwrap(), faux_compiler.get_built_in_functions());

    let interpretation_result = interpreter.execute_main();
    match interpretation_result {
        Ok(string_rep) => {
            assert_eq!(string_rep, "Int(11)");
        },
        Err(err) => {
            panic!("{err}");
        },
    }
}

#[test]
fn reuse_variable() {
    let definition = include_str!("../../doc/faux_script.ebnf");

    let program = r#"
        6 = a;
        add(a, a) add(a)
    "#;

    let mut faux_compiler = FauxCompiler::build(&definition, None).unwrap();
    let parse_result = faux_compiler.compile_script(&program);
    
    if let Err(err) = parse_result {
        panic!("{err}");
    }

    let interpreter = Interpreter::new(parse_result.unwrap(), faux_compiler.get_built_in_functions());

    let interpretation_result = interpreter.execute_main();
    match interpretation_result {
        Ok(string_rep) => {
            assert_eq!(string_rep, "Int(18)");
        },
        Err(err) => {
            panic!("{err}");
        },
    }
}

#[test]
fn left_assigned_arguments() {
    let definition = include_str!("../../doc/faux_script.ebnf");

    let program = r#"
        10 sub(2)
    "#;

    let mut faux_compiler = FauxCompiler::build(&definition, None).unwrap();
    let parse_result = faux_compiler.compile_script(&program);
    
    if let Err(err) = parse_result {
        panic!("{err}");
    }

    let interpreter = Interpreter::new(parse_result.unwrap(), faux_compiler.get_built_in_functions());

    let interpretation_result = interpreter.execute_main();
    match interpretation_result {
        Ok(string_rep) => {
            assert_eq!(string_rep, "Int(8)");
        },
        Err(err) => {
            panic!("{err}");
        },
    }
}


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

        mul(4, a) 
            mul(c) 
            = fourac;

        mul(b, b) 
            sub(fourac)
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

    let interpreter = Interpreter::new(parse_result.unwrap(), faux_compiler.get_built_in_functions());

    let interpretation_result = interpreter.execute_main();
    match interpretation_result {
        Ok(string_rep) => {
            assert_eq!(string_rep, "Int(-4)");
        },
        Err(err) => {
            panic!("{err}");
        },
    }
}
