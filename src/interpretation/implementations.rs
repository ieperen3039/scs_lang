use std::collections::HashMap;

use crate::interpretation::meta_structures::*;

pub fn echo(mut args: HashMap<String, Value>) -> InterpResult<Value> {
    let Some(Value::String(in_string)) = args.remove("in") else {
        return Err(InterpretationError::InternalError(
            "unexpected argument value",
        ));
    };

    if args.contains_key("error") {
        eprintln!("{in_string}");
    } else {
        println!("{in_string}");
    }

    Ok(Value::String(in_string))
}
