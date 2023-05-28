struct Parameter {
    name: String,
    value: String,
    is_used: bool,
}

struct Flag {
    name: String,
    is_used: bool,
}

pub struct ArgumentParser {
    parameters: Vec<Parameter>,
    flags: Vec<Flag>,
}

impl ArgumentParser {
    pub fn new() -> ArgumentParser {
        let mut arg_itr = std::env::args();

        let mut parameters: Vec<Parameter> = Vec::new();
        let mut flags: Vec<Flag> = Vec::new();

        let mut next_arg = arg_itr.next();

        while let Some(name) = next_arg {
            assert!(name.starts_with("--"));

            next_arg = arg_itr.next();
            let maybe_next = next_arg.clone().filter(|value| !value.starts_with("--"));

            if let Some(value) = maybe_next {
                parameters.push(Parameter {
                    name,
                    value,
                    is_used: false,
                });
                next_arg = arg_itr.next();
            } else {
                flags.push(Flag {
                    name,
                    is_used: false,
                })
            }
        }

        ArgumentParser { parameters, flags }
    }

    pub fn contains(&mut self, argument: &str) -> bool {
        for elt in &mut self.flags {
            if argument == elt.name {
                elt.is_used = true;
                return true;
            }
        }
        // also check parameters
        for elt in &self.parameters {
            if argument == elt.name {
                // do not consider it 'used'
                return true;
            }
        }

        return false;
    }

    pub fn get_parameter(&mut self, argument: &str) -> Option<String> {
        for elt in &mut self.parameters {
            if argument == elt.name {
                elt.is_used = true;
                return Some(elt.value.clone());
            }
        }

        None
    }
}
