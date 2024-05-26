use crate::symbolization::{ast::*, function_collector::FunctionCollector};

pub fn get_functions(collector: &mut FunctionCollector) -> Vec<FunctionDeclaration> {
    vec![
        FunctionProto::new("echo", collector)
            .req_par("in", None, &TypeRef::STRING)
            .flag(Some("error"), Some("e"))
            .returns(&TypeRef::STRING)
    ]
}

struct FunctionProto {
    fn_id: FunctionId,
    var_id: VariableId,
    name: Identifier,
    parameters: Vec<Parameter>,
}

impl FunctionProto {
    pub fn new(name: &str, collector: &mut FunctionCollector) -> FunctionProto {
        FunctionProto {
            fn_id: collector.new_id(),
            var_id: 0,
            name: Identifier::from(name),
            parameters: Vec::new(),
        }
    }

    fn new_id(&mut self) -> u32 {
        let id = self.var_id;
        self.var_id = id + 1;
        id
    }

    fn flag(
        mut self,
        long_name: Option<&str>,
        short_name: Option<&str>,
    ) -> FunctionProto {
        let new_par = Parameter {
            id: self.new_id(),
            par_type: TypeRef::Flag,
            long_name: long_name.map(Identifier::from),
            short_name: short_name.map(Identifier::from),
        };
        self.parameters.push(new_par);
        self
    }

    fn opt_par(
        mut self,
        long_name: &str,
        short_name: Option<&str>,
        t: &TypeRef,
    ) -> FunctionProto {
        let new_par = Parameter {
            id: self.new_id(),
            par_type: TypeRef::Optional(Box::from(t.clone())),
            long_name: Some(Identifier::from(long_name)),
            short_name: short_name.map(Identifier::from),
        };
        self.parameters.push(new_par);
        self
    }

    fn req_par(
        mut self,
        long_name: &str,
        short_name: Option<&str>,
        t: &TypeRef,
    ) -> FunctionProto {
        let new_par = Parameter {
            id: self.new_id(),
            par_type: t.clone(),
            long_name: Some(Identifier::from(long_name)),
            short_name: short_name.map(Identifier::from),
        };
        self.parameters.push(new_par);
        self
    }

    fn returns(self, type_ref: &TypeRef) -> FunctionDeclaration {
        FunctionDeclaration {
            id: self.fn_id,
            name: self.name,
            parameters: self.parameters,
            return_type: type_ref.clone(),
            is_external: true,
        }
    }
}
