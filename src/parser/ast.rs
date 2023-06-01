
pub struct FunctionBody {
    statements : Vec<Statement>
}

pub struct Statement {
    initial : Evaluatable,
    modifiers : Vec<MethodCall>,
    result_assign : VariableDefinition,
}

pub struct MethodCall {
    source_type : Box<TypeDefinition>,
    definition : Box<FunctionDefinition>,
    parameters : Vec<ParameterAssign>,
}

pub struct ParameterAssign {
    parameter : Box<ParameterDefinition>,
    value : Evaluatable
}

pub enum Evaluatable {
    Variable(Box<VariableDefinition>),
    StaticFunctionCall(StaticFunctionCall),
    InlineFunction(FunctionBody)
}

pub struct StaticFunctionCall {
    definition : Box<FunctionDefinition>,
    parameters : Vec<Evaluatable>,
}

pub struct TypeDefinition {
    name: String,
    derived_from: Option<Box<TypeDefinition>>,
    methods : Vec<FunctionDefinition>
}

pub struct ParameterDefinition {
    name: String,
    r#type: TypeDefinition,
}

pub struct VariableDefinition {
    name: String,
    r#type: TypeDefinition,
}

pub struct FunctionDefinition {
    name: String,
    parameter_types: Vec<String>,
    body: FunctionBody,
}