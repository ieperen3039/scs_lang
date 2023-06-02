// scs_program             = [ version_declaration ], { definition | use_declaration }, function_block;
// use_declaration         = KeywordUse, StringLiteral;
pub struct Program<'a> {
    pub version: Option<Version<'a>>,
    pub use_declarations: Vec<String>,
    pub definitions: Vec<Definition<'a>>,
    pub main: FunctionBlock<'a>,
}

// version_declaration     = KeywordVersion, Name, ( Star | version );
// version                 = DecimalLiteral, Period, DecimalLiteral, Period, DecimalLiteral;
pub struct Version<'a> {
    pub dialect: &'a str,
    pub version: Option<(u16, u16, u16)>,
}

// definition              = scope | type_definition | enum_definition | implementation | function_definition;
pub enum Definition<'a> {
    // scope                   = Name, BracketOpen, { definition }, BracketClose;
    Scope(&'a str),
    Type(TypeDefinition<'a>),
    Enum(EnumDefinition<'a>),
    // implementation          = KeywordImpl, BracketOpen, { function_definition }, BracketClose;
    Impl(TypeName<'a>, Vec<FunctionDefinition<'a>>),
    Function(FunctionDefinition<'a>),
}

// type_name               = named_type | array_type | fn_type;
pub enum TypeName<'a> {
    // named_type              = Name, [ generic_type ];
    Named(&'a str),
    // generic_type            = AngleBracketOpen, Name, AngleBracketClose;
    Generic { name: &'a str, generic: &'a str },
    // array_type              = type_name, SquareBracketOpen, SquareBracketClose;
    Array(&'a str),
    Function(Box<FunctionType<'a>>),
}

// fn_type                 = KeywordFn, [ AngleBracketOpen, [ ParenthesisOpen, { unnamed_parameter_list }, ParenthesisClose ], type_name, AngleBracketClose ];
// unnamed_parameter_list  = type_name, { Comma, type_name };
pub struct FunctionType<'a> {
    name: &'a str,
    parameters: Vec<&'a str>,
    // assignment              = AngleBracketClose, ( KeywordReturn | [ type_name ], Name);
    return_type: TypeName<'a>,
}

// type_definition         = KeywordType, Equal, ( type_name | KeywordNative ), [ BracketOpen, { function_definition }, BracketClose ];
pub struct TypeDefinition<'a> {
    pub type_name: TypeName<'a>,
    pub derived_from: Option<TypeName<'a>>, // None if native
    pub implementation: Option<FunctionBlock<'a>>,
}

// enum_definition         = KeywordEnum, Name, SquareBracketOpen, { Name, Comma }, SquareBracketClose;
pub struct EnumDefinition<'a> {
    pub name: &'a str,
    pub values: Vec<&'a str>,
}

// function_definition     = function_signature, ( function_block | KeywordNative );
// function_signature      = KeywordFn, [ KeywordStatic ], Name, [ generic_type ], [ ParenthesisOpen, [ parameter_list ], ParenthesisClose ], Colon, type_name;
// parameter_list          = parameter, { Comma, parameter };
// parameter               = type_name, [ Ellipsis ], Name;
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub parameters: Vec<Parameter<'a>>,
    pub return_type: TypeName<'a>,
    pub is_static: bool,
}

pub struct Parameter<'a> {
    pub type_name: TypeName<'a>,
    pub name: &'a str,
    pub expansion: bool,
}

// function_block          = BracketOpen, { statement }, [ last_statement ], BracketClose;
pub struct FunctionBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    pub last_statement: Option<Box<Statement<'a>>>,
}

// statement               = expression, { method_call }, [ assignment ], SemiColon;
// last_statement          = expression, { method_call };
pub struct Statement<'a> {
    pub base_element: Expression<'a>,
    pub modifiers: Vec<MethodCall<'a>>,
    pub return_variable: Option<TypeName<'a>>, // None if last_statement
}

// expression              = Name | static_function_call | function_block | array_initialisation | literal;
pub enum Expression<'a> {
    VariableName(String),
    // static_function_call    = Name, Period, method_call;
    StaticFunctionCall {
        namespace: &'a str,
        function: MethodCall<'a>,
    },
    FunctionBlock(FunctionBlock<'a>),
    // array_initialisation    = SquareBracketOpen, [ expression, { Comma, expression } ], SquareBracketClose;
    Array(Vec<Expression<'a>>),
    Literal(Literal<'a>),
}

// literal                 = StringLiteral | DecimalLiteral | HexadecimalLiteral | BinaryLiteral;
pub enum Literal<'a> {
    Number(u32),
    SignedNumber(i32),
    String(&'a str),
}

// method_call             = Name, [ ParenthesisOpen, [ argument_list ], ParenthesisClose ];
pub struct MethodCall<'a> {
    name: &'a str,
    arguments: ArgumentList<'a>,
}

// argument_list           = Name | named_argument, { Comma, named_argument };
pub enum ArgumentList<'a> {
    Empty,
    Name(&'a str),
    NamedList(Vec<NamedArgument<'a>>),
}

// named_argument          = Name, Colon, expression;
pub struct NamedArgument<'a> {
    parameter_name: &'a str,
    value: Expression<'a>,
}
