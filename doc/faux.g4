grammar faux;

// lexical rules
WS : [ \r\n\t\p{White_Space}] -> skip;
LINE_COMMENT : '//' ~[\r\n]* '\r'? '\n' -> skip ;
BLOCK_COMMENT : '/*' ~[*/]* '*/' -> skip ;

IDENTIFIER : [a-zA-Z_] [a-zA-Z0-9_]* ;
NUMBER : [0-9]+ ;
STRING : '"' ~'"'* '"' ;
OPERATOR : [?!#$%&*+\-/|~\\^`@:<>] ;

// parser rules
faux_program            : version_declaration? include_declaration* definition_* (program_interface function_block)? EOF;
program_interface       : function_interface_;
version_dialect         : 'faux';
version_declaration     : 'version' version_dialect version?;
version                 : version_major '.' version_minor '.' version_patch;
version_major           : NUMBER;
version_minor           : NUMBER;
version_patch           : NUMBER;
include_declaration     : 'use' use_export_decl? include_file;
include_file            : STRING;
identifier              : IDENTIFIER;
definition_             : constant_def | scope | type_definition | enum_definition | variant_definition | implementation | function_definition;
scope                   : scope_name '{' definition_* '}';
scope_name              : IDENTIFIER;
type_definition         : 'type' base_type_decl ':'  (type_ref | native_decl);
field_declaration       : type_ref identifier ';';
type_ref                : (base_type_ref | tuple_inst | fn_type) buffer_symbol*;
tuple_inst              : '[' type_list_ ']';
base_type_ref           : (scope_name '.')* identifier generic_types_inst?;
base_type_decl          : identifier generic_types_decl?;
generic_types_decl      : '<' name_list_ '>';
generic_types_inst      : '<' type_list_ '>';
fn_type                 : 'fn' '<' ('(' unnamed_parameter_list ')')? return_type '>';
enum_definition         : 'enum' identifier '[' enum_value_decl (',' enum_value_decl)* ']';
enum_value_decl         : IDENTIFIER;
variant_definition      : 'variant' identifier generic_types_decl? '[' variant_value_decl (',' variant_value_decl)* ']';
variant_value_decl      : identifier '(' type_ref ')';
implementation          : 'impl' base_type_decl buffer_symbol* '{' function_definition* '}';
constant_def            : 'const' type_ref identifier ':' expression_;
function_definition     : function_signature ( function_block | native_decl );
function_signature      : 'fn' function_name generic_types_decl? function_interface_;
function_name           : IDENTIFIER;
function_interface_     : '(' parameter_list? ')' ':' return_type;
parameter_list          : parameter (',' parameter)*;
parameter               : type_ref expansion_decl? identifier;
unnamed_parameter_list  : type_list_;
untyped_parameter_list  : name_list_;
return_type             : type_ref | no_return_decl;
function_block          : '{' statement ( statement_separator statement )* statement_separator? '}';
statement               : value_expression_ function_expression_*; 
expression_             : value_expression_ | function_expression_;
value_expression_       : variable_name | tuple_construction | literal_ | function_as_value; 
function_expression_    : method_call | external_method_call | mutator_cast | mutator_assign | implicit_par_lamda | explicit_par_lamda | (operator function_expression_);
function_as_value       : 'fn' '(' function_expression_ ')';
statement_separator     : ';';
variable_name           : IDENTIFIER;
method_call             : function_name '(' (argument_list_)? ')';
external_method_call    : base_type_ref '.' method_call;
argument_list_          : single_argument | (named_argument (',' named_argument)*);
single_argument         : expression_;
named_argument          : identifier ':' expression_;
type_list_              : type_ref (',' type_ref)*;
name_list_              : identifier (',' identifier)*;
operator                : OPERATOR;
mutator_cast            : 'as' type_ref;
mutator_assign          : '=' (return_decl | (type_ref? variable_name));
tuple_construction      : '[' value_expression_ (',' value_expression_)* ']';
implicit_par_lamda     : function_block;
explicit_par_lamda     : '(' untyped_parameter_list ')' (':' return_type)? function_block;
literal_                : string_literal | integer_literal | float_literal;
string_literal          : STRING;
integer_literal         : NUMBER;
float_literal           : NUMBER '.' NUMBER;
return_decl             : 'return';
native_decl             : 'extern';
use_export_decl         : 'public';
no_return_decl          : '!';
buffer_symbol           : '[]';
expansion_decl          : '...';