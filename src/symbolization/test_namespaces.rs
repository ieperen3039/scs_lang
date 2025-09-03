use crate::built_in::function_builder::FunctionBuilder;
use crate::built_in::type_builder::TypeBuilder;
use crate::built_in::functions::NativeFunctionBuilder;
use crate::symbolization::ast;
use crate::symbolization::ast::*;


fn create_test_namespace() -> Namespace {
    let mut fn_ids = NativeFunctionBuilder::new();
    let mut type_builder = TypeBuilder::new();
    let mut namespace = Namespace::new_root();

    namespace.add_constant_literal(ast::Identifier::from("true"), Literal::Boolean(true));
    namespace.add_constant_literal(ast::Identifier::from("false"), Literal::Boolean(false));

    let foo_definition = type_builder.build_plain(vec!["foo"]);
    namespace.add_type(&foo_definition);

    {
        let mut foo_ns = namespace.get_type_scope(&foo_definition);

        foo_ns.add_function({
            let mut builder = FunctionBuilder::new();
            FunctionDeclaration {
                id: GlobalFunctionTarget::Native(fn_ids.new_id()),
                name: ast::Identifier::from("execute"),
                parameters: vec![builder.req_par("par1", &TypeRef::from(&foo_definition))],
                return_type: TypeRef::STRING,
                start_char: 0,
                generic_parameters: Vec::new(),
            }
        });

        namespace.add_sub_scope(foo_ns);
    }

    let bar_definition = type_builder.build_plain(vec!["bar"]);
    namespace.add_type(&bar_definition);

    {
        let mut bar_ns = namespace.get_type_scope(&bar_definition);
        bar_ns.add_function({
            let mut builder = FunctionBuilder::new();
            FunctionDeclaration {
                id: GlobalFunctionTarget::Native(fn_ids.new_id()),
                name: ast::Identifier::from("execute"),
                parameters: vec![builder.req_par("par1", &TypeRef::from(&bar_definition))],
                return_type: TypeRef::STRING,
                start_char: 0,
                generic_parameters: Vec::new(),
            }
        })
    }

    namespace
}

#[test]
fn hello_world() {

}