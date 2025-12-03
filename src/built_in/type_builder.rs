use crate::built_in::primitives;
use crate::symbolization::ast::*;

pub struct TypeBuilder {
    var_id: TypeId,
}

impl TypeBuilder {
    pub fn new() -> TypeBuilder {
        TypeBuilder { var_id: primitives::FIRST_CUSTOM_TYPE_ID }
    }

    pub fn new_type_id(&mut self) -> TypeId {
        let id = self.var_id;
        self.var_id = id + 1;
        id
    }

    fn build(&mut self, mut full_name: Vec<&str>, class: TypeClass) -> TypeDefinition {
        let name = Identifier::from(full_name.pop().unwrap());
        let full_scope = full_name.into_iter().map(Identifier::from).collect();

        TypeDefinition {
            id: self.new_type_id(),
            name,
            full_scope,
            type_class: class,
        }
    }

    pub fn build_plain(&mut self, full_name: Vec<&str>) -> TypeDefinition {
        self.build(full_name, TypeClass::Base { derived: None })
    }

    pub fn build_derived(&mut self, full_name: Vec<&str>, base: &TypeRef) -> TypeDefinition {
        self.build(full_name, TypeClass::Base {
            derived: Some(Box::from(base.clone())),
        })
    }

    pub fn build_result(&mut self, type_1: &TypeRef, type_2: &TypeRef) -> TypeDefinition {
        self.build_variant(
            vec![&format!("{:?}!{:?}", type_1, type_2)],
            vec![("Pos", type_1), ("Neg", type_2)],
        )
    }

    pub fn build_optional(&mut self, opt_type: &TypeRef) -> TypeDefinition {
        self.build_variant(
            vec![&format!("{:?}?", opt_type)],
            vec![("Some", opt_type), ("None", &TypeRef::Void)],
        )
    }

    pub fn build_variant(&mut self, full_name: Vec<&str>, values: Vec<(&str, &TypeRef)>) -> TypeDefinition {
        self.build(full_name, TypeClass::Variant {
            variants: values
                .into_iter()
                .map(|(value_1, type_1)| VariantValue {
                    name: Identifier::from(value_1),
                    value_type: type_1.clone(),
                })
                .collect(),
        })
    }
}
