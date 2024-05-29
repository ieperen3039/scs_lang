use std::rc::Rc;

use super::{
    ast::*,
    semantic_result::{SemanticError, SemanticResult},
};

struct Var {
    rc: Rc<VariableDeclaration>,
    used: bool,
}

pub struct VarStorage {
    data: Vec<Var>,
    next_var_id: VariableId,
}

impl VarStorage {
    pub fn new() -> VarStorage {
        VarStorage {
            data: Vec::new(),
            next_var_id: 0,
        }
    }

    pub fn from(other: &VarStorage) -> VarStorage {
        VarStorage {
            data: other
                .data
                .iter()
                .map(|v| Var {
                    rc: v.rc.clone(),
                    used: false,
                })
                .collect(),
            next_var_id: other.next_var_id,
        }
    }

    pub fn use_var_by_name(&mut self, identifier: &str) -> Option<Rc<VariableDeclaration>> {
        let found = self
            .data
            .iter_mut()
            .find(|var| var.rc.name.as_ref() == identifier);

        if let Some(var) = found {
            var.used = true;
            Some(var.rc.clone())
        } else {
            None
        }
    }

    pub fn use_var(&mut self, identifier: VariableId) -> Rc<VariableDeclaration> {
        let var = self.data
            .iter_mut()
            .find(|var| var.rc.id == identifier)
            .expect("ids given to use_var should always exist");

        var.used = true;
        var.rc.clone()
    }

    pub fn get_unused_vars(&self) -> Vec<Rc<VariableDeclaration>> {
        self.data
            .iter()
            .filter(|v| !v.used)
            .map(|v| v.rc.clone())
            .collect()
    }

    pub fn get_used_vars(&self) -> Vec<Rc<VariableDeclaration>> {
        self.data
            .iter()
            .filter(|v| v.used)
            .map(|v| v.rc.clone())
            .collect()
    }

    pub fn insert(
        &mut self,
        name: Identifier,
        var_type: TypeRef,
    ) -> SemanticResult<Rc<VariableDeclaration>> {
        let id = self.next_var_id;
        self.next_var_id += 1;
        let var: Rc<VariableDeclaration> = Rc::from(VariableDeclaration { id, var_type, name });
        self.insert_raw(var.clone())?;
        Ok(var)
    }

    pub fn insert_raw(&mut self, var: Rc<VariableDeclaration>) -> SemanticResult<()> {
        if self.contains(&var) {
            return Err(SemanticError::VariableExists {
                name: var.name.clone(),
            });
        }

        self.data.push(Var {
            rc: var,
            used: false,
        });

        Ok(())
    }

    pub fn insert_from_param(&mut self, param: &Parameter) -> SemanticResult<()> {
        self.insert_raw(Rc::from(VariableDeclaration {
            var_type: param.par_type.to_owned(),
            name: param.name().to_owned(),
            id: param.id,
        }))
    }

    pub fn contains(&self, var: &VariableDeclaration) -> bool {
        // TODO shadowing
        self.data.iter().any(|inner| inner.rc.name == var.name)
    }

    pub fn get_var_ids(&self) -> Vec<VariableId> {
        self.data.iter().map(|v| v.rc.id).collect()
    }
}
