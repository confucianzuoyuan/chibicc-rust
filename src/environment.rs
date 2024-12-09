use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ast::Obj, sema::Type};

#[derive(Debug)]
pub struct Scope {
    pub vars: Vec<HashMap<String, Rc<RefCell<Obj>>>>,
    pub tags: Vec<HashMap<String, Type>>,
}

impl Scope {
    pub fn new() -> Self {
        let mut scope = Scope {
            vars: vec![],
            tags: vec![],
        };
        scope.enter_scope();
        scope
    }

    pub fn is_parent_env_empty(&self) -> bool {
        self.vars.len() == self.tags.len() && self.vars.len() <= 1 && self.tags.len() <= 1
    }

    pub fn enter_scope(&mut self) {
        self.vars.insert(0, HashMap::new());
        self.tags.insert(0, HashMap::new());
    }

    pub fn leave_scope(&mut self) {
        self.vars.remove(0);
        self.tags.remove(0);
    }

    pub fn find_var(&mut self, name: String) -> Option<Rc<RefCell<Obj>>> {
        for sc in &self.vars {
            if let Some(obj) = sc.get(&name) {
                return Some(obj.clone());
            }
        }

        None
    }

    pub fn find_tag(&mut self, name: String) -> Option<Type> {
        for sc in &self.tags {
            if let Some(ty) = sc.get(&name) {
                return Some(ty.clone());
            }
        }

        None
    }

    pub fn find_tag_in_current_scope(&mut self, name: String) -> Option<Type> {
        if let Some(sc) = self.tags.first_mut() {
            if let Some(ty) = sc.get(&name) {
                return Some(ty.clone());
            }
        }
        None
    }

    pub fn replace_tag_in_current_scope(&mut self, name: String, ty: Type) {
        if let Some(sc) = self.tags.first_mut() {
            sc.insert(name, ty.clone());
        }
    }

    pub fn enter_tag(&mut self, name: String, ty: Type) {
        if let Some(sc) = self.tags.first_mut() {
            sc.insert(name, ty);
        } else {
            // self.tags.insert(0, HashMap::new());
            self.enter_scope();
            if let Some(sc) = self.tags.first_mut() {
                sc.insert(name, ty);
            }
        }
    }

    pub fn enter_var(&mut self, name: String, var: Rc<RefCell<Obj>>) {
        if let Some(sc) = self.vars.first_mut() {
            sc.insert(name, var);
        } else {
            // self.vars.insert(0, HashMap::new());
            self.enter_scope();
            if let Some(sc) = self.vars.first_mut() {
                sc.insert(name, var);
            }
        }
    }
}
