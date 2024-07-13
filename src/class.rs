use crate::{
    callable::{Callable, UserFunction},
    interpreter::{self, Interpreter},
    object::Object,
    token::Token,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Class {
    pub superclass: Option<Rc<Class>>,
    pub name: Token,
    pub methods: HashMap<String, UserFunction>,
}

impl Class {
    pub fn new(
        name: Token,
        superclass: Option<Rc<Class>>,
        methods: HashMap<String, UserFunction>,
    ) -> Self {
        Self {
            name,
            superclass,
            methods,
        }
    }

    pub fn arity(&self) -> u8 {
        if let Some(init) = self.find_method(&String::from("init")) {
            return init.decl.params.len() as u8;
        }

        0
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<Object>>,
    ) -> ::std::result::Result<Rc<Object>, interpreter::Error> {
        let instance = Rc::new(RefCell::new(Instance::new(self.clone())));

        if let Some(init) = self.find_method(&String::from("init")) {
            _ = init.bind(Rc::clone(&instance)).call(interpreter, args)?;
        }

        Ok(Rc::new(Object::Instance(instance)))
    }

    pub fn find_method(&self, name: &String) -> Option<UserFunction> {
        if self.methods.contains_key(name) {
            return self.methods.get(name).cloned();
        }

        match &self.superclass {
            Some(s) => s.find_method(name),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Class,
    fields: RefCell<HashMap<String, Rc<Object>>>,
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, instance: &Rc<RefCell<Instance>>, name: &Token) -> Option<Rc<Object>> {
        let var_name = &name.kind.as_string();
        if self.fields.borrow().contains_key(var_name) {
            return self.fields.borrow().get(&name.kind.as_string()).cloned();
        }

        self.class.find_method(var_name).map(|method| {
            Some(Rc::new(Object::Func(Callable::User(
                method.bind(Rc::clone(instance)),
            ))))
        })?
    }

    pub fn set(&self, name: &Token, obj: Rc<Object>) {
        self.fields.borrow_mut().insert(name.kind.as_string(), obj);
    }
}
