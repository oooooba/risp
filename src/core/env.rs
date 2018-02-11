use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;

use core::value::{Value, ValuePtr};

#[derive(PartialEq, Debug)]
pub struct Env {
    pub map: HashMap<String, ValuePtr>,
    pub outer: Option<EnvPtr>,
}

pub type EnvPtr = Rc<Env>;

impl Env {
    fn new(map: HashMap<String, ValuePtr>, outer: Option<EnvPtr>) -> EnvPtr {
        Rc::new(Env {
            map: map,
            outer: outer,
        })
    }

    pub fn create_empty() -> EnvPtr {
        Env::new(HashMap::new(), None)
    }

    pub fn create(pairs: Vec<(String, ValuePtr)>, outer: Option<EnvPtr>) -> EnvPtr {
        Env::new(HashMap::from_iter(pairs), outer)
    }

    pub fn create_global() -> EnvPtr {
        let pairs = vec![
            ("(".to_string(), Value::create_keyword("(".to_string())),
            (")".to_string(), Value::create_keyword(")".to_string())),
        ];
        Env::new(HashMap::from_iter(pairs), None)
    }

    pub fn create_clone(original_env: &EnvPtr) -> EnvPtr {
        Env::new(original_env.map.clone(), None)
    }

    pub fn lookup(&self, key: &String) -> Option<&ValuePtr> {
        match self.map.get(key) {
            value @ Some(_) => value,
            None => match self.outer {
                Some(ref env) => env.lookup(key),
                None => None,
            }
        }
    }
}
