use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;

use core::value::{Value, ValuePtr, FuncKind};
use evaluator::builtinfunc;

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

    pub fn create_default() -> EnvPtr {
        let pairs = vec![
            ("(".to_string(), Value::create_keyword("(".to_string())),
            (")".to_string(), Value::create_keyword(")".to_string())),
            ("[".to_string(), Value::create_keyword("[".to_string())),
            ("]".to_string(), Value::create_keyword("]".to_string())),
            ("+".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_add_integer)),
                                                    vec!["%1".to_string(), "%2".to_string()],
                                                    Env::create_empty())),
            ("=".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_equal)),
                                                    vec!["x".to_string(), "y".to_string()],
                                                    Env::create_empty())),
            ("not".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(|env| {
                let val = env.lookup(&"%".to_string()).unwrap();
                Ok(Value::create_boolean(*val == Value::create_boolean(false) || *val == Value::create_nil()))
            })), vec!["%".to_string()], Env::create_empty())),
        ];
        Env::new(HashMap::from_iter(pairs), None)
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
