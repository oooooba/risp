use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;

use core::value::{Value, ValuePtr, ValueKind, FuncKind};
use core::exception::{Exception, ExceptionKind};

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
            ("+".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(|env| {
                let x_str = "x".to_string();
                let y_str = "y".to_string();
                let type_int_str = ValueKind::type_str_integer().to_string();

                let x_val = env.lookup(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
                let x_int = match x_val.kind {
                    ValueKind::IntegerValue(n) => n,
                    _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, x_val.kind.as_type_str().to_string()), None)),
                };

                let y_val = env.lookup(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
                let y_int = match y_val.kind {
                    ValueKind::IntegerValue(n) => n,
                    _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, y_val.kind.as_type_str().to_string()), None)),
                };

                Ok(Value::create_integer(x_int + y_int))
            })), vec!["x".to_string(), "y".to_string()], Env::create_empty())),
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
