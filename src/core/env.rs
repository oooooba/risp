use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use core::parse_and_eval;
use core::value::{Value, ValuePtr, FuncKind, FuncParam};
use evaluator::builtinfunc;
use reader::tokenize;

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

    pub fn load_library(outer: EnvPtr) -> EnvPtr {
        let risp_home = env::var("RISP_HOME").unwrap();
        let core_path: PathBuf = [&risp_home, "library", "core.clj"].iter().collect();
        let mut file = File::open(core_path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        let mut tokens = tokenize(content, outer.clone()).unwrap();
        let mut outer_env = outer;
        loop {
            let (result, rest_tokens) = parse_and_eval(tokens, outer_env.clone());
            let env = match result {
                Ok(_) => outer_env,
                Err(exception) => exception.extract_env_from_continuation().unwrap_or(outer_env),
            };
            match rest_tokens {
                None => return env,
                Some(rest_tokens) => {
                    tokens = rest_tokens;
                }
            }
            outer_env = env;
        }
    }

    pub fn create_default() -> EnvPtr {
        let pairs = vec![
            ("(".to_string(), Value::create_keyword("(".to_string())),
            (")".to_string(), Value::create_keyword(")".to_string())),
            ("[".to_string(), Value::create_keyword("[".to_string())),
            ("]".to_string(), Value::create_keyword("]".to_string())),
            ("+".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_add_integer)),
                                                    None,
                                                    FuncParam::new(vec!["%1".to_string(), "%2".to_string()], None),
                                                    Env::create_empty())),
            ("-".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_sub_integer)),
                                                    None,
                                                    FuncParam::new(vec!["%1".to_string(), "%2".to_string()], None),
                                                    Env::create_empty())),
            ("*".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_mul_integer)),
                                                    None,
                                                    FuncParam::new(vec!["%1".to_string(), "%2".to_string()], None),
                                                    Env::create_empty())),
            ("/".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_div_integer)),
                                                    None,
                                                    FuncParam::new(vec!["%1".to_string(), "%2".to_string()], None),
                                                    Env::create_empty())),
            ("=".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::op_equal)),
                                                    None,
                                                    FuncParam::new(vec!["x".to_string(), "y".to_string()], None),
                                                    Env::create_empty())),
            ("cons".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::cons)),
                                                       None,
                                                       FuncParam::new(vec!["%1".to_string(), "%2".to_string()], None),
                                                       Env::create_empty())),
            ("first".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::builtinfunc_first)),
                                                        None,
                                                        FuncParam::new(vec!["%1".to_string()], None),
                                                        Env::create_empty())),
            ("rest".to_string(), Value::create_closure(FuncKind::BuiltinFunc(Box::new(builtinfunc::builtinfunc_rest)),
                                                       None,
                                                       FuncParam::new(vec!["%1".to_string()], None),
                                                       Env::create_empty())),
        ];
        Env::load_library(Env::new(HashMap::from_iter(pairs), None))
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
