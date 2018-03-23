use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::hash::{Hash, Hasher};

use core::parse_and_eval;
use core::value::{Value, ValuePtr, BuiltinFuncType, FuncKind, FuncParam, Applicable};
use evaluator::builtinfunc;
use reader::tokenize;

#[derive(PartialEq, Debug, Eq)]
pub struct Env {
    pub map: HashMap<String, ValuePtr>,
    pub outer: Option<EnvPtr>,
}

impl Hash for Env {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (key, val) in self.map.iter() {
            key.hash(state);
            val.hash(state);
        }
        self.outer.hash(state);
    }
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
            prepare_builtinfunc("+", Box::new(builtinfunc::op_add_integer), 2),
            prepare_builtinfunc("-", Box::new(builtinfunc::op_sub_integer), 2),
            prepare_builtinfunc("*", Box::new(builtinfunc::op_mul_integer), 2),
            prepare_builtinfunc("/", Box::new(builtinfunc::op_div_integer), 2),
            prepare_builtinfunc("=", Box::new(builtinfunc::op_equal), 2),
            prepare_builtinfunc("cons", Box::new(builtinfunc::cons), 2),
            prepare_builtinfunc("first", Box::new(builtinfunc::builtinfunc_first), 1),
            prepare_builtinfunc("rest", Box::new(builtinfunc::builtinfunc_rest), 1),
            prepare_builtinfunc("nil?", Box::new(builtinfunc::builtinfunc_nil_q), 1),
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

    pub fn lookup_nth_param(&self, n: usize) -> Option<&ValuePtr> {
        self.lookup(&format!("%{}", n).to_string())
    }
}

fn prepare_builtinfunc(name: &str, f: Box<BuiltinFuncType>, num_args: usize) -> (String, ValuePtr) {
    let name = name.to_string();
    let mut params = vec![];
    for i in 0..num_args {
        params.push(format!("%{}", i + 1));
    }
    let param = FuncParam::new(params, None);
    let env = Env::create_empty();
    let applicable = Applicable::new(None, param, FuncKind::BuiltinFunc(f));
    let closure = Value::create_closure(applicable, env);
    (name, closure)
}
