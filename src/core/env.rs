use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use core::parse_and_eval;
use core::value::{Value, ValuePtr, BuiltinFuncType, ApplicableBodyKind, Applicable};
use core::map;
use core::pattern::Pattern;
use evaluator::builtinfunc;
use reader::tokenize;

#[derive(PartialEq, Debug, Eq)]
pub struct Env(map::TreeMap<String, ValuePtr>);

pub type EnvPtr = Rc<Env>;

impl Env {
    fn new(map: HashMap<String, ValuePtr>, outer: Option<EnvPtr>) -> EnvPtr {
        let mut new_map = if let Some(outer_map) = outer {
            outer_map.0.clone()
        } else {
            map::TreeMap::create_empty()
        };
        for (k, v) in map.iter() {
            new_map = new_map.insert(k.to_string(), v.clone()).0;
        }
        Rc::new(Env(new_map))
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
            prepare_builtinfunc("<", Box::new(builtinfunc::op_lt_integer), 2),
            prepare_builtinfunc(">", Box::new(builtinfunc::op_gt_integer), 2),
            prepare_builtinfunc("=", Box::new(builtinfunc::op_equal), 2),
            prepare_builtinfunc("cons", Box::new(builtinfunc::cons), 2),
            prepare_builtinfunc("first", Box::new(builtinfunc::builtinfunc_first), 1),
            prepare_builtinfunc("rest", Box::new(builtinfunc::builtinfunc_rest), 1),
            prepare_builtinfunc("list?", Box::new(builtinfunc::builtinfunc_list_q), 1),
            prepare_builtinfunc("nil?", Box::new(builtinfunc::builtinfunc_nil_q), 1),
            prepare_builtinfunc("println", Box::new(builtinfunc::builtinfunc_println), 1),
            prepare_builtinfunc("_get", Box::new(builtinfunc::builtinfunc_get), 3),
        ];
        Env::load_library(Env::new(HashMap::from_iter(pairs), None))
    }

    pub fn lookup(&self, key: &String) -> Option<&ValuePtr> {
        self.0.lookup(key)
    }

    pub fn lookup_nth_param(&self, n: usize) -> Option<&ValuePtr> {
        self.lookup(&format!("%{}", n).to_string())
    }
}

fn prepare_builtinfunc(name: &str, f: Box<BuiltinFuncType>, num_args: usize) -> (String, ValuePtr) {
    let name = name.to_string();
    let mut params = vec![];
    for i in 0..num_args {
        params.push(Pattern::create_symbol(Value::create_symbol(format!("%{}", i + 1))));
    }
    let param = Pattern::create_vector(params, vec![], None);
    let env = Env::create_empty();
    let applicable = Applicable::new(None, param, ApplicableBodyKind::BuiltinBody(f));
    let closure = Value::create_closure(applicable, env);
    (name, closure)
}
