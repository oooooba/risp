use std::rc::Rc;
use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::iter::FromIterator;

use core::exception::Exception;

#[derive(Debug)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(LinkedList<ValuePtr>),
    ClosureValue(FuncKind, String, EnvPtr),
    // (body, arg, env), support only one argument currently
    NilValue,
    MapValue(HashMap<String, ValuePtr>, ValuePtr), // (map, extra_map), extra_map must be MapValue or NilValue
}

impl ValueKind {
    pub fn matches_nil(&self) -> bool {
        match self {
            &ValueKind::NilValue => true,
            _ => false,
        }
    }

    pub fn matches_map(&self) -> bool {
        match self {
            &ValueKind::MapValue(_, _) => true,
            //ValueKind::MapValue(_)=>true,
            _ => false,
        }
    }

    pub fn flatten_map(&self) -> HashMap<String, ValuePtr> {
        let (map, extra_map) = match self {
            &ValueKind::NilValue => return HashMap::new(),
            &ValueKind::MapValue(ref map, ref extra_map) => (map, extra_map),
            _ => unreachable!(),
        };
        let mut result_map = extra_map.kind.flatten_map();
        for (k, v) in map.iter() {
            result_map.insert(k.clone(), v.clone());
        }
        result_map
    }
}

impl PartialEq for ValueKind {
    fn eq(&self, other: &ValueKind) -> bool {
        use self::ValueKind::*;
        match (self, other) {
            (&IntegerValue(ref lhs), &IntegerValue(ref rhs)) if lhs == rhs => true,
            (&StringValue(ref lhs), &StringValue(ref rhs)) if lhs == rhs => true,
            (&SymbolValue(ref lhs), &SymbolValue(ref rhs)) if lhs == rhs => true,
            (&KeywordValue(ref lhs), &KeywordValue(ref rhs)) if lhs == rhs => true,
            (&ListValue(ref lhs), &ListValue(ref rhs)) if lhs == rhs => true,
            (&NilValue, &NilValue) => true,
            (&MapValue(_, _), &MapValue(_, _)) => self.flatten_map() == other.flatten_map(),
            _ => false,
        }
    }
}

pub enum FuncKind {
    AstFunc(ValuePtr),
    BuiltinFunc(Box<Fn(EnvPtr) -> Result<ValuePtr, Exception>>),
}

impl fmt::Debug for FuncKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FuncKind::*;
        match self {
            &AstFunc(ref ast) => write!(f, "<AstFunc> :- {:?}", ast),
            &BuiltinFunc(_) => write!(f, "<BuiltinFunc>"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Value {
    pub kind: ValueKind,
}

pub type ValuePtr = Rc<Value>;

impl Value {
    fn new(kind: ValueKind) -> ValuePtr {
        Rc::new(Value { kind: kind })
    }

    pub fn create_integer(integer: isize) -> ValuePtr {
        Value::new(ValueKind::IntegerValue(integer))
    }

    pub fn create_string(string: String) -> ValuePtr {
        Value::new(ValueKind::StringValue(string))
    }

    pub fn create_symbol(symbol: String) -> ValuePtr {
        Value::new(ValueKind::SymbolValue(symbol))
    }

    pub fn create_keyword(keyword: String) -> ValuePtr {
        Value::new(ValueKind::KeywordValue(keyword))
    }

    pub fn create_list(mut values: Vec<ValuePtr>) -> ValuePtr {
        let mut list = LinkedList::new();
        while let Some(value) = values.pop() {
            list.push_front(value);
        }
        Value::new(ValueKind::ListValue(list))
    }

    pub fn create_closure(func: FuncKind, arg: String, env: EnvPtr) -> ValuePtr {
        Value::new(ValueKind::ClosureValue(func, arg, env))
    }

    pub fn create_nil() -> ValuePtr {
        Value::new(ValueKind::NilValue)
    }

    pub fn create_map(map: HashMap<String, ValuePtr>, extra_map: ValuePtr) -> ValuePtr {
        assert!(extra_map.kind.matches_map() || extra_map.kind.matches_nil());
        Value::new(ValueKind::MapValue(map, extra_map))
    }
}

#[derive(PartialEq, Debug)]
pub struct Env {
    pub map: HashMap<String, ValuePtr>,
    pub outer: Option<EnvPtr>,
}

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

pub type EnvPtr = Rc<Env>;

#[cfg(test)]
mod tests {
    use super::*;
    use core::exception::{Exception, ExceptionKind};

    fn builtin_func(env: EnvPtr) -> Result<ValuePtr, Exception> {
        let x_str = "x".to_string();
        let y_str = "y".to_string();
        let type_int_str = "Integer".to_string();
        let type_unknown_str = "Unknown".to_string();

        let x_val = env.map.get(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
        let x_int = match x_val.kind {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        let y_val = env.map.get(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
        let y_int = match y_val.kind {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        Ok(Value::create_integer(x_int + y_int))
    }

    fn run_builtin_func(func: Box<Fn(EnvPtr) -> Result<ValuePtr, Exception>>, env: EnvPtr, result: Result<ValuePtr, Exception>) {
        let func = FuncKind::BuiltinFunc(func);
        let closure = Value::create_closure(func, "_".to_string(), env);
        match closure.kind {
            ValueKind::ClosureValue(ref func, _, ref env) => {
                match func {
                    &FuncKind::BuiltinFunc(ref f) => assert_eq!(f(Env::new(env.map.clone(), None)), result),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_builtin_function() {
        use core::exception::ExceptionKind::*;
        {
            let env = Env::create(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("y".to_string(), Value::create_integer(2)),
            ], None);
            run_builtin_func(Box::new(builtin_func), env,
                             Ok(Value::create_integer(3)));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("z".to_string(), Value::create_integer(2)),
            ], None);
            run_builtin_func(Box::new(builtin_func), env,
                             Err(Exception::new(EvaluatorUndefinedSymbolException("y".to_string()), None)));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), Value::create_string("1".to_string())),
                ("y".to_string(), Value::create_integer(2)),
            ], None);
            run_builtin_func(Box::new(builtin_func), env,
                             Err(Exception::new(EvaluatorTypeException("Integer".to_string(), "Unknown".to_string()), None)));
        }
    }

    #[test]
    fn test_map_value() {
        {
            let map1 = HashMap::from_iter(vec![("x".to_string(), Value::create_integer(1))]);
            let map2 = HashMap::from_iter(vec![("y".to_string(), Value::create_integer(2))]);
            let map3 = HashMap::from_iter(vec![("z".to_string(), Value::create_integer(3))]);
            let lhs_map_value = Value::create_map(map1,
                                                  Value::create_map(map2,
                                                                    Value::create_map(map3,
                                                                                      Value::create_nil())));
            let map123 = HashMap::from_iter(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("y".to_string(), Value::create_integer(2)),
                ("z".to_string(), Value::create_integer(3)),
            ]);
            let rhs_map_value = Value::create_map(map123, Value::create_nil());
            assert_eq!(lhs_map_value, rhs_map_value);
        }
    }
}
