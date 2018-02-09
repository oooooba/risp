use std::rc::Rc;
use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::iter::FromIterator;

use value::Exception;

#[derive(Debug)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(LinkedList<Value>),
    ClosureValue(FuncKind, String, EnvPtr), // (body, arg, env), support only one argument currently
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
            _ => false,
        }
    }
}

pub enum FuncKind {
    AstFunc(Value),
    BuiltinFunc(Box<Fn(EnvPtr) -> Result<Value, Exception>>),
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

pub type Value = Rc<ValueKind>;

pub fn create_integer_value(integer: isize) -> Value {
    Rc::new(ValueKind::IntegerValue(integer))
}

pub fn create_string_value(string: String) -> Value {
    Rc::new(ValueKind::StringValue(string))
}

pub fn create_symbol_value(symboll: String) -> Value {
    Rc::new(ValueKind::SymbolValue(symboll))
}

pub fn create_keyword_value(keyword: String) -> Value {
    Rc::new(ValueKind::KeywordValue(keyword))
}

pub fn create_list_value(mut values: Vec<Value>) -> Value {
    let mut list = LinkedList::new();
    while let Some(value) = values.pop() {
        list.push_front(value);
    }
    Rc::new(ValueKind::ListValue(list))
}

pub fn create_closure_value(func: FuncKind, arg: String, env: EnvPtr) -> Value {
    Rc::new(ValueKind::ClosureValue(func, arg, env))
}

#[derive(PartialEq, Debug)]
pub struct Env {
    pub map: HashMap<String, Value>,
    pub outer: Option<EnvPtr>,
}

impl Env {
    fn new(map: HashMap<String, Value>, outer: Option<EnvPtr>) -> EnvPtr {
        Rc::new(Env {
            map: map,
            outer: outer,
        })
    }

    pub fn create_empty() -> EnvPtr {
        Env::new(HashMap::new(), None)
    }

    pub fn create(pairs: Vec<(String, Value)>, outer: Option<EnvPtr>) -> EnvPtr {
        Env::new(HashMap::from_iter(pairs), outer)
    }

    pub fn create_global() -> EnvPtr {
        let pairs = vec![
            ("(".to_string(), create_keyword_value("(".to_string())),
            (")".to_string(), create_keyword_value(")".to_string())),
        ];
        Env::new(HashMap::from_iter(pairs), None)
    }

    pub fn lookup(&self, key: &String) -> Option<&Value> {
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
    use value::{Exception, ExceptionKind};

    fn builtin_func(env: EnvPtr) -> Result<Value, Exception> {
        let x_str = "x".to_string();
        let y_str = "y".to_string();
        let type_int_str = "Integer".to_string();
        let type_unknown_str = "Unknown".to_string();

        let x_val = env.map.get(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
        let x_int = match **x_val {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        let y_val = env.map.get(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
        let y_int = match **y_val {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        Ok(create_integer_value(x_int + y_int))
    }

    fn run_builtin_func(func: Box<Fn(EnvPtr) -> Result<Value, Exception>>, env: EnvPtr, result: Result<Value, Exception>) {
        let func = FuncKind::BuiltinFunc(func);
        let closure = create_closure_value(func, "_".to_string(), env);
        match *closure {
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
        use value::ExceptionKind::*;
        {
            let env = Env::create(vec![
                ("x".to_string(), create_integer_value(1)),
                ("y".to_string(), create_integer_value(2)),
            ], None);
            run_builtin_func(Box::new(builtin_func), env,
                             Ok(create_integer_value(3)));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), create_integer_value(1)),
                ("z".to_string(), create_integer_value(2)),
            ], None);
            run_builtin_func(Box::new(builtin_func), env,
                             Err(Exception::new(EvaluatorUndefinedSymbolException("y".to_string()), None)));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), create_string_value("1".to_string())),
                ("y".to_string(), create_integer_value(2)),
            ], None);
            run_builtin_func(Box::new(builtin_func), env,
                             Err(Exception::new(EvaluatorTypeException("Integer".to_string(), "Unknown".to_string()), None)));
        }
    }
}
