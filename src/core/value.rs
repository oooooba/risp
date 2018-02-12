use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use core::exception::Exception;
use core::env::EnvPtr;

#[derive(Debug)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(ValuePtr, ValuePtr), // (car, cdr), cdr must be ListValue or NilValue
    ClosureValue(FuncKind, Vec<String>, EnvPtr), // (body, args, env)
    NilValue,
    MapValue(HashMap<String, ValuePtr>, ValuePtr), // (map, extra_map), extra_map must be MapValue or NilValue
    BooleanValue(bool),
}

impl ValueKind {
    pub fn matches_list(&self) -> bool {
        match self {
            &ValueKind::ListValue(_, _) => true,
            _ => false,
        }
    }

    pub fn matches_nil(&self) -> bool {
        match self {
            &ValueKind::NilValue => true,
            _ => false,
        }
    }

    pub fn matches_map(&self) -> bool {
        match self {
            &ValueKind::MapValue(_, _) => true,
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

    pub fn length_list(&self) -> usize {
        assert!(self.matches_list() || self.matches_nil());
        let mut len = 0;
        let mut p = self;
        loop {
            match p {
                &ValueKind::ListValue(_, ref cdr) => {
                    len += 1;
                    p = &cdr.kind;
                }
                &ValueKind::NilValue => break,
                _ => {
                    len += 1;
                    break;
                }
            }
        }
        len
    }
}

impl PartialEq for ValueKind {
    fn eq(&self, other: &ValueKind) -> bool {
        use self::ValueKind::*;
        match (self, other) {
            (&IntegerValue(ref lhs), &IntegerValue(ref rhs)) => lhs == rhs,
            (&StringValue(ref lhs), &StringValue(ref rhs)) => lhs == rhs,
            (&SymbolValue(ref lhs), &SymbolValue(ref rhs)) => lhs == rhs,
            (&KeywordValue(ref lhs), &KeywordValue(ref rhs)) => lhs == rhs,
            (&ListValue(ref lhs_car, ref lhs_cdr), &ListValue(ref rhs_car, ref rhs_cdr)) => lhs_car == rhs_car && lhs_cdr == rhs_cdr,
            (&NilValue, &NilValue) => true,
            (&MapValue(_, _), &MapValue(_, _)) => self.flatten_map() == other.flatten_map(),
            (&BooleanValue(ref lhs), &BooleanValue(ref rhs)) => lhs == rhs,
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

    pub fn create_list(car: ValuePtr, cdr: ValuePtr) -> ValuePtr {
        assert!(cdr.kind.matches_list() || cdr.kind.matches_nil());
        Value::new(ValueKind::ListValue(car, cdr))
    }

    pub fn create_list_from_vec(mut values: Vec<ValuePtr>) -> ValuePtr {
        let mut list = Value::create_nil();
        while let Some(value) = values.pop() {
            list = Value::create_list(value, list);
        }
        list
    }

    pub fn create_closure(func: FuncKind, arg: Vec<String>, env: EnvPtr) -> ValuePtr {
        Value::new(ValueKind::ClosureValue(func, arg, env))
    }

    pub fn create_nil() -> ValuePtr {
        Value::new(ValueKind::NilValue)
    }

    pub fn create_map(map: HashMap<String, ValuePtr>, extra_map: ValuePtr) -> ValuePtr {
        assert!(extra_map.kind.matches_map() || extra_map.kind.matches_nil());
        Value::new(ValueKind::MapValue(map, extra_map))
    }

    pub fn create_boolean(boolean: bool) -> ValuePtr {
        Value::new(ValueKind::BooleanValue(boolean))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::FromIterator;

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
