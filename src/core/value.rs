use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;
use std::ops::Index;
use std::iter::Iterator;
use std::slice::Iter;

use core::exception::Exception;
use core::env::EnvPtr;

#[derive(Debug)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(ListKind),
    ClosureValue(FuncKind, Option<String>, FuncParam, EnvPtr), // (body, funcname, param, env)
    NilValue,
    MapValue(HashMap<String, ValuePtr>, ValuePtr), // (map, extra_map), extra_map must be MapValue or NilValue
    BooleanValue(bool),
    VectorValue(Vec<ValuePtr>),
}

#[derive(PartialEq, Debug)]
pub enum ListKind {
    ConsList(ValuePtr, ValuePtr), // (car, cdr), cdr must be ListValue
    EmptyList,
}

impl ValueKind {
    pub fn as_type_str(&self) -> &'static str {
        use self::ValueKind::*;
        match self {
            &IntegerValue(_) => ValueKind::type_str_integer(),
            &StringValue(_) => ValueKind::type_str_string(),
            &SymbolValue(_) => ValueKind::type_str_symbol(),
            &KeywordValue(_) => ValueKind::type_str_keyword(),
            &ListValue(_) => ValueKind::type_str_list(),
            &ClosureValue(_, _, _, _) => ValueKind::type_str_closure(),
            &NilValue => ValueKind::type_str_nil(),
            &MapValue(_, _) => ValueKind::type_str_map(),
            &BooleanValue(_) => ValueKind::type_str_boolean(),
            &VectorValue(_) => ValueKind::type_str_vector(),
        }
    }

    pub fn type_str_integer() -> &'static str { "Integer" }
    pub fn type_str_string() -> &'static str { "String" }
    pub fn type_str_symbol() -> &'static str { "Symbol" }
    pub fn type_str_keyword() -> &'static str { "Keyword" }
    pub fn type_str_list() -> &'static str { "List" }
    pub fn type_str_closure() -> &'static str { "Closure" }
    pub fn type_str_nil() -> &'static str { "Nil" }
    pub fn type_str_map() -> &'static str { "Map" }
    pub fn type_str_boolean() -> &'static str { "Boolean" }
    pub fn type_str_vector() -> &'static str { "Vector" }

    pub fn is_symbol(&self) -> bool {
        match self {
            &ValueKind::SymbolValue(_) => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            &ValueKind::ListValue(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            &ValueKind::NilValue => true,
            _ => false,
        }
    }

    pub fn is_map(&self) -> bool {
        match self {
            &ValueKind::MapValue(_, _) => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        match self {
            &ValueKind::VectorValue(_) => true,
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

    pub fn matches_symbol(&self, expected: &str) -> bool {
        match self {
            &ValueKind::SymbolValue(ref actual) => expected == actual.as_str(),
            _ => false,
        }
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
            (&ListValue(ref lhs), &ListValue(ref rhs)) => lhs == rhs,
            (&NilValue, &NilValue) => true,
            (&MapValue(_, _), &MapValue(_, _)) => self.flatten_map() == other.flatten_map(),
            (&BooleanValue(ref lhs), &BooleanValue(ref rhs)) => lhs == rhs,
            (&VectorValue(ref lhs), &VectorValue(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

pub type BuitinFuncType = Fn(EnvPtr) -> Result<ValuePtr, Exception>;

pub enum FuncKind {
    AstFunc(ValuePtr),
    BuiltinFunc(Box<BuitinFuncType>),
}

#[derive(Debug)]
pub struct FuncParam {
    pub params: Vec<String>,
    pub rest_param: Option<String>,
}

impl FuncParam {
    pub fn new(params: Vec<String>, rest_param: Option<String>) -> FuncParam {
        FuncParam {
            params: params,
            rest_param: rest_param,
        }
    }
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

    pub fn create_list(list: ListKind) -> ValuePtr {
        if let ListKind::ConsList(_, ref cdr) = list {
            assert!(cdr.kind.is_list());
        }
        Value::new(ValueKind::ListValue(list))
    }

    pub fn create_list_from_vec(mut values: Vec<ValuePtr>) -> ValuePtr {
        use self::ListKind::*;
        let mut list = Value::create_list(EmptyList);
        while let Some(value) = values.pop() {
            list = Value::create_list(ConsList(value, list));
        }
        list
    }

    pub fn create_closure(func: FuncKind, name: Option<String>, param: FuncParam, env: EnvPtr) -> ValuePtr {
        Value::new(ValueKind::ClosureValue(func, name, param, env))
    }

    pub fn create_nil() -> ValuePtr {
        Value::new(ValueKind::NilValue)
    }

    pub fn create_map(map: HashMap<String, ValuePtr>, extra_map: ValuePtr) -> ValuePtr {
        assert!(extra_map.kind.is_map() || extra_map.kind.is_nil());
        Value::new(ValueKind::MapValue(map, extra_map))
    }

    pub fn create_boolean(boolean: bool) -> ValuePtr {
        Value::new(ValueKind::BooleanValue(boolean))
    }

    pub fn create_vector(vector: Vec<ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::VectorValue(vector))
    }

    pub fn iter(target: &ValuePtr) -> ValueIterator {
        use self::ValueIteratorKind::*;
        let iter = match target.kind {
            ValueKind::ListValue(_) => ListIterator(target.clone()),
            ValueKind::VectorValue(ref vector) => VectorIterator(vector.iter()),
            _ => unimplemented!(),
        };
        ValueIterator(iter)
    }

    pub fn get_as_symbol<'a>(&'a self) -> Option<&'a String> {
        match self.kind {
            ValueKind::SymbolValue(ref symbol) => Some(symbol),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ValueIteratorKind<'a> {
    ListIterator(ValuePtr),
    VectorIterator(Iter<'a, ValuePtr>),
}

#[derive(Debug)]
pub struct ValueIterator<'a>(ValueIteratorKind<'a>);

impl<'a> ValueIterator<'a> {
    pub fn rest(&mut self) -> ValuePtr {
        use self::ValueIteratorKind::*;
        let rest_val = match self.0 {
            ListIterator(ref cur) => cur.clone(),
            VectorIterator(ref mut iter) => {
                let mut rest_val = vec![];
                while let Some(val) = iter.next() {
                    rest_val.push(val.clone());
                }
                return Value::create_vector(rest_val);
            }
        };
        self.0 = ListIterator(Value::create_list(ListKind::EmptyList));
        rest_val
    }
}

impl<'a> Iterator for ValueIterator<'a> {
    type Item = ValuePtr;

    fn next(&mut self) -> Option<ValuePtr> {
        use self::ValueIteratorKind::*;
        let (val, next_cur) = match self.0 {
            VectorIterator(ref mut iter) => return match iter.next() {
                Some(ref val) => Some((*val).clone()),
                None => None,
            },
            ListIterator(ref cur) => match cur.kind {
                ValueKind::ListValue(ListKind::EmptyList) => return None,
                ValueKind::ListValue(ListKind::ConsList(ref car, ref cdr)) => {
                    assert!(cdr.kind.is_list());
                    (Some(car.clone()), cdr.clone())
                }
                _ => unimplemented!(),
            },
        };
        self.0 = ListIterator(next_cur);
        val
    }
}

impl Index<usize> for Value {
    type Output = ValuePtr;

    fn index(&self, index: usize) -> &ValuePtr {
        use self::ValueKind::*;
        match self.kind {
            ListValue(ListKind::EmptyList) => panic!(),
            ListValue(ListKind::ConsList(ref car, _)) if index == 0 => car,
            ListValue(ListKind::ConsList(_, ref cdr)) if index > 0 && cdr.kind.is_list() => &cdr[index - 1],
            ListValue(_) => panic!(),
            VectorValue(ref vector) => &vector[index],
            _ => panic!(),
        }
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

    #[test]
    fn test_iterator() {
        {
            let list_val = Value::create_list_from_vec(vec![
                Value::create_integer(1),
                Value::create_integer(2),
                Value::create_integer(3),
            ]);
            let mut iter = Value::iter(&list_val);
            assert_eq!(iter.next(), Some(Value::create_integer(1)));
            assert_eq!(iter.rest(), Value::create_list_from_vec(vec![
                Value::create_integer(2),
                Value::create_integer(3),
            ]));
            assert_eq!(iter.next(), None);
            assert_eq!(list_val, Value::create_list_from_vec(vec![
                Value::create_integer(1),
                Value::create_integer(2),
                Value::create_integer(3),
            ]));
        }
    }
}
