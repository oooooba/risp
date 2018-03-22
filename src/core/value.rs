use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;
use std::ops::Index;
use std::iter::Iterator;
use std::slice::Iter;
use std::string::ToString;
use std::hash::{Hash, Hasher};

use core::exception::Exception;
use core::env::EnvPtr;

#[derive(Debug, Eq)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(ListKind),
    ClosureValue(FuncKind, Option<String>, FuncParam, EnvPtr), // (body, funcname, param, env)
    NilValue,
    MapValue(HashMap<ValuePtr, ValuePtr>),
    BooleanValue(bool),
    VectorValue(Vec<ValuePtr>),
}

#[derive(PartialEq, Debug, Eq, Hash)]
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
            &MapValue(_) => ValueKind::type_str_map(),
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
            &ValueKind::MapValue(_) => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        match self {
            &ValueKind::VectorValue(_) => true,
            _ => false,
        }
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
            (&MapValue(ref lhs), &MapValue(ref rhs)) => lhs == rhs,
            (&BooleanValue(ref lhs), &BooleanValue(ref rhs)) => lhs == rhs,
            (&VectorValue(ref lhs), &VectorValue(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl ToString for ValueKind {
    fn to_string(&self) -> String {
        use self::ValueKind::*;
        match self {
            &IntegerValue(ref n) => n.to_string(),
            &StringValue(ref s) => format!(r#""{}""#, s),
            &SymbolValue(ref s) => s.clone(),
            &KeywordValue(ref k) => format!(":{}", k),
            &ListValue(ref l) => {
                use self::ListKind::*;
                let mut text = String::new();
                text.push('(');
                let mut iter = l;
                let mut is_first = true;
                while let &ConsList(ref car, ref cdr) = iter {
                    if is_first {
                        is_first = false;
                    } else {
                        text.push(' ');
                    }
                    text.push_str(&car.to_string());
                    iter = match cdr.kind {
                        ListValue(ref l) => l,
                        _ => unreachable!(),
                    }
                }
                text.push(')');
                text
            }
            &ClosureValue(ref f, ref n, ref p, ref e) => {
                use std::mem::transmute;
                let mut text = String::new();
                text.push('<');
                text.push_str(&format!("{:x}", unsafe { transmute::<&FuncKind, usize>(f) }));
                text.push_str(", ");
                match n {
                    &Some(ref name) => text.push_str(name),
                    &None => text.push_str("<anon>"),
                }
                text.push_str(", ");
                text.push_str(&format!("{:x}", unsafe { transmute::<&FuncParam, usize>(p) }));
                text.push_str(", ");
                text.push_str(&format!("{:x}", unsafe { transmute::<&EnvPtr, usize>(e) }));
                text.push('>');
                text
            }
            &NilValue => "nil".to_string(),
            &MapValue(ref m) => {
                let mut text = String::new();
                text.push('{');
                let mut is_first = true;
                for (key, val) in m.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        text.push_str(", ");
                    }
                    text.push_str(&key.to_string());
                    text.push(' ');
                    text.push_str(&val.to_string());
                }
                text.push('}');
                text
            }
            &BooleanValue(ref b) => b.to_string(),
            &VectorValue(ref v) => {
                let mut text = String::new();
                text.push('[');
                let mut is_first = true;
                for item in v.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        text.push(' ');
                    }
                    text.push_str(&item.to_string());
                }
                text.push(']');
                text
            }
        }
    }
}

impl Hash for ValueKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

pub type BuiltinFuncType = Fn(EnvPtr) -> Result<ValuePtr, Exception>;

pub enum FuncKind {
    AstFunc(ValuePtr),
    BuiltinFunc(Box<BuiltinFuncType>),
}

impl Eq for FuncKind {}

impl PartialEq for FuncKind {
    fn eq(&self, other: &FuncKind) -> bool {
        use self::FuncKind::*;
        use std::mem::transmute;
        match (self, other) {
            (&AstFunc(ref lhs), &AstFunc(ref rhs)) => lhs == rhs,
            (&BuiltinFunc(ref lhs), &BuiltinFunc(ref rhs)) => {
                unsafe {
                    let addr_lhs: usize = transmute(lhs);
                    let addr_rhs: usize = transmute(rhs);
                    addr_lhs == addr_rhs
                }
            }
            _ => false,
        }
    }
}

impl Hash for FuncKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::FuncKind::*;
        use std::mem::transmute;
        match self {
            &AstFunc(ref f) => f.hash(state),
            &BuiltinFunc(ref f) => {
                let addr: usize = unsafe { transmute(f) };
                addr.hash(state);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
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

#[derive(PartialEq, Debug, Eq, Hash)]
pub struct Value {
    pub kind: ValueKind,
    pub is_macro: bool,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

pub type ValuePtr = Rc<Value>;

impl Value {
    fn new(kind: ValueKind) -> ValuePtr {
        Rc::new(Value { kind: kind, is_macro: false })
    }

    fn new_macro(kind: ValueKind) -> ValuePtr {
        Rc::new(Value { kind: kind, is_macro: true })
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

    pub fn create_closure_for_macro(func: FuncKind, name: Option<String>, param: FuncParam, env: EnvPtr) -> ValuePtr {
        Value::new_macro(ValueKind::ClosureValue(func, name, param, env))
    }

    pub fn create_nil() -> ValuePtr {
        Value::new(ValueKind::NilValue)
    }

    pub fn create_map(map: HashMap<ValuePtr, ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::MapValue(map))
    }

    pub fn create_map_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        assert_eq!(values.len() % 2, 0);
        let mut map = HashMap::new();
        for i in 0..(values.len() / 2) {
            let key = values[i * 2].clone();
            let val = values[i * 2 + 1].clone();
            map.insert(key, val);
        }
        Value::create_map(map)
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
