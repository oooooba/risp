use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hash_map;
use std::fmt;
use std::ops::Index;
use std::iter::Iterator;
use std::slice::Iter;
use std::string::ToString;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use core::exception::Exception;
use core::env::EnvPtr;
use core::reserved;

#[derive(Debug, Eq)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(ListKind),
    ClosureValue(Applicable, EnvPtr),
    NilValue,
    MapValue(HashMap<ValuePtr, ValuePtr>),
    BooleanValue(bool),
    VectorValue(Vec<ValuePtr>),
    MacroValue(Applicable),
    TypeValue(TypePtr),
}

#[derive(PartialEq, Debug, Eq)]
pub enum ListKind {
    ConsList(ValuePtr, ValuePtr), // (car, cdr), cdr must be ListValue
    EmptyList,
}

#[derive(PartialEq, Debug, Eq)]
pub struct Applicable {
    pub name: Option<String>,
    pub param: PatternPtr,
    pub body: ApplicableBodyKind,
}

impl Applicable {
    pub fn new(name: Option<String>, param: PatternPtr, body: ApplicableBodyKind) -> Applicable {
        Applicable {
            name: name,
            param: param,
            body: body,
        }
    }
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
            &ClosureValue(_, _) => ValueKind::type_str_closure(),
            &NilValue => ValueKind::type_str_nil(),
            &MapValue(_) => ValueKind::type_str_map(),
            &BooleanValue(_) => ValueKind::type_str_boolean(),
            &VectorValue(_) => ValueKind::type_str_vector(),
            &MacroValue(_) => unreachable!(),
            &TypeValue(_) => ValueKind::type_str_type(),
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
    pub fn type_str_type() -> &'static str { "Type" }

    pub fn is_integer(&self) -> bool {
        match self {
            &ValueKind::IntegerValue(_) => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            &ValueKind::SymbolValue(_) => true,
            _ => false,
        }
    }

    pub fn is_keyword(&self) -> bool {
        match self {
            &ValueKind::KeywordValue(_) => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            &ValueKind::ListValue(_) => true,
            _ => false,
        }
    }

    pub fn is_closure(&self) -> bool {
        match self {
            &ValueKind::ClosureValue(_, _) => true,
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

    pub fn is_macro(&self) -> bool {
        match self {
            &ValueKind::MacroValue(_) => true,
            _ => false,
        }
    }

    pub fn matches_symbol(&self, expected: &str) -> bool {
        match self {
            &ValueKind::SymbolValue(ref actual) => expected == actual.as_str(),
            _ => false,
        }
    }

    pub fn matches_keyword(&self, expected: &str) -> bool {
        match self {
            &ValueKind::KeywordValue(ref actual) => expected == actual.as_str(),
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
            (&TypeValue(ref lhs), &TypeValue(ref rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

pub type BuiltinFuncType = Fn(EnvPtr) -> Result<ValuePtr, Exception>;

pub enum ApplicableBodyKind {
    AstBody(ValuePtr),
    BuiltinBody(Box<BuiltinFuncType>),
}

impl Eq for ApplicableBodyKind {}

impl PartialEq for ApplicableBodyKind {
    fn eq(&self, other: &ApplicableBodyKind) -> bool {
        use self::ApplicableBodyKind::*;
        use std::mem::transmute;
        match (self, other) {
            (&AstBody(ref lhs), &AstBody(ref rhs)) => lhs == rhs,
            (&BuiltinBody(ref lhs), &BuiltinBody(ref rhs)) => {
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

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum PatternKind {
    SymbolPattern(ValuePtr),
    VectorPattern(Vec<PatternPtr>, Vec<PatternPtr>, Option<PatternPtr>),
    MapPattern(Vec<(PatternPtr, ValuePtr, Option<ValuePtr>)>, Option<PatternPtr>),
}

impl ToString for PatternKind {
    fn to_string(&self) -> String {
        use self::PatternKind::*;
        match self {
            &SymbolPattern(ref s) => s.to_string(),
            &VectorPattern(ref v, ref r, ref s) => {
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
                for p in r.iter() {
                    text.push_str(" & ");
                    text.push_str(&p.to_string());
                }
                if let &Some(ref symbol) = s {
                    text.push_str(" :as ");
                    text.push_str(&symbol.to_string());
                }
                text.push(']');
                text
            }
            &MapPattern(ref _v, ref _s) => unimplemented!(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct Pattern {
    pub kind: PatternKind,
}

pub type PatternPtr = Box<Pattern>;

impl Pattern {
    pub fn create_symbol(symbol: ValuePtr) -> PatternPtr {
        assert!(symbol.kind.is_symbol());
        Box::new(Pattern {
            kind: PatternKind::SymbolPattern(symbol),
        })
    }

    pub fn create_vector(patterns: Vec<PatternPtr>, rest_patterns: Vec<PatternPtr>,
                         as_symbol: Option<PatternPtr>) -> PatternPtr {
        Box::new(Pattern {
            kind: PatternKind::VectorPattern(patterns, rest_patterns, as_symbol),
        })
    }

    pub fn create_map(patterns: Vec<(PatternPtr, ValuePtr, Option<ValuePtr>)>,
                      as_symbol: Option<PatternPtr>) -> PatternPtr {
        Box::new(Pattern {
            kind: PatternKind::MapPattern(patterns, as_symbol),
        })
    }
}

impl ToString for Pattern {
    fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ApplicableParam {
    pub params: Vec<String>,
    pub rest_param: Option<String>,
}

impl ApplicableParam {
    pub fn new(params: Vec<String>, rest_param: Option<String>) -> ApplicableParam {
        ApplicableParam {
            params: params,
            rest_param: rest_param,
        }
    }
}

impl fmt::Debug for ApplicableBodyKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ApplicableBodyKind::*;
        match self {
            &AstBody(ref ast) => write!(f, "<AstFunc> :- {:?}", ast),
            &BuiltinBody(_) => write!(f, "<BuiltinFunc>"),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Type {
    fields: Vec<(String, Option<TypePtr>)>,
}

impl Type {
    pub fn create(fields: Vec<(String, Option<TypePtr>)>) -> TypePtr {
        Rc::new(Type { fields: fields })
    }

    pub fn instantiate(&self, values: Vec<ValuePtr>) -> ValuePtr {
        let mut map = HashMap::new();
        for i in 0..self.fields.len() {
            let field = &self.fields[i].0;
            let key = Value::create_keyword(field.clone());
            let value = values[i].clone();
            map.insert(key, value);
        }
        Value::create_map(map)
    }
}

pub fn constructor(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let type_name = env.lookup(&"ClassName".to_string()).unwrap();
    let args = env.lookup_nth_param(1).unwrap();

    let typ = match type_name.kind {
        ValueKind::TypeValue(ref typ) => typ,
        _ => unimplemented!(),
    };

    let mut arg_vec = vec![];
    for arg in Value::iter(args) {
        arg_vec.push(arg)
    }

    Ok(typ.instantiate(arg_vec))
}

pub type TypePtr = Rc<Type>;

#[derive(PartialEq, Debug, Eq)]
pub struct Value {
    pub kind: ValueKind,
    pub is_literal: bool,
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct ValuePtr(Rc<Value>);

fn to_string_helper(begin_str: &str, end_str: &str, delim_str: &str, mut iter: ValueIterator) -> String {
    let mut text = String::new();
    text.push_str(begin_str);
    if let Some(val) = iter.next() {
        text.push_str(&val.to_string());
        while let Some(val) = iter.next() {
            text.push_str(delim_str);
            text.push_str(&val.to_string());
        }
    }
    text.push_str(end_str);
    text
}

impl ToString for ValuePtr {
    fn to_string(&self) -> String {
        use self::ValueKind::*;
        match self.kind {
            IntegerValue(ref n) => n.to_string(),
            StringValue(ref s) => format!("{}{}{}", reserved::CHAR_D_QUOTE, s, reserved::CHAR_D_QUOTE),
            SymbolValue(ref s) => s.clone(),
            KeywordValue(ref k) => format!("{}{}", reserved::CHAR_COLON, k),
            ListValue(_) => to_string_helper(reserved::STR__L_PAREN_, reserved::STR__R_PAREN_,
                                             " ", self.iter()),
            ClosureValue(ref a, ref e) => {
                use std::mem::transmute;
                let mut text = String::new();
                text.push('<');
                text.push_str(&format!("{:x}", unsafe { transmute::<&ApplicableBodyKind, usize>(&a.body) }));
                text.push_str(", ");
                match a.name {
                    Some(ref name) => text.push_str(name),
                    None => text.push_str("<anon>"),
                }
                text.push_str(", ");
                text.push_str(&format!("{:x}", unsafe { transmute::<&PatternPtr, usize>(&a.param) }));
                text.push_str(", ");
                text.push_str(&format!("{:x}", unsafe { transmute::<&EnvPtr, usize>(e) }));
                text.push('>');
                text
            }
            NilValue => reserved::STR_NIL.to_string(),
            MapValue(ref m) => {
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
            BooleanValue(ref b) => (if *b { reserved::STR_TRUE } else { reserved::STR_FALSE }).to_string(),
            VectorValue(_) => to_string_helper(reserved::STR__L_BRACKET_, reserved::STR__R_BRACKET_,
                                             " ", self.iter()),
            MacroValue(_) => unimplemented!(),
            TypeValue(_) => unimplemented!(),
        }
    }
}

impl Deref for ValuePtr {
    type Target = Value;
    fn deref(&self) -> &Value {
        &self.0
    }
}

impl Hash for ValuePtr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl ValuePtr {
    pub fn wrap(value: Value) -> ValuePtr {
        ValuePtr(Rc::new(value))
    }

    pub fn iter(&self) -> ValueIterator {
        use self::ValueIteratorKind::*;
        let iterator = match self.kind {
            ValueKind::ListValue(_) => ListIterator(self.clone()),
            ValueKind::VectorValue(ref vector) => VectorIterator(vector.iter()),
            ValueKind::MapValue(ref map) => MapIterator(map.iter()),
            _ => unimplemented!(),
        };
        ValueIterator(iterator)
    }
}

impl Value {
    fn new(kind: ValueKind) -> ValuePtr {
        ValuePtr::wrap(Value { kind: kind, is_literal: false })
    }

    fn new_literal(kind: ValueKind) -> ValuePtr {
        ValuePtr::wrap(Value { kind: kind, is_literal: true })
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

    pub fn create_closure(applicable: Applicable, env: EnvPtr) -> ValuePtr {
        Value::new(ValueKind::ClosureValue(applicable, env))
    }

    pub fn create_nil() -> ValuePtr {
        Value::new(ValueKind::NilValue)
    }

    pub fn create_map(map: HashMap<ValuePtr, ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::MapValue(map))
    }

    pub fn create_map_literal(map: HashMap<ValuePtr, ValuePtr>) -> ValuePtr {
        Value::new_literal(ValueKind::MapValue(map))
    }

    fn build_map_from_vec(values: Vec<ValuePtr>) -> HashMap<ValuePtr, ValuePtr> {
        assert_eq!(values.len() % 2, 0);
        let mut map = HashMap::new();
        for i in 0..(values.len() / 2) {
            let key = values[i * 2].clone();
            let val = values[i * 2 + 1].clone();
            map.insert(key, val);
        }
        map
    }

    pub fn create_map_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        Value::create_map(Value::build_map_from_vec(values))
    }

    pub fn create_map_literal_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        Value::create_map_literal(Value::build_map_from_vec(values))
    }

    pub fn create_boolean(boolean: bool) -> ValuePtr {
        Value::new(ValueKind::BooleanValue(boolean))
    }

    pub fn create_vector(vector: Vec<ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::VectorValue(vector))
    }

    pub fn create_vector_literal(vector: Vec<ValuePtr>) -> ValuePtr {
        Value::new_literal(ValueKind::VectorValue(vector))
    }

    pub fn create_macro(applicable: Applicable) -> ValuePtr {
        Value::new(ValueKind::MacroValue(applicable))
    }

    pub fn create_type(typ: TypePtr) -> ValuePtr {
        Value::new(ValueKind::TypeValue(typ))
    }

    pub fn iter(target: &ValuePtr) -> ValueIterator {
        use self::ValueIteratorKind::*;
        let iter = match target.kind {
            ValueKind::ListValue(_) => ListIterator(target.clone()),
            ValueKind::VectorValue(ref vector) => VectorIterator(vector.iter()),
            ValueKind::MapValue(ref map) => MapIterator(map.iter()),
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

    pub fn get_as_integer<'a>(&'a self) -> Option<&'a isize> {
        match self.kind {
            ValueKind::IntegerValue(ref integer) => Some(integer),
            _ => None,
        }
    }

    pub fn get_as_map<'a>(&'a self) -> Option<&'a HashMap<ValuePtr, ValuePtr>> {
        match self.kind {
            ValueKind::MapValue(ref map) => Some(map),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ValueIteratorKind<'a> {
    ListIterator(ValuePtr),
    VectorIterator(Iter<'a, ValuePtr>),
    MapIterator(hash_map::Iter<'a, ValuePtr, ValuePtr>),
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
            MapIterator(ref mut iter) => {
                let mut rest_val = vec![];
                while let Some((ref key, ref val)) = iter.next() {
                    rest_val.push((*key).clone());
                    rest_val.push((*val).clone());
                }
                return Value::create_map_from_vec(rest_val);
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
            MapIterator(ref mut iter) => return match iter.next() {
                Some((ref key, ref val)) => Some(Value::create_list_from_vec(vec![
                    (*key).clone(), (*val).clone()
                ])),
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
        {
            let map_val = Value::create_map_from_vec(vec![
                Value::create_keyword("a".to_string()),
                Value::create_integer(1),
            ]);
            let mut iter = Value::iter(&map_val);
            assert_eq!(iter.next(), Some(Value::create_list_from_vec(vec![
                Value::create_keyword("a".to_string()),
                Value::create_integer(1),
            ])));
            assert_eq!(iter.next(), None);
            assert_eq!(map_val, Value::create_map_from_vec(vec![
                Value::create_keyword("a".to_string()),
                Value::create_integer(1),
            ]));
        }
        {
            let map_val = Value::create_map_from_vec(vec![
                Value::create_keyword("a".to_string()),
                Value::create_integer(1),
                Value::create_keyword("b".to_string()),
                Value::create_integer(2),
                Value::create_keyword("c".to_string()),
                Value::create_integer(3),
            ]);
            let mut iter = Value::iter(&map_val);
            assert_eq!(iter.rest(), Value::create_map_from_vec(vec![
                Value::create_keyword("a".to_string()),
                Value::create_integer(1),
                Value::create_keyword("b".to_string()),
                Value::create_integer(2),
                Value::create_keyword("c".to_string()),
                Value::create_integer(3),
            ]));
            assert_eq!(iter.next(), None);
            assert_eq!(map_val, Value::create_map_from_vec(vec![
                Value::create_keyword("a".to_string()),
                Value::create_integer(1),
                Value::create_keyword("b".to_string()),
                Value::create_integer(2),
                Value::create_keyword("c".to_string()),
                Value::create_integer(3),
            ]));
        }
    }
}
