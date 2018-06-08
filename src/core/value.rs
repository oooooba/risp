use std::rc::Rc;
use std::fmt;
use std::iter::Iterator;
use std::slice::Iter;
use std::string::ToString;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::cmp::Ordering;

use core::exception::Exception;
use core::env::EnvPtr;
use core::reserved;
use core::pair;
use core::pattern::PatternPtr;
use core::map;
use core::list;

#[derive(Debug, Eq)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(list::List<ValuePtr>),
    ClosureValue(Applicable, EnvPtr),
    NilValue,
    MapValue(map::TreeMap<ValuePtr, ValuePtr>),
    BooleanValue(bool),
    VectorValue(Vec<ValuePtr>),
    MacroValue(Applicable),
    TypeValue(TypePtr),
    InternalPairValue(pair::Pair<ValuePtr, ValuePtr>), // internal use
    SetValue(map::TreeMap<ValuePtr, ()>),
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
            &InternalPairValue(_) => unreachable!(),
            &SetValue(_) => ValueKind::type_str_set(),
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
    pub fn type_str_set() -> &'static str { "Set" }

    fn is_integer(&self) -> bool {
        match self {
            &ValueKind::IntegerValue(_) => true,
            _ => false,
        }
    }

    fn is_symbol(&self) -> bool {
        match self {
            &ValueKind::SymbolValue(_) => true,
            _ => false,
        }
    }

    fn is_keyword(&self) -> bool {
        match self {
            &ValueKind::KeywordValue(_) => true,
            _ => false,
        }
    }

    fn is_list(&self) -> bool {
        match self {
            &ValueKind::ListValue(_) => true,
            _ => false,
        }
    }

    fn is_closure(&self) -> bool {
        match self {
            &ValueKind::ClosureValue(_, _) => true,
            _ => false,
        }
    }

    fn is_nil(&self) -> bool {
        match self {
            &ValueKind::NilValue => true,
            _ => false,
        }
    }

    fn is_map(&self) -> bool {
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

    fn is_pair(&self) -> bool {
        match self {
            &ValueKind::InternalPairValue(_) => true,
            _ => false,
        }
    }

    fn is_set(&self) -> bool {
        match self {
            &ValueKind::SetValue(_) => true,
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
            (&InternalPairValue(ref lhs), &InternalPairValue(ref rhs)) => lhs == rhs,
            (&SetValue(ref lhs), &SetValue(ref rhs)) => lhs == rhs,
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
        let mut pairs = vec![];
        for i in 0..self.fields.len() {
            let field = &self.fields[i].0;
            let key = Value::create_keyword(field.clone());
            let value = values[i].clone();
            pairs.push((key, value));
        }
        Value::create_map(pairs)
    }
}

pub fn constructor(typ: TypePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    let args = env.lookup(&":_args".to_string()).unwrap();
    let arg_vec = args.iter().collect();
    Ok(typ.instantiate(arg_vec))
}

pub type TypePtr = Rc<Type>;

#[derive(PartialEq, Debug, Eq)]
pub struct Value {
    pub kind: ValueKind,
    pub is_literal: bool,
}

#[derive(Debug, Eq, Clone)]
pub struct ValuePtr(Rc<Value>);

impl PartialEq for ValuePtr {
    fn eq(&self, other: &Self) -> bool {
        use self::ValueKind::*;
        match (&self.0.kind, &other.0.kind) {
            (&ListValue(_), &VectorValue(_)) => (),
            (&VectorValue(_), &ListValue(_)) => (),
            _ => return self.0 == other.0,
        }
        let mut lhs_iter = self.iter();
        let mut rhs_iter = other.iter();
        loop {
            match (lhs_iter.next(), rhs_iter.next()) {
                (None, None) => return true,
                (Some(ref lhs_item), Some(ref rhs_item)) if lhs_item == rhs_item => (),
                (_, _) => return false,
            }
        }
    }
}

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
            MapValue(_) => to_string_helper(reserved::STR__L_CURLY_, reserved::STR__R_CURLY_,
                                            ", ", self.iter()),
            BooleanValue(ref b) => (if *b { reserved::STR_TRUE } else { reserved::STR_FALSE }).to_string(),
            VectorValue(_) => to_string_helper(reserved::STR__L_BRACKET_, reserved::STR__R_BRACKET_,
                                               " ", self.iter()),
            MacroValue(_) => unimplemented!(),
            TypeValue(_) => unimplemented!(),
            InternalPairValue(ref p) => format!("{} {}", p.first.to_string(), p.second.to_string()),
            SetValue(_) => to_string_helper(reserved::STR__SHARP__L_CURLY_, reserved::STR__R_CURLY_,
                                            ", ", self.iter()),
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

impl PartialOrd for ValuePtr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn cmp_helper(mut self_iter: ValueIterator, mut other_iter: ValueIterator) -> Ordering {
    loop {
        let self_item = self_iter.next();
        let other_item = other_iter.next();
        match (self_item.is_none(), other_item.is_none()) {
            (true, true) => return Ordering::Equal,
            (true, false) => return Ordering::Less,
            (false, true) => return Ordering::Greater,
            (false, false) => (),
        }
        let b = self_item.unwrap().cmp(&other_item.unwrap());
        if b != Ordering::Equal {
            return b;
        }
    }
}

impl Ord for ValuePtr {
    fn cmp(&self, other: &Self) -> Ordering {
        use self::ValueKind::*;
        fn to_int(kind: &ValueKind) -> usize {
            match kind {
                &IntegerValue(_) => 1,
                &StringValue(_) => 2,
                &SymbolValue(_) => 3,
                &KeywordValue(_) => 4,
                &ListValue(_) => 5,
                &ClosureValue(_, _) => 6,
                &NilValue => 7,
                &MapValue(_) => 8,
                &BooleanValue(_) => 9,
                &VectorValue(_) => 10,
                &MacroValue(_) => 11,
                &TypeValue(_) => 12,
                &InternalPairValue(_) => 13,
                &SetValue(_) => 14,
            }
        }

        let self_int = to_int(&self.kind);
        let other_int = to_int(&other.kind);
        if self_int != other_int {
            return self_int.cmp(&other_int);
        }

        assert_eq!(self_int, other_int);
        if self == other {
            return Ordering::Equal;
        }

        match (&self.kind, &other.kind) {
            (&IntegerValue(ref lhs), &IntegerValue(ref rhs)) => lhs.cmp(rhs),
            (&StringValue(ref lhs), &StringValue(ref rhs)) => lhs.cmp(rhs),
            (&SymbolValue(ref lhs), &SymbolValue(ref rhs)) => lhs.cmp(rhs),
            (&KeywordValue(ref lhs), &KeywordValue(ref rhs)) => lhs.cmp(rhs),
            (&ClosureValue(_, _), &ClosureValue(_, _)) => unimplemented!(),
            (&ListValue(_), &ListValue(_)) => cmp_helper(self.iter(), other.iter()),
            (&NilValue, &NilValue) => unreachable!(),
            (&MapValue(_), &MapValue(_)) => unimplemented!(),
            (&BooleanValue(ref lhs), &BooleanValue(ref rhs)) => lhs.cmp(rhs),
            (&VectorValue(_), &VectorValue(_)) => cmp_helper(self.iter(), other.iter()),
            (&TypeValue(_), &TypeValue(_)) => unimplemented!(),
            (&SetValue(_), &SetValue(_)) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}

impl ValuePtr {
    pub fn wrap(value: Value) -> ValuePtr {
        ValuePtr(Rc::new(value))
    }

    pub fn iter(&self) -> ValueIterator {
        use self::ValueIteratorKind::*;
        let iterator = match self.kind {
            ValueKind::ListValue(ref list) => ListIterator(list.iter()),
            ValueKind::VectorValue(ref vector) => VectorIterator(vector.iter()),
            ValueKind::MapValue(ref map) => MapIterator(map.iter()),
            ValueKind::SetValue(ref set) => SetIterator(set.iter()),
            _ => unimplemented!(),
        };
        ValueIterator(iterator)
    }

    pub fn is_integer(&self) -> bool {
        self.kind.is_integer()
    }

    pub fn is_symbol(&self) -> bool {
        self.kind.is_symbol()
    }

    pub fn is_keyword(&self) -> bool {
        self.kind.is_keyword()
    }

    pub fn is_list(&self) -> bool {
        self.kind.is_list()
    }

    pub fn is_closure(&self) -> bool {
        self.kind.is_closure()
    }

    pub fn is_nil(&self) -> bool {
        self.kind.is_nil()
    }

    pub fn is_map(&self) -> bool {
        self.kind.is_map()
    }

    pub fn is_pair(&self) -> bool {
        self.kind.is_pair()
    }

    pub fn is_set(&self) -> bool {
        self.kind.is_set()
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

    pub fn create_list(list: list::List<ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::ListValue(list))
    }

    pub fn create_list_empty() -> ValuePtr {
        Value::create_list(list::List::create_empty())
    }

    pub fn create_list_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        Value::create_list(list::List::create(values))
    }

    pub fn create_closure(applicable: Applicable, env: EnvPtr) -> ValuePtr {
        Value::new(ValueKind::ClosureValue(applicable, env))
    }

    pub fn create_nil() -> ValuePtr {
        Value::new(ValueKind::NilValue)
    }

    fn build_pairs_from_vec(values: Vec<ValuePtr>) -> Vec<(ValuePtr, ValuePtr)> {
        assert_eq!(values.len() % 2, 0);
        let mut pairs = vec![];
        for i in 0..(values.len() / 2) {
            let key = values[i * 2].clone();
            let val = values[i * 2 + 1].clone();
            pairs.push((key, val));
        }
        pairs
    }

    pub fn create_map_raw(map: map::TreeMap<ValuePtr, ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::MapValue(map))
    }

    pub fn create_map(pairs: Vec<(ValuePtr, ValuePtr)>) -> ValuePtr {
        Value::new(ValueKind::MapValue(map::TreeMap::create(pairs)))
    }

    pub fn create_map_literal(pairs: Vec<(ValuePtr, ValuePtr)>) -> ValuePtr {
        Value::new_literal(ValueKind::MapValue(map::TreeMap::create(pairs)))
    }

    pub fn create_map_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        let pairs = Value::build_pairs_from_vec(values);
        Value::create_map(pairs)
    }

    pub fn create_map_literal_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        let pairs = Value::build_pairs_from_vec(values);
        Value::create_map_literal(pairs)
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

    pub fn create_pair(pair: pair::Pair<ValuePtr, ValuePtr>) -> ValuePtr {
        Value::new(ValueKind::InternalPairValue(pair))
    }

    pub fn create_set(set: map::TreeMap<ValuePtr, ()>) -> ValuePtr {
        Value::new(ValueKind::SetValue(set))
    }

    pub fn create_set_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        let pairs = values.iter().map(|v| (v.clone(), ())).collect();
        Value::create_set(map::TreeMap::create(pairs))
    }

    pub fn create_set_literal_from_vec(values: Vec<ValuePtr>) -> ValuePtr {
        let pairs = values.iter().map(|v| (v.clone(), ())).collect();
        Value::new_literal(ValueKind::SetValue(map::TreeMap::create(pairs)))
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

    pub fn get_as_list<'a>(&'a self) -> Option<&'a list::List<ValuePtr>> {
        match self.kind {
            ValueKind::ListValue(ref list) => Some(list),
            _ => None,
        }
    }

    pub fn get_as_map<'a>(&'a self) -> Option<&'a map::TreeMap<ValuePtr, ValuePtr>> {
        match self.kind {
            ValueKind::MapValue(ref map) => Some(map),
            _ => None,
        }
    }

    pub fn get_as_pair<'a>(&'a self) -> Option<&'a pair::Pair<ValuePtr, ValuePtr>> {
        match self.kind {
            ValueKind::InternalPairValue(ref pair) => Some(pair),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ValueIteratorKind<'a> {
    ListIterator(list::ListIterator<ValuePtr>),
    VectorIterator(Iter<'a, ValuePtr>),
    MapIterator(map::TreeMapIterator<ValuePtr, ValuePtr>),
    SetIterator(map::TreeMapIterator<ValuePtr, ()>),
}

#[derive(Debug)]
pub struct ValueIterator<'a>(ValueIteratorKind<'a>);

impl<'a> ValueIterator<'a> {
    pub fn rest(&mut self) -> ValuePtr {
        use self::ValueIteratorKind::*;
        match self.0 {
            ListIterator(ref mut iter) => {
                let mut rest_val = vec![];
                while let Some(val) = iter.next() {
                    rest_val.push(val.clone());
                }
                Value::create_list_from_vec(rest_val)
            }
            VectorIterator(ref mut iter) => {
                let mut rest_val = vec![];
                while let Some(val) = iter.next() {
                    rest_val.push(val.clone());
                }
                Value::create_vector(rest_val)
            }
            MapIterator(ref mut iter) => Value::create_vector(iter.map(|p| {
                Value::create_pair(p)
            }).collect()),
            SetIterator(ref mut iter) => {
                let mut rest_val = vec![];
                while let Some(val) = iter.next() {
                    rest_val.push(val.first);
                }
                Value::create_vector(rest_val)
            }
        }
    }
}

impl<'a> Iterator for ValueIterator<'a> {
    type Item = ValuePtr;

    fn next(&mut self) -> Option<ValuePtr> {
        use self::ValueIteratorKind::*;
        match self.0 {
            VectorIterator(ref mut iter) => match iter.next() {
                Some(ref val) => Some((*val).clone()),
                None => None,
            },
            MapIterator(ref mut iter) => match iter.next() {
                Some(pair) => Some(Value::create_pair(pair)),
                None => None,
            },
            ListIterator(ref mut iter) => iter.next(),
            SetIterator(ref mut iter) => match iter.next() {
                Some(pair) => Some(pair.first),
                None => None,
            },
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
            let mut iter = list_val.iter();
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
            let map_val = Value::create_map(vec![
                (Value::create_keyword("a".to_string()), Value::create_integer(1)),
            ]);
            let mut iter = map_val.iter();
            assert_eq!(iter.next(), Some(Value::create_pair(
                pair::Pair::new(Value::create_keyword("a".to_string()), Value::create_integer(1)))));
            assert_eq!(iter.next(), None);
            assert_eq!(map_val, Value::create_map(vec![
                (Value::create_keyword("a".to_string()), Value::create_integer(1)),
            ]));
        }
        {
            let map_val = Value::create_map(vec![
                (Value::create_keyword("a".to_string()), Value::create_integer(1)),
                (Value::create_keyword("b".to_string()), Value::create_integer(2)),
                (Value::create_keyword("c".to_string()), Value::create_integer(3)),
                (Value::create_keyword("d".to_string()), Value::create_integer(4)),
            ]);
            let mut iter = map_val.iter();
            assert_eq!(iter.next(), Some(Value::create_pair(pair::Pair::new(Value::create_keyword("a".to_string()), Value::create_integer(1)))));
            assert_eq!(iter.rest(), Value::create_vector(vec![
                Value::create_pair(pair::Pair::new(Value::create_keyword("b".to_string()), Value::create_integer(2))),
                Value::create_pair(pair::Pair::new(Value::create_keyword("c".to_string()), Value::create_integer(3))),
                Value::create_pair(pair::Pair::new(Value::create_keyword("d".to_string()), Value::create_integer(4))),
            ]));
            assert_eq!(iter.next(), None);
            assert_eq!(map_val, Value::create_map(vec![
                (Value::create_keyword("a".to_string()), Value::create_integer(1)),
                (Value::create_keyword("b".to_string()), Value::create_integer(2)),
                (Value::create_keyword("c".to_string()), Value::create_integer(3)),
                (Value::create_keyword("d".to_string()), Value::create_integer(4)),
            ]));
        }
    }

    #[test]
    fn test_comparison() {
        use std::cmp::Ordering::*;
        assert_eq!(Value::create_integer(1).cmp(&Value::create_integer(2)), Less);
        assert_eq!(Value::create_integer(2).cmp(&Value::create_integer(2)), Equal);
        assert_eq!(Value::create_integer(2).cmp(&Value::create_integer(1)), Greater);
        assert_eq!(Value::create_symbol("ab".to_string()).cmp(&Value::create_symbol("abc".to_string())), Less);
        assert_eq!(Value::create_symbol("ab".to_string()).cmp(&Value::create_symbol("a".to_string())), Greater);
        assert_eq!(Value::create_integer(9).cmp(&Value::create_symbol("1".to_string())), Less);
        assert_eq!(Value::create_list_from_vec(vec![]).cmp(&Value::create_list_from_vec(vec![])), Equal);
        assert_eq!(Value::create_list_from_vec(vec![]).cmp(&Value::create_list_from_vec(vec![Value::create_integer(1)])), Less);
        assert_eq!(Value::create_vector(vec![
            Value::create_integer(3), Value::create_integer(4),
        ]).cmp(&Value::create_vector(vec![
            Value::create_integer(1), Value::create_integer(2),
        ])), Greater);
        assert_eq!(Value::create_vector(vec![
            Value::create_keyword("a".to_string()),
            Value::create_keyword("b".to_string()),
            Value::create_keyword("c".to_string()),
        ]), Value::create_list_from_vec(vec![
            Value::create_keyword("a".to_string()),
            Value::create_keyword("b".to_string()),
            Value::create_keyword("c".to_string()),
        ]));
    }
}
