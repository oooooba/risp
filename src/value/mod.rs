use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub enum ValueKind {
    IntegerValue(isize),
    StringValue(String),
    SymbolValue(String),
    KeywordValue(String),
    ListValue(Vec<Value>),
}

#[derive(PartialEq, Debug)]
pub enum ExceptionKind {
    TokenizerInvalidLexemeException(String),
    TokenizerInvalidEscapedCharacterException(char, usize),
    TokenizerNonTerminatedStringException,
    ReaderIOException(String),
}

#[derive(PartialEq, Debug)]
pub enum InfoKind {
    TokenizerInfo(usize, usize),
}

pub type Value = Rc<ValueKind>;

#[derive(PartialEq, Debug)]
pub struct Exception {
    kind: ExceptionKind,
    info: Option<InfoKind>,
}

impl Exception {
    pub fn new(kind: ExceptionKind, info: Option<InfoKind>) -> Exception {
        Exception {
            kind: kind,
            info: info,
        }
    }
}

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

pub fn create_list_value(values: Vec<Value>) -> Value {
    Rc::new(ValueKind::ListValue(values))
}
