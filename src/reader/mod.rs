mod tokenizer;
mod parser;

use std::io;
use std::rc::Rc;

use value::{Value, ValueKind, Exception, ExceptionKind};
use self::tokenizer::Tokenizer;
use self::parser::Parser;

pub fn read() -> Result<Value, Exception> {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).map_err(|error|
        Exception::new(ExceptionKind::ReaderIOException(error.to_string()), None))
        .and_then(|n| if n == 0 {
            Err(Exception::new(ExceptionKind::ReaderEndOfInputException, None))
        } else {
            Ok(buf)
        })
        .and_then(|buf| Tokenizer::new(buf).tokenize())
        .and_then(|tokens| match Rc::try_unwrap(tokens).unwrap_or_else(
            |_| unreachable!()) {
            ValueKind::ListValue(tokens) => Ok(tokens),
            _ => unreachable!()
        })
        .and_then(|tokens| Parser::new(tokens).parse())
}
