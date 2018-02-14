mod tokenizer;
mod parser;

use std::io;

use core::value::ValuePtr;
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;
use self::tokenizer::Tokenizer;
use self::parser::Parser;

pub fn parse(tokens: ValuePtr) -> Result<(ValuePtr, Option<ValuePtr>), Exception> {
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    Ok((ast, parser.pop_all()))
}

pub fn read(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).map_err(|error|
        Exception::new(ExceptionKind::ReaderIOException(error.to_string()), None))
        .and_then(|n| if n == 0 {
            Err(Exception::new(ExceptionKind::ReaderEndOfInputException, None))
        } else {
            Ok(buf)
        })
        .and_then(|buf| Tokenizer::new(buf, env).tokenize())
        .and_then(|tokens| parse(tokens))
        .and_then(|pair| Ok(pair.0))
}
