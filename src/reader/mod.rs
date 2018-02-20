mod tokenizer;
mod parser;

use std::io;

use core::value::ValuePtr;
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;
use self::tokenizer::Tokenizer;
use self::parser::Parser;

pub fn tokenize(content: String, env: EnvPtr) -> Result<ValuePtr, Exception> {
    Tokenizer::new(content, env).tokenize()
}

pub fn parse(tokens: ValuePtr) -> (Result<ValuePtr, Exception>, Option<ValuePtr>) {
    let mut parser = Parser::new(tokens);
    let result = parser.parse();
    let rest_tokens = parser.pop_all();
    (result, rest_tokens)
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
        .and_then(|buf| tokenize(buf, env))
        .and_then(|tokens| parse(tokens).0)
}
