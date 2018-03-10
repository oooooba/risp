mod tokenizer;
mod tokenizer2;
mod parser;

use std::io;
use std::collections::LinkedList;

use core::value::ValuePtr;
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;
use core::exception::InfoKind;
use self::tokenizer::Tokenizer;
use self::parser::Parser;

#[derive(PartialEq, Debug)]
enum TokenKind{
    IntegerToken,
    StringToken,
    SymbolToken,
    KeywordToken,
    LParenToken,
    RParenToken,
    LBracketToken,
    RBracketToken,
    AmpToken,
}

#[derive(PartialEq, Debug)]
pub struct Token{
    lexeme: String,
    kind: TokenKind,
    info: Option<InfoKind>,
}

impl Token{
    fn new(lexeme: String, kind: TokenKind, info: Option<InfoKind>)->Token{
        Token{ lexeme: lexeme, kind: kind, info: info }
    }
}

pub fn tokenize2(content: String, env: EnvPtr) -> Result<LinkedList<Token>, Exception> {
    tokenizer2::Tokenizer::new(content, env).tokenize()
}

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
