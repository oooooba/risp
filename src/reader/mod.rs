mod tokenizer;
mod parser;

use std::io;
use std::collections::LinkedList;

use core::value::ValuePtr;
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;
use core::exception::InfoKind;

#[derive(PartialEq, Debug)]
enum TokenKind {
    IntegerToken,
    StringToken,
    SymbolToken,
    KeywordToken,
    TrueToken,
    FalseToken,
    LParenToken,
    RParenToken,
    LBracketToken,
    RBracketToken,
    AmpToken,
    QuoteToken,
    BackQuoteToken,
    TildeToken,
    LCurlyToken,
    RCurlyToken,
}

#[derive(PartialEq, Debug)]
pub struct Token {
    lexeme: String,
    kind: TokenKind,
    info: Option<InfoKind>,
}

impl Token {
    fn new(lexeme: String, kind: TokenKind, info: Option<InfoKind>) -> Token {
        Token { lexeme: lexeme, kind: kind, info: info }
    }
}

pub fn tokenize(content: String, env: EnvPtr) -> Result<LinkedList<Token>, Exception> {
    tokenizer::Tokenizer::new(content, env).tokenize()
}

pub fn parse(tokens: LinkedList<Token>) -> (Result<ValuePtr, Exception>, Option<LinkedList<Token>>) {
    let mut parser = parser::Parser::new(tokens);
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
