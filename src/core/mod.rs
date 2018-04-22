pub mod value;
pub mod exception;
pub mod env;
pub mod reserved;
pub mod map;

use std::collections::LinkedList;

use core::value::ValuePtr;
use core::exception::Exception;
use core::env::EnvPtr;
use reader::{parse, Token};
use evaluator::eval;

pub fn parse_and_eval(tokens: LinkedList<Token>, env: EnvPtr) -> (Result<ValuePtr, Exception>, Option<LinkedList<Token>>) {
    let (ast, rest_tokens) = parse(tokens);
    let ast = match ast {
        Ok(ast) => ast,
        err @ Err(_) => return (err, rest_tokens),
    };
    let result = eval(ast, env);
    (result, rest_tokens)
}
