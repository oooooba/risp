pub mod env;
pub mod exception;
pub mod list;
pub mod map;
pub mod pair;
pub mod pattern;
pub mod reserved;
pub mod value;

use std::collections::LinkedList;

use super::evaluator::eval;
use super::reader::{parse, Token};
use env::EnvPtr;
use exception::Exception;
use value::ValuePtr;

pub fn parse_and_eval(
    tokens: LinkedList<Token>,
    env: EnvPtr,
) -> (Result<ValuePtr, Exception>, Option<LinkedList<Token>>) {
    let (ast, rest_tokens) = parse(tokens);
    let ast = match ast {
        Ok(ast) => ast,
        err @ Err(_) => return (err, rest_tokens),
    };
    let result = eval(ast, env);
    (result, rest_tokens)
}
