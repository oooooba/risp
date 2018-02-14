pub mod value;
pub mod exception;
pub mod env;

use core::value::ValuePtr;
use core::exception::Exception;
use core::env::EnvPtr;
use reader::parse;
use evaluator::eval;

pub fn parse_and_eval(tokens: ValuePtr, env: EnvPtr) -> Result<(ValuePtr, Option<ValuePtr>), Exception> {
    assert!(tokens.kind.is_list() || tokens.kind.is_nil());
    let (ast, rest_tokens) = parse(tokens)?;
    let val = eval(ast, env)?;
    Ok((val, rest_tokens))
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::value::Value;
    use core::env::Env;

    #[test]
    fn test_acceptance() {
        let env = Env::create_default();
        assert_eq!(parse_and_eval(Value::create_list_from_vec(vec![
            Value::create_keyword("(".to_string()),
            Value::create_symbol("+".to_string()),
            Value::create_integer(1),
            Value::create_integer(2),
            Value::create_keyword(")".to_string()),
            Value::create_boolean(true),
        ]), env), Ok((Value::create_integer(3), Some(Value::create_list_from_vec(vec![
            Value::create_boolean(true),
        ])))));
    }
}
