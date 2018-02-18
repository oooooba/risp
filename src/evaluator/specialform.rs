use core::value::{ValueKind, ValuePtr};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};
use evaluator::eval;

pub fn eval_specialform_let(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    use self::ValueKind::*;
    assert!(ast.kind.is_list());
    let (bindings, rest) = match ast.kind {
        ListValue(ref car, ref cdr) => {
            match car.kind {
                VectorValue(ref bindings) => (bindings, cdr),
                _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), ast.kind.as_type_str()), None)),
            }
        }
        _ => unreachable!(),
    };
    if bindings.len() % 2 != 0 {
        return Err(Exception::new(ExceptionKind::EvaluatorArityException(bindings.len() + 1, bindings.len()), None));
    }

    let mut pairs = vec![];
    for i in 0..(bindings.len() / 2) {
        let key_i = i * 2;
        let key = match bindings[key_i].kind {
            SymbolValue(ref s) => s.clone(),
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_symbol(), bindings[key_i].kind.as_type_str()), None)),
        };
        let val_i = i * 2 + 1;
        let val = eval(bindings[val_i].clone(), Env::create(pairs.clone(), Some(env.clone())))?;
        pairs.push((key, val));
    }
    let let_env = Env::create(pairs, Some(env));

    let body = match rest.kind {
        ListValue(ref car, ref cdr) => {
            assert!(cdr.kind.is_nil());
            car
        }
        _ => unreachable!(),
    };
    eval(body.clone(), let_env)
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::value::*;

    #[test]
    fn test_specialform_let() {
        let env = Env::create_default();
        assert_eq!(eval(Value::create_list_from_vec(vec![
            Value::create_symbol("let".to_string()),
            Value::create_vector(vec![
                Value::create_symbol("x".to_string()),
                Value::create_integer(1),
                Value::create_symbol("y".to_string()),
                Value::create_list_from_vec(vec![
                    Value::create_symbol("+".to_string()),
                    Value::create_symbol("x".to_string()),
                    Value::create_integer(2),
                ]),
            ]),
            Value::create_symbol("y".to_string()),
        ]), env), Ok(Value::create_integer(3)));
    }
}
