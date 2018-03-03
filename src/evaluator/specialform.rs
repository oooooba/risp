use core::value::{Value, ValueKind, ValuePtr, FuncKind, FuncParam};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};
use evaluator::eval;

pub fn eval_specialform_let(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    use self::ValueKind::*;
    assert!(ast.kind.is_pair());
    let (bindings, rest) = match ast.kind {
        PairValue(ref car, ref cdr) => {
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
        PairValue(ref car, ref cdr) => {
            assert!(cdr.kind.is_nil());
            car
        }
        _ => unreachable!(),
    };
    eval(body.clone(), let_env)
}

pub fn eval_specialform_quote(ast: &ValuePtr, _env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_pair());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("quote"));

    let val = match iter.next() {
        Some(val) => val,
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("quote"), None)),
    };

    assert_eq!(iter.next(), None);
    Ok(val)
}

pub fn eval_specialform_def(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_pair());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("def"));

    let symbol = match iter.next() {
        Some(ref symbol) if symbol.kind.is_symbol() => symbol.get_as_symbol().unwrap().clone(),
        Some(other) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_symbol(), other.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("def"), None)),
    };

    let body_expr = match iter.next() {
        Some(body_expr) => body_expr,
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("def"), None)),
    };
    let val = eval(body_expr, env.clone())?;

    assert_eq!(iter.next(), None);
    Err(Exception::new(ExceptionKind::Continuation(Env::create(vec![(symbol, val)], Some(env))), None))
}

pub fn eval_specialform_if(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_pair());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("if"));

    let cond_expr = match iter.next() {
        Some(cond_expr) => cond_expr,
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("if"), None)),
    };
    let true_expr = match iter.next() {
        Some(true_expr) => true_expr,
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("if"), None)),
    };
    let false_expr = match iter.next() {
        Some(false_expr) => false_expr,
        None => Value::create_nil(),
    };
    assert_eq!(iter.next(), None);

    let cond = eval(cond_expr, env.clone())?;
    match cond.kind {
        ValueKind::BooleanValue(false) => eval(false_expr, env),
        ValueKind::NilValue => eval(false_expr, env),
        _ => eval(true_expr, env),
    }
}

fn parse_fn_param_vec(param_vec: &ValuePtr) -> Result<FuncParam, Exception> {
    let mut params = vec![];
    let mut declares_rest_param = false;
    let mut iter = Value::iter(param_vec);

    while let Some(param) = iter.next() {
        match param.kind {
            ValueKind::SymbolValue(ref symbol) => params.push(symbol.clone()),
            ValueKind::KeywordValue(ref keyword) if keyword == "&" => {
                declares_rest_param = true;
                break;
            }
            _ => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
        }
    }

    let mut rest_param = None;
    if declares_rest_param {
        match iter.next() {
            Some(ref symbol) if symbol.kind.is_symbol() => rest_param = Some(symbol.get_as_symbol().unwrap().clone()),
            _ => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
        }
    }

    if iter.next() != None {
        return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None));
    }

    Ok(FuncParam::new(params, rest_param))
}

pub fn eval_specialform_fn(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_pair());
    let mut iter = Value::iter(ast).peekable();
    assert!(iter.next().unwrap().kind.matches_symbol("fn"));

    let has_funcname = match iter.peek() {
        Some(val) => val.kind.is_symbol(),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
    };
    let funcname = if has_funcname {
        Some(iter.next().unwrap().get_as_symbol().unwrap().clone())
    } else {
        None
    };

    let param_vector = match iter.next() {
        Some(ref vector) if vector.kind.is_vector() => vector.clone(),
        Some(other) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), other.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
    };

    let param = parse_fn_param_vec(&param_vector)?;

    let body_expr = match iter.next() {
        Some(ref expr) => expr.clone(),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
    };
    Ok(Value::create_closure(FuncKind::AstFunc(body_expr.clone()), funcname, param, env))
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

    #[test]
    fn test_specialform_quote() {
        let env = Env::create_default();
        assert_eq!(eval(Value::create_list_from_vec(vec![
            Value::create_symbol("quote".to_string()),
            Value::create_symbol("x".to_string()),
        ]), env), Ok(Value::create_symbol("x".to_string())));
    }

    #[test]
    fn test_specialform_if() {
        let env = Env::create_default();
        assert_eq!(eval(Value::create_list_from_vec(vec![
            Value::create_symbol("if".to_string()),
            Value::create_boolean(true),
            Value::create_integer(1),
            Value::create_integer(2),
        ]), env.clone()), Ok(Value::create_integer(1)));
        assert_eq!(eval(Value::create_list_from_vec(vec![
            Value::create_symbol("if".to_string()),
            Value::create_nil(),
            Value::create_integer(1),
        ]), env), Ok(Value::create_nil()));
    }

    #[test]
    fn test_specialform_fn() {
        let env = Env::create_default();
        assert_eq!(eval(Value::create_list_from_vec(vec![
            Value::create_list_from_vec(vec![
                Value::create_symbol("fn".to_string()),
                Value::create_vector(vec![
                    Value::create_symbol("x".to_string()),
                    Value::create_symbol("y".to_string()),
                ]),
                Value::create_list_from_vec(vec![
                    Value::create_symbol("+".to_string()),
                    Value::create_symbol("x".to_string()),
                    Value::create_symbol("y".to_string()),
                ]),
            ]),
            Value::create_integer(1),
            Value::create_integer(2),
        ]), env.clone()), Ok(Value::create_integer(3)));
    }
}
