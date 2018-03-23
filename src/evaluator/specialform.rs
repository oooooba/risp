use core::value::{Value, ValueKind, ValuePtr, ApplicableBodyKind, ApplicableParam, ListKind, ValueIterator, Applicable};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};
use evaluator::eval;

pub fn eval_specialform_let(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("let"));

    let bindings = match iter.next() {
        Some(ref bindings) if bindings.kind.is_vector() => bindings.clone(),
        Some(_) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), ast.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("let"), None)),
    };

    let mut unevaled_pairs: Vec<(String, ValuePtr)> = vec![];
    let mut bindings_iter = Value::iter(&bindings);
    loop {
        let symbol = match bindings_iter.next() {
            Some(ref symbol) if symbol.kind.is_symbol() => symbol.get_as_symbol().unwrap().clone(),
            Some(other) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_symbol(), other.kind.as_type_str()), None)),
            None => break,
        };
        let expr = match bindings_iter.next() {
            Some(expr) => expr,
            None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalArgumentException(
                "number of forms in binding vector".to_string()), None)),
        };
        unevaled_pairs.push((symbol, expr));
    }

    let mut evaled_pairs = vec![];
    for &(ref symbol, ref expr) in unevaled_pairs.iter() {
        let tmp_env = Env::create(evaled_pairs.clone(), Some(env.clone()));
        let val = eval(expr.clone(), tmp_env)?;
        evaled_pairs.push(((*symbol).clone(), val));
    }
    let let_env = Env::create(evaled_pairs, Some(env));

    let mut result_value = Value::create_nil();
    while let Some(expr) = iter.next() {
        result_value = eval(expr.clone(), let_env.clone())?;
    }
    Ok(result_value)
}

pub fn eval_specialform_quote(ast: &ValuePtr, _env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
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
    assert!(ast.kind.is_list());
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
    assert!(ast.kind.is_list());
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

fn parse_fn_param_vec(param_vec: &ValuePtr) -> Result<ApplicableParam, Exception> {
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

    Ok(ApplicableParam::new(params, rest_param))
}

pub fn eval_specialform_fn(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
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
    let applicable = Applicable::new(funcname, param, ApplicableBodyKind::AstBody(body_expr.clone()));
    Ok(Value::create_closure(applicable, env))
}

fn eval_specialform_unquote_common(mut iter: ValueIterator, env: EnvPtr, enables: bool) -> Result<ValuePtr, Exception> {
    let name = iter.next().unwrap().get_as_symbol().unwrap().clone();

    if !enables {
        return Err(Exception::new(ExceptionKind::EvaluatorIllegalStateException(
            name,
            "not in quasiquote form".to_string(),
        ), None));
    }

    match iter.next() {
        Some(expr) => eval(expr, env),
        None => Ok(Value::create_nil()),
    }
}

pub fn eval_specialform_unquote(ast: &ValuePtr, env: EnvPtr, enables: bool) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    assert!(Value::iter(ast).peekable().peek().unwrap().kind.matches_symbol("unquote"));
    eval_specialform_unquote_common(Value::iter(ast), env, enables)
}

pub fn eval_specialform_splice_unquote(ast: &ValuePtr, env: EnvPtr, enables: bool) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    assert!(Value::iter(ast).peekable().peek().unwrap().kind.matches_symbol("splice-unquote"));
    eval_specialform_unquote_common(Value::iter(ast), env, enables)
}

fn eval_specialform_quasiquote_core(ast: &ValuePtr, env: EnvPtr) -> Result<(ValuePtr, bool), Exception> {
    use self::ValueKind::*;
    match ast.kind {
        ListValue(ListKind::ConsList(ref car, _)) if car.kind.matches_symbol("unquote") =>
            eval_specialform_unquote(ast, env, true).map(|val| (val, false)),
        ListValue(ListKind::ConsList(ref car, _)) if car.kind.matches_symbol("splice-unquote") =>
            eval_specialform_splice_unquote(ast, env, true).map(|val| (val, true)),
        ListValue(ListKind::ConsList(ref car, ref cdr)) => {
            let (car_val, splices) = eval_specialform_quasiquote_core(car, env.clone())?;
            let (cdr_val, _) = eval_specialform_quasiquote_core(cdr, env)?;
            if splices {
                let mut iter = Value::iter(&car_val);
                let mut stack = vec![];
                while let Some(item) = iter.next() {
                    stack.push(item);
                }
                let mut list = cdr_val;
                while let Some(item) = stack.pop() {
                    list = Value::create_list(ListKind::ConsList(item, list));
                }
                Ok((list, false))
            } else {
                Ok((Value::create_list(ListKind::ConsList(car_val, cdr_val)), false))
            }
        }
        _ => Ok((ast.clone(), false))
    }
}

pub fn eval_specialform_quasiquote(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast).peekable();
    assert!(iter.next().unwrap().kind.matches_symbol("quasiquote"));

    match iter.next() {
        Some(ref ast) => eval_specialform_quasiquote_core(ast, env).map(|t| t.0),
        None => Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("quasiquote"), None)),
    }
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
