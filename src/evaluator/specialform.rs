use core::value::{Value, ValueKind, ValuePtr, ApplicableBodyKind, ApplicableParam, ListKind, ValueIterator, Applicable,
                  Pattern, PatternKind, PatternPtr};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};
use evaluator::eval;

fn parse_pattern(pattern: &ValuePtr) -> Result<PatternPtr, Exception> {
    use self::ValueKind::*;

    match pattern.kind {
        SymbolValue(_) => Ok(Pattern::create_symbol(pattern.clone())),
        VectorValue(_) => {
            let mut iter = Value::iter(pattern);
            let mut declares_rest_param = false;
            let mut patterns = vec![];
            while let Some(ref pattern) = iter.next() {
                if pattern.kind.matches_symbol("&") {
                    declares_rest_param = true;
                    break;
                }
                patterns.push(parse_pattern(pattern)?);
            }

            let rest_param = if declares_rest_param {
                match iter.next() {
                    Some(ref symbol) if symbol.kind.is_symbol() => Some(Pattern::create_symbol(symbol.clone())),
                    _ => unimplemented!(),
                }
            } else {
                None
            };

            if iter.next() != None {
                unimplemented!()
            }

            Ok(Pattern::create_vector(patterns, rest_param, None))
        }
        _ => unimplemented!(),
    }
}

fn match_pattern_and_expr(pattern: &PatternPtr, expr: &ValuePtr) -> Result<Vec<(ValuePtr, ValuePtr)>, Exception> {
    use self::PatternKind::*;
    let mut pairs = vec![];

    match pattern.kind {
        SymbolPattern(ref symbol) => {
            assert!(symbol.kind.is_symbol());
            pairs.push((symbol.clone(), expr.clone()));
        }
        VectorPattern(ref patterns, ref rest_pattern, ref as_symbol) => {
            if !(expr.kind.is_list() || expr.kind.is_vector()) { // ToDo: fix to accept streaming-like data
                unimplemented!()
            }
            let mut expr_iter = Value::iter(expr);
            for pattern in patterns.iter() {
                match expr_iter.next() {
                    Some(ref expr) => pairs.append(&mut match_pattern_and_expr(pattern, expr)?),
                    None => unimplemented!(), // exception
                }
            }
            if let &Some(ref rest_pattern) = rest_pattern {
                pairs.append(&mut match_pattern_and_expr(rest_pattern, &expr_iter.rest())?);
            }

            if let &Some(ref pattern) = as_symbol {
                pairs.append(&mut match_pattern_and_expr(pattern, expr)?);
            }
        }
    }

    Ok(pairs)
}

fn split_let_binding_form(form: &ValuePtr) -> Result<(ValuePtr, ValuePtr), Exception> {
    assert!(form.kind.is_vector());
    let mut patterns = vec![];
    let mut exprs = vec![];
    let mut iter = Value::iter(form);
    loop {
        let pattern = match iter.next() {
            Some(ref symbol) if symbol.kind.is_symbol() => symbol.clone(),
            Some(ref vector) if vector.kind.is_vector() => vector.clone(),
            Some(_) => unimplemented!(), // exception
            None => break,
        };
        patterns.push(pattern);

        match iter.next() {
            Some(expr) => exprs.push(expr),
            None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalArgumentException(
                "number of forms in binding vector".to_string()), None)),
        };
    }
    assert_eq!(patterns.len(), exprs.len());
    Ok((Value::create_vector(patterns), Value::create_vector(exprs)))
}

pub fn eval_specialform_let(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("let"));

    let (patterns, exprs) = match iter.next() {
        Some(ref form) if form.kind.is_vector() => split_let_binding_form(form)?,
        Some(_) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), ast.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("let"), None)),
    };

    let patterns = parse_pattern(&patterns)?;
    let unevaled_pairs = match_pattern_and_expr(&patterns, &exprs)?;

    let mut evaled_pairs = vec![];
    for &(ref symbol, ref expr) in unevaled_pairs.iter() {
        assert!(symbol.kind.is_symbol());
        let symbol = symbol.get_as_symbol().unwrap().clone();
        let tmp_env = Env::create(evaled_pairs.clone(), Some(env.clone()));
        let val = eval(expr.clone(), tmp_env)?;
        evaled_pairs.push((symbol, val));
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
            ValueKind::SymbolValue(ref symbol) if symbol == "&" => {
                declares_rest_param = true;
                break;
            }
            ValueKind::SymbolValue(ref symbol) => params.push(symbol.clone()),
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

pub fn eval_specialform_defmacro(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("defmacro"));

    let symbol = match iter.next() {
        Some(ref symbol) if symbol.kind.is_symbol() => symbol.get_as_symbol().unwrap().clone(),
        Some(other) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_symbol(), other.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("defmacro"), None)),
    };

    let param_vector = match iter.next() {
        Some(ref vector) if vector.kind.is_vector() => vector.clone(),
        Some(other) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), other.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("defmacro"), None)),
    };
    let param = parse_fn_param_vec(&param_vector)?;

    let body_expr = match iter.next() {
        Some(ref expr) => expr.clone(),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("defmacro"), None)),
    };
    assert_eq!(iter.next(), None);

    let applicable = Applicable::new(None, param, ApplicableBodyKind::AstBody(body_expr));
    let val = Value::create_macro(applicable);

    Err(Exception::new(ExceptionKind::Continuation(Env::create(vec![(symbol, val)], Some(env))), None))
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
