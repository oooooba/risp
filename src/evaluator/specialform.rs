use std::collections::LinkedList;

use core::value::{Value, ValueKind, ValuePtr, ApplicableBodyKind, ListKind, ValueIterator, Applicable,
                  Pattern, PatternKind, PatternPtr, Type};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};
use evaluator::eval;

fn parse_pattern(pattern: &ValuePtr) -> Result<PatternPtr, Exception> {
    use self::ValueKind::*;

    match pattern.kind {
        SymbolValue(_) => Ok(Pattern::create_symbol(pattern.clone())),
        VectorValue(_) => {
            let mut iter = Value::iter(pattern);
            let mut patterns = vec![];
            let mut rest_patterns = vec![];
            let mut as_symbol = None;
            loop {
                let pattern = match iter.next() {
                    Some(pattern) => pattern,
                    None => break,
                };
                if pattern.kind.matches_keyword("as") {
                    match iter.next() {
                        Some(ref symbol) if symbol.kind.is_symbol() => {
                            as_symbol = Some(parse_pattern(&symbol)?);
                            break;
                        }
                        _ => unimplemented!(),
                    }
                } else if pattern.kind.matches_symbol("&") {
                    match iter.next() {
                        Some(ref pattern) => rest_patterns.push(parse_pattern(pattern)?),
                        None => unimplemented!(),
                    }
                } else {
                    patterns.push(parse_pattern(&pattern)?);
                }
            }

            if iter.next() != None {
                unimplemented!()
            }

            Ok(Pattern::create_vector(patterns, rest_patterns, as_symbol))
        }
        MapValue(_) => {
            let mut as_symbol = None;
            let mut or_value = None;
            let mut pat_key_pairs = LinkedList::new();
            let mut iter = Value::iter(pattern);
            loop {
                let (pattern_val, key_val) = match iter.next() {
                    Some(ref list) if list.kind.is_list() => {
                        let mut iter = Value::iter(list);
                        let pattern = iter.next().unwrap();
                        let key = iter.next().unwrap();
                        (pattern, key)
                    }
                    Some(_) => unimplemented!(),
                    None => break,
                };
                if pattern_val.kind.matches_keyword("as") {
                    if key_val.kind.is_symbol() && as_symbol.is_none() {
                        as_symbol = Some(parse_pattern(&key_val)?);
                    } else {
                        unimplemented!();
                    }
                } else if pattern_val.kind.matches_keyword("or") {
                    if or_value.is_none() {
                        or_value = Some(key_val.clone());
                    } else {
                        unimplemented!();
                    }
                } else {
                    let pattern = parse_pattern(&pattern_val)?;
                    pat_key_pairs.push_back((pattern, key_val));
                }
            }

            let mut pat_key_val_triples = vec![];
            while let Some((pattern, key)) = pat_key_pairs.pop_front() {
                let default_val = match pattern.kind {
                    PatternKind::SymbolPattern(ref symbol) if or_value.is_some() => {
                        let default_val_map = or_value.as_ref().unwrap().get_as_map().unwrap();
                        default_val_map.get(symbol).map(|val| { val.clone() })
                    }
                    _ => None,
                };
                pat_key_val_triples.push((pattern, key, default_val));
            }

            Ok(Pattern::create_map(pat_key_val_triples, as_symbol))
        }
        _ => unimplemented!(),
    }
}

pub fn bind_pattern_to_value(pattern: &PatternPtr, value: &ValuePtr, env: EnvPtr) -> Result<Vec<(String, ValuePtr)>, Exception> {
    use self::PatternKind::*;
    let mut pairs = vec![];

    match pattern.kind {
        SymbolPattern(ref symbol) => {
            assert!(symbol.kind.is_symbol());
            pairs.push((symbol.get_as_symbol().unwrap().clone(), value.clone()));
        }
        VectorPattern(ref patterns, ref rest_patterns, ref as_symbol) => {
            if !(value.kind.is_list() || value.kind.is_vector()) { // ToDo: fix to accept streaming-like data
                unimplemented!()
            }
            let mut value_iter = Value::iter(value);
            for pattern in patterns.iter() {
                match value_iter.next() {
                    Some(ref value) => pairs.append(&mut bind_pattern_to_value(pattern, value, env.clone())?),
                    None => unimplemented!(), // nil
                }
            }
            let rest_value = value_iter.rest();
            for rest_pattern in rest_patterns {
                pairs.append(&mut bind_pattern_to_value(rest_pattern, &rest_value, env.clone())?);
            }

            if let &Some(ref pattern) = as_symbol {
                pairs.append(&mut bind_pattern_to_value(pattern, value, env)?);
            }
        }
        MapPattern(ref patterns, ref as_symbol) => {
            if !(value.kind.is_map()) {
                unimplemented!()
            }
            //let mut value_iter = Value::iter(value);
            let arg_map = value.get_as_map().unwrap();

            for &(ref pattern, ref key, ref default_val) in patterns.iter() {
                let val = arg_map.get(key)
                    .map(|val| val.clone())
                    .or(match default_val {
                        &Some(ref expr) => Some(eval(expr.clone(), env.clone())?),
                        &None => Some(Value::create_nil()),
                    })
                    .unwrap();
                pairs.append(&mut bind_pattern_to_value(pattern, &val, env.clone())?);
            }

            if let &Some(ref pattern) = as_symbol {
                pairs.append(&mut bind_pattern_to_value(pattern, value, env)?);
            }
        }
    }

    Ok(pairs)
}

fn split_let_binding_form(form: &ValuePtr) -> Result<(Vec<ValuePtr>, Vec<ValuePtr>), Exception> {
    assert!(form.kind.is_vector());
    let mut patterns = vec![];
    let mut exprs = vec![];
    let mut iter = Value::iter(form);
    loop {
        let pattern = match iter.next() {
            Some(ref symbol) if symbol.kind.is_symbol() => symbol.clone(),
            Some(ref vector) if vector.kind.is_vector() => vector.clone(),
            Some(ref map) if map.kind.is_map() => map.clone(),
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
    Ok((patterns, exprs))
}

pub fn eval_specialform_let(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("let"));

    let (unparsed_patterns, unevaled_exprs) = match iter.next() {
        Some(ref form) if form.kind.is_vector() => split_let_binding_form(form)?,
        Some(_) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), ast.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("let"), None)),
    };

    let mut patterns = vec![];
    for pattern in unparsed_patterns.iter() {
        patterns.push(parse_pattern(pattern)?);
    }

    let mut evaled_pairs = vec![];
    assert_eq!(patterns.len(), unevaled_exprs.len());
    for i in 0..(patterns.len()) {
        let expr = &unevaled_exprs[i];
        let tmp_env = Env::create(evaled_pairs.clone(), Some(env.clone()));
        let val = eval(expr.clone(), tmp_env)?;
        let pattern = &patterns[i];
        let mut pairs = bind_pattern_to_value(pattern, &val, env.clone())?;
        evaled_pairs.append(&mut pairs);
    }

    let let_env = Env::create(evaled_pairs, Some(env));
    eval_specialform_do_core(iter, let_env)
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
    let param = parse_pattern(&param_vector)?;

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
    let param = parse_pattern(&param_vector)?;

    let body_expr = match iter.next() {
        Some(ref expr) => expr.clone(),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("defmacro"), None)),
    };
    assert_eq!(iter.next(), None);

    let applicable = Applicable::new(None, param, ApplicableBodyKind::AstBody(body_expr));
    let val = Value::create_macro(applicable);

    Err(Exception::new(ExceptionKind::Continuation(Env::create(vec![(symbol, val)], Some(env))), None))
}

fn eval_specialform_do_core(mut iter: ValueIterator, env: EnvPtr) -> Result<ValuePtr, Exception> {
    let mut result_value = Value::create_nil();
    while let Some(expr) = iter.next() {
        result_value = eval(expr.clone(), env.clone())?;
    }
    Ok(result_value)
}

pub fn eval_specialform_do(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("do"));
    eval_specialform_do_core(iter, env)
}

fn split_try_form(mut iter: ValueIterator)
                  -> Result<(Vec<ValuePtr>, Vec<ValuePtr>, Option<ValuePtr>), Exception> {
    let mut try_exprs = vec![];
    let mut catch_clauses = vec![];
    let mut finally_clause = None;

    while let Some(expr) = iter.next() {
        if expr.kind.is_list() || expr.kind.is_vector() {
            match Value::iter(&expr).peekable().peek() {
                Some(ref symbol) if symbol.kind.matches_symbol("catch") => {
                    catch_clauses.push(expr.clone());
                    break;
                }
                Some(ref symbol) if symbol.kind.matches_symbol("finally") => {
                    finally_clause = Some(expr.clone());
                    break;
                }
                _ => (),
            }
        }
        try_exprs.push(expr.clone());
    }

    if finally_clause.is_some() {
        assert_eq!(catch_clauses.len(), 0);
        if iter.next().is_none() {
            return Ok((try_exprs, catch_clauses, finally_clause));
        } else {
            unimplemented!()
        }
    }

    while let Some(expr) = iter.next() {
        if !(expr.kind.is_list() || expr.kind.is_vector()) {
            unimplemented!()
        }
        match Value::iter(&expr).peekable().peek() {
            Some(ref symbol) if symbol.kind.matches_symbol("catch") => catch_clauses.push(expr.clone()),
            Some(ref symbol) if symbol.kind.matches_symbol("finally") => {
                finally_clause = Some(expr.clone());
                break;
            }
            _ => unimplemented!(),
        }
    }

    if iter.next().is_none() {
        Ok((try_exprs, catch_clauses, finally_clause))
    } else {
        unimplemented!()
    }
}

fn parse_catch_clause(ast: &ValuePtr) -> Result<(ExceptionKind, String, ValuePtr), Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("catch"));

    let exception_class = match iter.next() {
        Some(ref symbol) if symbol.kind.is_symbol() => ExceptionKind::EvaluatorUndefinedSymbolException("example".to_string()),
        Some(_other) => unimplemented!(),
        None => unimplemented!(),
    };

    let bound_variable = match iter.next() {
        Some(ref symbol) if symbol.kind.is_symbol() => symbol.get_as_symbol().unwrap().clone(),
        Some(_other) => unimplemented!(),
        None => unimplemented!(),
    };

    let exprs = iter.rest();

    Ok((exception_class, bound_variable, exprs))
}

fn parse_finally_clause(ast: &ValuePtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("finally"));

    let exprs = iter.rest();
    Ok(exprs)
}

pub fn eval_specialform_try(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("try"));

    let (try_exprs, unparsed_catch_clauses, unparsed_finally_clause) = split_try_form(iter)?;

    let mut catch_clauses = vec![];
    for catch_clause in unparsed_catch_clauses.iter() {
        catch_clauses.push(parse_catch_clause(catch_clause)?);
    }

    let finally_clause = match unparsed_finally_clause {
        Some(ref clause) => Some(parse_finally_clause(clause)?),
        _ => None,
    };

    let mut ret_val = Value::create_nil();
    let mut exception = None;
    for expr in try_exprs.iter() {
        match eval(expr.clone(), env.clone()) {
            Ok(val) => ret_val = val,
            Err(e) => {
                exception = Some(e);
                break;
            }
        }
    }

    if let Some(_e) = exception {
        if catch_clauses.len() > 0 {
            let clause = &catch_clauses[0];
            let new_env = Env::create(vec![(clause.1.clone(), Value::create_nil())], Some(env.clone()));
            ret_val = eval_specialform_do_core(Value::iter(&clause.2), new_env)?;
        }
    }

    if let Some(clause) = finally_clause {
        eval_specialform_do_core(Value::iter(&clause), env)?;
    }

    Ok(ret_val)
}

pub fn eval_specialform_defrecord(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("defrecord"));

    let record_name = match iter.next() {
        Some(ref symbol) if symbol.kind.is_symbol() => symbol.get_as_symbol().unwrap().clone(),
        _ => unimplemented!(),
    };

    let symbols = match iter.next() {
        Some(ref vector) if vector.kind.is_vector() => {
            for symbol in Value::iter(vector) {
                if !symbol.kind.is_symbol() {
                    unimplemented!()
                }
            }
            vector.clone()
        }
        _ => unimplemented!(),
    };

    let mut fields = vec![];
    for symbol in Value::iter(&symbols) {
        let field = symbol.get_as_symbol().unwrap().clone();
        fields.push((field, None))
    }

    let record = Value::create_type(Type::create(fields));
    Err(Exception::new(ExceptionKind::Continuation(
        Env::create(vec![(record_name, record)], Some(env))), None))
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
