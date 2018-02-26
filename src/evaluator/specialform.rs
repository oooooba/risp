use core::value::{Value, ValueKind, ValuePtr, FuncKind};
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

pub fn eval_specialform_quote(ast: &ValuePtr, _env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("quote"));

    let val=match iter.next(){
        Some(val)=> val,
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
    assert!(ast[0].kind.matches_symbol("if"));
    let cond_expr = &ast[1];
    let true_expr = &ast[2];
    let false_expr = &ast[3];

    let cond = eval(cond_expr.clone(), env.clone())?;
    match cond.kind {
        ValueKind::BooleanValue(false) => eval(false_expr.clone(), env),
        ValueKind::NilValue => eval(false_expr.clone(), env),
        _ => eval(true_expr.clone(), env),
    }
}

pub fn eval_specialform_fn(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    let mut iter = Value::iter(ast);
    assert!(iter.next().unwrap().kind.matches_symbol("fn"));

    let param_vector = match iter.next() {
        Some(ref vector) if vector.kind.is_vector() => vector.clone(),
        Some(other) => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_vector(), other.kind.as_type_str()), None)),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
    };

    let mut params = vec![];
    for param in Value::iter(&param_vector) {
        match param.get_as_symbol() {
            Some(ref symbol) => params.push((*symbol).clone()),
            None => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_symbol(), param.kind.as_type_str()), None)),
        };
    }

    let body_expr = match iter.next() {
        Some(ref expr) => expr.clone(),
        None => return Err(Exception::new(ExceptionKind::EvaluatorIllegalFormException("fn"), None)),
    };
    Ok(Value::create_closure(FuncKind::AstFunc(body_expr.clone()), params, env))
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
