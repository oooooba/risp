mod specialform;
pub mod builtinfunc;

use std::collections::HashMap;

use core::value::{Value, ValueKind, ValuePtr, ApplicableBodyKind, ListKind};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};

fn eval_list_trampoline(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_list());
    use self::ListKind::*;
    let mut iter = Value::iter(ast).peekable();
    match iter.peek() {
        Some(symbol) if symbol.kind.matches_symbol("if") => return specialform::eval_specialform_if(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("fn") => return specialform::eval_specialform_fn(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("def") => return specialform::eval_specialform_def(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("quote") => return specialform::eval_specialform_quote(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("let") => return specialform::eval_specialform_let(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("quasiquote") => return specialform::eval_specialform_quasiquote(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("unquote") => return specialform::eval_specialform_unquote(ast, env, false),
        Some(symbol) if symbol.kind.matches_symbol("splice-unquote") => return specialform::eval_specialform_splice_unquote(ast, env, false),
        Some(symbol) if symbol.kind.matches_symbol("defmacro") => return specialform::eval_specialform_defmacro(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("do") => return specialform::eval_specialform_do(ast, env),
        Some(symbol) if symbol.kind.matches_symbol("try") => return specialform::eval_specialform_try(ast, env),
        None => return Ok(Value::create_list(EmptyList)),
        _ => (),
    }
    let mut iter = Value::iter(ast);
    let car = iter.next().unwrap();
    let evaled_car = eval(car.clone(), env.clone())?;
    let cdr = iter.rest();
    if evaled_car.kind.is_closure() {
        apply(&evaled_car, &cdr, env)
    } else if evaled_car.kind.is_macro() {
        let new_ast = apply(&evaled_car, &cdr, env.clone())?;
        eval(new_ast, env)
    } else {
        Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_closure(), evaled_car.kind.as_type_str()), None))
    }
}

fn apply(applicable_val: &ValuePtr, args_val: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(applicable_val.kind.is_closure() || applicable_val.kind.is_macro());
    assert!(args_val.kind.is_list());
    let (applicable, closure_env) = match applicable_val.kind {
        ValueKind::ClosureValue(ref applicable, ref closure_env) => (applicable, Some(closure_env.clone())),
        ValueKind::MacroValue(ref applicable) => (applicable, None),
        _ => unreachable!(),
    };
    let processes_closure = closure_env != None;
    let param = &applicable.param;

    let mut evaled_args = vec![];
    for arg in Value::iter(args_val) {
        let val = if processes_closure {
            eval(arg.clone(), env.clone())?
        } else {
            arg.clone()
        };
        evaled_args.push(val);
    }
    let evaled_args_val = Value::create_list_from_vec(evaled_args);

    let mut evaled_pairs = vec![];
    if let Some(ref name) = applicable.name {
        evaled_pairs.push((name.clone(), applicable_val.clone()));
    }
    evaled_pairs.append(&mut specialform::bind_pattern_to_value(param, &evaled_args_val, env.clone())?);

    let new_env = if processes_closure {
        Env::create(evaled_pairs, closure_env)
    } else {
        Env::create(evaled_pairs, Some(env))
    };

    match applicable.body {
        ApplicableBodyKind::BuiltinBody(ref f) => f(new_env),
        ApplicableBodyKind::AstBody(ref f) => {
            eval(f.clone(), new_env)
        }
    }
}

fn eval_map(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_map());
    if let ValueKind::MapValue(ref map) = ast.kind {
        if ast.is_literal {
            let mut evaled_map = HashMap::new();
            for (key, val) in map.iter() {
                let key = eval(key.clone(), env.clone())?;
                let val = eval(val.clone(), env.clone())?;
                evaled_map.insert(key, val);
            }
            Ok(Value::create_map(evaled_map))
        } else {
            Ok(ast.clone())
        }
    } else {
        unreachable!()
    }
}

fn eval_vector(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_vector());
    if let ValueKind::VectorValue(ref vector) = ast.kind {
        if ast.is_literal {
            let mut evaled_vector = vec![];
            for item in vector.iter() {
                let val = eval(item.clone(), env.clone())?;
                evaled_vector.push(val);
            }
            Ok(Value::create_vector(evaled_vector))
        } else {
            Ok(ast.clone())
        }
    } else {
        unreachable!()
    }
}

pub fn eval(ast: ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    use self::ValueKind::*;
    match ast.kind {
        IntegerValue(_) => Ok(ast.clone()),
        StringValue(_) => Ok(ast.clone()),
        SymbolValue(ref symbol) => {
            match env.lookup(symbol) {
                Some(v) => Ok(v.clone()),
                None => Err(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(symbol.clone()), None)),
            }
        }
        KeywordValue(_) => Ok(ast.clone()),
        ClosureValue(_, _) => Ok(ast.clone()),
        ListValue(_) => eval_list_trampoline(&ast, env),
        NilValue => Ok(ast.clone()),
        MapValue(_) => eval_map(&ast, env),
        BooleanValue(_) => Ok(ast.clone()),
        VectorValue(_) => eval_vector(&ast, env),
        MacroValue(_) => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::value::*;

    fn builtin_func(env: EnvPtr) -> Result<ValuePtr, Exception> {
        let x_str = "x".to_string();
        let y_str = "y".to_string();

        let x_val = env.lookup(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
        let x_int = match x_val.kind {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_integer(), x_val.kind.as_type_str()), None)),
        };

        let y_val = env.lookup(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
        let y_int = match y_val.kind {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_integer(), y_val.kind.as_type_str()), None)),
        };

        Ok(Value::create_integer(x_int + y_int))
    }

    #[test]
    fn test_acceptance() {
        {
            let env = Env::create_empty();
            assert_eq!(eval(Value::create_keyword("XYZ".to_string()), env),
                       Ok(Value::create_keyword("XYZ".to_string())));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), Value::create_string("abc".to_string())),
            ], None);
            assert_eq!(eval(Value::create_symbol("x".to_string()), env),
                       Ok(Value::create_string("abc".to_string())));
        }
        {
            let func = ApplicableBodyKind::BuiltinBody(Box::new(builtin_func));
            let closure_env = Env::create(vec![
                ("y".to_string(), Value::create_integer(3)),
            ], None);
            let param = Pattern::create_vector(vec![
                Pattern::create_symbol(Value::create_symbol("x".to_string()))
            ], vec![], None);
            let applicable = Applicable::new(None, param, func);
            let env = Env::create(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("y".to_string(), Value::create_integer(2)),
                ("f".to_string(), Value::create_closure(applicable, closure_env)),
            ], None);
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("f".to_string()),
                Value::create_integer(4),
            ]), env), Ok(Value::create_integer(7)));
        }
        {
            let func = ApplicableBodyKind::AstBody(Value::create_symbol("x".to_string()));
            let closure_env = Env::create(vec![
                ("x".to_string(), Value::create_integer(2)),
            ], None);
            let param = Pattern::create_vector(vec![
                Pattern::create_symbol(Value::create_symbol("x".to_string()))
            ], vec![], None);
            let applicable = Applicable::new(None, param, func);
            let env = Env::create(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("f".to_string(), Value::create_closure(applicable, closure_env)),
            ], None);
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("f".to_string()),
                Value::create_integer(3),
            ]), env), Ok(Value::create_integer(3)));
        }
        {
            let env = Env::create_empty();
            assert_eq!(eval(Value::create_list_from_vec(vec![]), env),
                       Ok(Value::create_list_from_vec(vec![])));
        }
        {
            let env = Env::create_empty();
            assert_eq!(eval(Value::create_nil(), env),
                       Ok(Value::create_nil()));
        }
        {
            let env = Env::create_default();
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("+".to_string()),
                Value::create_integer(1),
                Value::create_integer(2),
            ]), env), Ok(Value::create_integer(3)));
        }
        {
            let env = Env::create_default();
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("=".to_string()),
                Value::create_boolean(true),
                Value::create_boolean(true),
            ]), env), Ok(Value::create_boolean(true)));
        }
    }

    #[test]
    fn test_rejection() {
        use core::exception::*;
        {
            let env = Env::create_empty();
            assert_eq!(eval(Value::create_symbol("x".to_string()), env),
                       Err(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException("x".to_string()), None)));
        }
        {
            let env = Env::create_default();
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("+".to_string()),
                Value::create_integer(1),
                Value::create_string("x".to_string()),
            ]), env), Err(Exception::new(ExceptionKind::EvaluatorTypeException(
                ValueKind::type_str_integer(),
                ValueKind::type_str_string()), None)));
        }
    }

    #[test]
    fn test_builtin_let() {
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
    fn test_macro() {
        {
            let outer_env = Some(Env::create_default());
            let macro_body = Value::create_integer(1);
            let funcparam = Pattern::create_vector(vec![], vec![], None);
            let applicable = Applicable::new(None, funcparam, ApplicableBodyKind::AstBody(macro_body));
            let macro_val = Value::create_macro(applicable);
            let env = Env::create(vec![("one".to_string(), macro_val)], outer_env);
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("one".to_string()),
            ]), env),
                       Ok(Value::create_integer(1)));
        }
        {
            let outer_env = Some(Env::create_default());

            let s_pred = "pred".to_string();
            let s_a = "a".to_string();
            let s_b = "b".to_string();
            let s_unquote = "unquote".to_string();

            let macro_body = Value::create_list_from_vec(vec![
                Value::create_symbol("quasiquote".to_string()),
                Value::create_list_from_vec(vec![
                    Value::create_symbol("if".to_string()),
                    Value::create_list_from_vec(vec![
                        Value::create_symbol(s_unquote.clone()),
                        Value::create_symbol(s_pred.clone()),
                    ]),
                    Value::create_list_from_vec(vec![
                        Value::create_symbol(s_unquote.clone()),
                        Value::create_symbol(s_b.clone()),
                    ]),
                    Value::create_list_from_vec(vec![
                        Value::create_symbol(s_unquote.clone()),
                        Value::create_symbol(s_a.clone()),
                    ]),
                ]),
            ]);
            let funcparam = Pattern::create_vector(vec![
                Pattern::create_symbol(Value::create_symbol(s_pred)),
                Pattern::create_symbol(Value::create_symbol(s_a)),
                Pattern::create_symbol(Value::create_symbol(s_b)),
            ], vec![], None);
            let applicable = Applicable::new(None, funcparam, ApplicableBodyKind::AstBody(macro_body));

            let closure = Value::create_macro(applicable);
            let env = Env::create(vec![("unless".to_string(), closure)], outer_env);
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("unless".to_string()),
                Value::create_boolean(false),
                Value::create_integer(7),
                Value::create_integer(8),
            ]), env),
                       Ok(Value::create_integer(7)));
        }
    }
}
