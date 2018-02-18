use core::value::{ValueKind, ValuePtr, FuncKind};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};

fn eval_builtin_let(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
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

fn eval_list(car: &ValuePtr, cdr: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    use self::ValueKind::*;
    if car.kind.matches_symbol("let") {
        return eval_builtin_let(cdr, env);
    }
    let evaled_car = eval(car.clone(), env.clone())?;
    match evaled_car.kind {
        ClosureValue(ref func, ref params, ref closure_env) => {
            let len_params = params.len();
            let len_args = cdr.kind.length_list();
            if len_params != len_args {
                return Err(Exception::new(ExceptionKind::EvaluatorArityException(len_params, len_args), None));
            }
            let mut pairs = vec![];
            let mut cur = cdr;
            for param in params.iter() {
                let arg = match cur.kind {
                    ListValue(ref arg, ref rest) => {
                        cur = rest;
                        eval(arg.clone(), env.clone())?
                    }
                    _ => eval(cur.clone(), env.clone())?,
                };
                pairs.push((param.clone(), arg));
            }
            let new_env = Env::create(pairs, Some(closure_env.clone()));
            match func {
                &FuncKind::BuiltinFunc(ref f) => f(new_env),
                &FuncKind::AstFunc(ref f) => {
                    eval(f.clone(), new_env)
                }
            }
        }
        _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_closure(), evaled_car.kind.as_type_str()), None)),
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
        ClosureValue(_, _, _) => Ok(ast.clone()),
        ListValue(ref car, ref cdr) => eval_list(car, cdr, env),
        NilValue => Ok(ast.clone()),
        MapValue(_, _) => unimplemented!(),
        BooleanValue(_) => Ok(ast.clone()),
        VectorValue(_) => Ok(ast.clone()),
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
            let func = FuncKind::BuiltinFunc(Box::new(builtin_func));
            let closure_env = Env::create(vec![
                ("y".to_string(), Value::create_integer(3)),
            ], None);
            let env = Env::create(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("y".to_string(), Value::create_integer(2)),
                ("f".to_string(), Value::create_closure(func, vec!["x".to_string()], closure_env)),
            ], None);
            assert_eq!(eval(Value::create_list_from_vec(vec![
                Value::create_symbol("f".to_string()),
                Value::create_integer(4),
            ]), env), Ok(Value::create_integer(7)));
        }
        {
            let func = FuncKind::AstFunc(Value::create_symbol("x".to_string()));
            let closure_env = Env::create(vec![
                ("x".to_string(), Value::create_integer(2)),
            ], None);
            let env = Env::create(vec![
                ("x".to_string(), Value::create_integer(1)),
                ("f".to_string(), Value::create_closure(func, vec!["x".to_string()], closure_env)),
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
}
