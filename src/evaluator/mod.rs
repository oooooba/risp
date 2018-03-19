mod specialform;
pub mod builtinfunc;

use std::collections::HashMap;

use core::value::{Value, ValueKind, ValuePtr, FuncKind, ListKind};
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
        None => return Ok(Value::create_list(EmptyList)),
        _ => (),
    }
    let mut iter = Value::iter(ast);
    let car = iter.next().unwrap();
    let cdr = iter.rest();
    eval_list(&car, &cdr, env)
}

fn eval_list(car: &ValuePtr, cdr: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    use self::ValueKind::*;
    let evaled_car = eval(car.clone(), env.clone())?;
    match evaled_car.kind {
        ClosureValue(ref func, ref funcname, ref param, ref closure_env) => {
            assert!(cdr.kind.is_list());

            let mut arg_iter = Value::iter(cdr);
            let mut num_args = 0;

            let mut unevaled_param_pairs = vec![];
            for symbol in param.params.iter() {
                if let Some(arg) = arg_iter.next() {
                    unevaled_param_pairs.push((symbol, arg));
                    num_args += 1;
                } else {
                    return Err(Exception::new(ExceptionKind::EvaluatorArityException(param.params.len(), num_args), None));
                }
            }

            let mut unevaled_rest_args = vec![];
            while let Some(arg) = arg_iter.next() {
                unevaled_rest_args.push(arg);
                num_args += 1;
            }

            if param.rest_param == None && unevaled_rest_args.len() > 0 {
                return Err(Exception::new(ExceptionKind::EvaluatorArityException(param.params.len(), num_args), None));
            }

            let mut evaled_param_pairs = vec![];
            if let &Some(ref name) = funcname {
                evaled_param_pairs.push((name.clone(), evaled_car.clone()));
            }
            for &(ref symbol, ref arg) in unevaled_param_pairs.iter() {
                let val = eval(arg.clone(), env.clone())?;
                evaled_param_pairs.push(((*symbol).clone(), val));
            }

            let mut evaled_rest_args = vec![];
            for arg in unevaled_rest_args.iter() {
                let val = eval(arg.clone(), env.clone())?;
                evaled_rest_args.push(val);
            }

            if let Some(ref symbol) = param.rest_param {
                evaled_param_pairs.push((symbol.clone(), Value::create_list_from_vec(evaled_rest_args)));
            }

            let new_env = Env::create(evaled_param_pairs, Some(closure_env.clone()));
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

fn eval_map(ast: &ValuePtr, env: EnvPtr) -> Result<ValuePtr, Exception> {
    assert!(ast.kind.is_map());
    if let ValueKind::MapValue(ref map) = ast.kind {
        let mut evaled_map = HashMap::new();
        for (key, val) in map.iter() {
            let key = eval(key.clone(), env.clone())?;
            let val = eval(val.clone(), env.clone())?;
            evaled_map.insert(key, val);
        }
        Ok(Value::create_map(evaled_map))
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
        ClosureValue(_, _, _, _) => Ok(ast.clone()),
        ListValue(_) => eval_list_trampoline(&ast, env),
        NilValue => Ok(ast.clone()),
        MapValue(_) => eval_map(&ast, env),
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
                ("f".to_string(), Value::create_closure(func, None, FuncParam::new(vec!["x".to_string()], None), closure_env)),
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
                ("f".to_string(), Value::create_closure(func, None, FuncParam::new(vec!["x".to_string()], None), closure_env)),
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
