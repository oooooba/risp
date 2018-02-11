use core::value::{ValueKind, ValuePtr, FuncKind};
use core::exception::{Exception, ExceptionKind};
use core::env::{Env, EnvPtr};

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
        ListValue(ref list) => {
            let mut iter = list.iter();
            let first = match iter.next() {
                Some(elem) => elem.clone(),
                None => return Ok(ast.clone()),
            };
            match (eval(first.clone(), env.clone())?).kind {
                ClosureValue(ref func, ref arg, ref closure_env) => {
                    let arg_val = match iter.next() {
                        Some(val) => eval(val.clone(), env)?,
                        None => return Err(Exception::new(ExceptionKind::EvaluatorArityException(1, 0), None)),
                    };
                    let new_env = Env::create(vec![(arg.clone(), arg_val)], Some(closure_env.clone()));
                    match func {
                        &FuncKind::BuiltinFunc(ref f) => f(new_env),
                        &FuncKind::AstFunc(ref f) => {
                            eval(f.clone(), new_env)
                        }
                    }
                }
                _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException("Closure".to_string(), "Unknown".to_string()), None)),
            }
        }
        NilValue => Ok(ast.clone()),
        MapValue(_, _) => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::value::*;

    fn builtin_func(env: EnvPtr) -> Result<ValuePtr, Exception> {
        let x_str = "x".to_string();
        let y_str = "y".to_string();
        let type_int_str = "Integer".to_string();
        let type_unknown_str = "Unknown".to_string();

        let x_val = env.lookup(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
        let x_int = match x_val.kind {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        let y_val = env.lookup(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
        let y_int = match y_val.kind {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
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
                ("f".to_string(), Value::create_closure(func, "x".to_string(), closure_env)),
            ], None);
            assert_eq!(eval(Value::create_list(vec![
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
                ("f".to_string(), Value::create_closure(func, "x".to_string(), closure_env)),
            ], None);
            assert_eq!(eval(Value::create_list(vec![
                Value::create_symbol("f".to_string()),
                Value::create_integer(3),
            ]), env), Ok(Value::create_integer(3)));
        }
        {
            let env = Env::create_empty();
            assert_eq!(eval(Value::create_list(vec![]), env),
                       Ok(Value::create_list(vec![])));
        }
        {
            let env = Env::create_empty();
            assert_eq!(eval(Value::create_nil(), env),
                       Ok(Value::create_nil()));
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
    }
}
