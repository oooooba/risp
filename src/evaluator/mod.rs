use value::{Value, ValueKind, FuncKind, Exception, ExceptionKind, Env};

pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn new(env: Env) -> Interpreter {
        Interpreter {
            env: env,
        }
    }

    fn eval(&mut self, ast: Value) -> Result<Value, Exception> {
        use self::ValueKind::*;
        match *ast {
            IntegerValue(_) => Ok(ast.clone()),
            StringValue(_) => Ok(ast.clone()),
            SymbolValue(ref symbol) => {
                match self.env.map.get(symbol.as_str()) {
                    Some(v) => Ok(v.clone()),
                    None => Err(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(symbol.clone()), None)),
                }
            }
            KeywordValue(_) => Ok(ast.clone()),
            ClosureValue(_, _, _) => Ok(ast.clone()),
            ListValue(ref list) => {
                let mut iter = list.iter();
                let func = match iter.next() {
                    Some(f) => f.clone(),
                    None => return Err(Exception::new(ExceptionKind::EvaluatorTypeException("Closure".to_string(), "Unknown".to_string()), None)),
                };
                match *self.eval(func)? {
                    ClosureValue(ref func, ref arg, ref env) => {
                        let arg_val = match iter.next() {
                            Some(val) => val.clone(),
                            None => return Err(Exception::new(ExceptionKind::EvaluatorTypeException("Closure".to_string(), "Unknown".to_string()), None)),
                        };
                        let mut env = Env::new(env.map.clone(), None); // ToDo: Fix to use outer env
                        env.map.insert(arg.clone(), arg_val);
                        match func {
                            &FuncKind::BuiltinFunc(ref f) => f(env),
                            &FuncKind::AstFunc(ref f) => {
                                self.env = env; // ToDo: Fix to save and restore old env
                                self.eval(f.clone())
                            }
                        }
                    }
                    _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException("Closure".to_string(), "Unknown".to_string()), None)),
                }
            }
        }
    }
}

pub fn eval(ast: Value, env: Env) -> Result<Value, Exception> {
    Interpreter::new(env).eval(ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::*;

    fn builtin_func(env: Env) -> Result<Value, Exception> {
        let x_str = "x".to_string();
        let y_str = "y".to_string();
        let type_int_str = "Integer".to_string();
        let type_unknown_str = "Unknown".to_string();

        let x_val = env.map.get(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
        let x_int = match **x_val {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        let y_val = env.map.get(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
        let y_int = match **y_val {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        Ok(create_integer_value(x_int + y_int))
    }

    #[test]
    fn test_acceptance() {
        use value::*;
        {
            let env = create_empty_env();
            assert_eq!(eval(create_keyword_value("XYZ".to_string()), env),
                       Ok(create_keyword_value("XYZ".to_string())));
        }
        {
            let mut env = create_empty_env();
            env.map.insert("x".to_string(), create_string_value("abc".to_string()));
            assert_eq!(eval(create_symbol_value("x".to_string()), env),
                       Ok(create_string_value("abc".to_string())));
        }
        {
            let mut env = create_empty_env();
            env.map.insert("x".to_string(), create_integer_value(1));
            let mut closure_env = create_empty_env();
            closure_env.map.insert("y".to_string(), create_integer_value(2));
            let func = FuncKind::BuiltinFunc(Box::new(builtin_func));
            assert_eq!(eval(create_list_value(vec![
                create_closure_value(func, "x".to_string(), closure_env),
                create_integer_value(3),
            ]), env), Ok(create_integer_value(5)));
        }
        {
            let mut env = create_empty_env();
            env.map.insert("x".to_string(), create_integer_value(1));
            let mut closure_env = create_empty_env();
            closure_env.map.insert("x".to_string(), create_integer_value(2));
            let func = FuncKind::AstFunc(create_symbol_value("x".to_string()));
            assert_eq!(eval(create_list_value(vec![
                create_closure_value(func, "x".to_string(), closure_env),
                create_integer_value(3),
            ]), env), Ok(create_integer_value(3)));
        }
    }

    #[test]
    fn test_rejection() {
        use value::*;
        {
            let env = create_empty_env();
            assert_eq!(eval(create_symbol_value("x".to_string()), env),
                       Err(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException("x".to_string()), None)));
        }
    }
}
