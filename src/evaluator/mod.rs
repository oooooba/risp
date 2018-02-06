use value::{Value, ValueKind, FuncKind, Exception, ExceptionKind, Env, EnvPtr};

pub struct Interpreter {
    env: EnvPtr,
}

impl Interpreter {
    pub fn new(env: EnvPtr) -> Interpreter {
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
                match self.env.lookup(symbol) {
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
                            Some(val) => self.eval(val.clone())?,
                            None => return Err(Exception::new(ExceptionKind::EvaluatorTypeException("Closure".to_string(), "Unknown".to_string()), None)),
                        };
                        let new_env = Env::create(vec![(arg.clone(), arg_val)], Some(env.clone()));
                        match func {
                            &FuncKind::BuiltinFunc(ref f) => f(new_env.clone()),
                            &FuncKind::AstFunc(ref f) => {
                                let current_env = self.env.clone();
                                self.env = new_env;
                                let result = self.eval(f.clone());
                                self.env = current_env;
                                result
                            }
                        }
                    }
                    _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException("Closure".to_string(), "Unknown".to_string()), None)),
                }
            }
        }
    }
}

pub fn eval(ast: Value, env: EnvPtr) -> Result<Value, Exception> {
    Interpreter::new(env).eval(ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::*;

    fn builtin_func(env: EnvPtr) -> Result<Value, Exception> {
        let x_str = "x".to_string();
        let y_str = "y".to_string();
        let type_int_str = "Integer".to_string();
        let type_unknown_str = "Unknown".to_string();

        let x_val = env.lookup(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
        let x_int = match **x_val {
            ValueKind::IntegerValue(n) => n,
            _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(type_int_str, type_unknown_str), None)),
        };

        let y_val = env.lookup(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;
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
            let env = Env::create_empty_env();
            assert_eq!(eval(create_keyword_value("XYZ".to_string()), env),
                       Ok(create_keyword_value("XYZ".to_string())));
        }
        {
            let env = Env::create_initialized_env(vec![
                ("x".to_string(), create_string_value("abc".to_string())),
            ]);
            assert_eq!(eval(create_symbol_value("x".to_string()), env),
                       Ok(create_string_value("abc".to_string())));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), create_integer_value(1)),
                ("y".to_string(), create_integer_value(2)),
            ], None);
            let closure_env = Env::create(vec![
                ("y".to_string(), create_integer_value(3)),
            ], None);
            let func = FuncKind::BuiltinFunc(Box::new(builtin_func));
            assert_eq!(eval(create_list_value(vec![
                create_closure_value(func, "x".to_string(), closure_env),
                create_integer_value(4),
            ]), env), Ok(create_integer_value(7)));
        }
        {
            let env = Env::create(vec![
                ("x".to_string(), create_integer_value(1)),
            ], None);
            let closure_env = Env::create(vec![
                ("x".to_string(), create_integer_value(2)),
            ], None);
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
            let env = Env::create_empty_env();
            assert_eq!(eval(create_symbol_value("x".to_string()), env),
                       Err(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException("x".to_string()), None)));
        }
    }
}
