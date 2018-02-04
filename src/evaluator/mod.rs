use value::{Value, ValueKind, Exception, ExceptionKind, Env};

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
            ListValue(_) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}

pub fn eval(ast: Value, env: Env) -> Result<Value, Exception> {
    Interpreter::new(env).eval(ast)
}

#[cfg(test)]
mod tests {
    use super::*;

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
