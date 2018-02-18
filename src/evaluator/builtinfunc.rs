use core::value::{Value, ValueKind, ValuePtr};
use core::exception::{Exception, ExceptionKind};
use core::env::{EnvPtr};

pub fn op_add_integer(env: EnvPtr)-> Result<ValuePtr, Exception> {
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

pub fn op_equal(env: EnvPtr)-> Result<ValuePtr, Exception> {
    let x_str = "x".to_string();
    let y_str = "y".to_string();

    let x_val = env.lookup(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
    let y_val = env.lookup(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;

    Ok(Value::create_boolean(x_val == y_val))
}
