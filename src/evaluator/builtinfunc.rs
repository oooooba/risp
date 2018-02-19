use core::value::{Value, ValueKind, ValuePtr};
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;

enum IntegerBuiltinOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
}

fn op_common_integer(kind: IntegerBuiltinOperatorKind, env: EnvPtr) -> Result<ValuePtr, Exception> {
    let lhs_val = env.lookup(&"%1".to_string()).unwrap();
    let lhs_int = match lhs_val.kind {
        ValueKind::IntegerValue(n) => n,
        _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_integer(), lhs_val.kind.as_type_str()), None)),
    };

    let rhs_val = env.lookup(&"%2".to_string()).unwrap();
    let rhs_int = match rhs_val.kind {
        ValueKind::IntegerValue(n) => n,
        _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_integer(), rhs_val.kind.as_type_str()), None)),
    };

    use self::IntegerBuiltinOperatorKind::*;
    let val = match kind {
        Add => lhs_int + rhs_int,
        Sub => lhs_int - rhs_int,
        Mul => lhs_int * rhs_int,
        Div => lhs_int / rhs_int,
    };
    Ok(Value::create_integer(val))
}

pub fn op_add_integer(env: EnvPtr) -> Result<ValuePtr, Exception> {
    op_common_integer(IntegerBuiltinOperatorKind::Add, env)
}

pub fn op_sub_integer(env: EnvPtr) -> Result<ValuePtr, Exception> {
    op_common_integer(IntegerBuiltinOperatorKind::Sub, env)
}

pub fn op_mul_integer(env: EnvPtr) -> Result<ValuePtr, Exception> {
    op_common_integer(IntegerBuiltinOperatorKind::Mul, env)
}

pub fn op_div_integer(env: EnvPtr) -> Result<ValuePtr, Exception> {
    op_common_integer(IntegerBuiltinOperatorKind::Div, env)
}

pub fn op_equal(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let x_str = "x".to_string();
    let y_str = "y".to_string();

    let x_val = env.lookup(&x_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(x_str), None))?;
    let y_val = env.lookup(&y_str).ok_or(Exception::new(ExceptionKind::EvaluatorUndefinedSymbolException(y_str), None))?;

    Ok(Value::create_boolean(x_val == y_val))
}
