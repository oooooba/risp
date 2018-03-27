use core::value::{Value, ValueKind, ValuePtr, ListKind};
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;

enum IntegerBuiltinOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
}

fn op_common_integer(kind: IntegerBuiltinOperatorKind, env: EnvPtr) -> Result<ValuePtr, Exception> {
    let lhs_val = env.lookup_nth_param(1).unwrap();
    let lhs_int = match lhs_val.kind {
        ValueKind::IntegerValue(n) => n,
        _ => return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_integer(), lhs_val.kind.as_type_str()), None)),
    };

    let rhs_val = env.lookup_nth_param(2).unwrap();
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
    let x_val = env.lookup_nth_param(1).unwrap();
    let y_val = env.lookup_nth_param(2).unwrap();
    Ok(Value::create_boolean(x_val == y_val))
}

pub fn cons(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let elem_val = env.lookup_nth_param(1).unwrap();
    let list_val = env.lookup_nth_param(2).unwrap();
    if !list_val.kind.is_list() {
        return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_list(), list_val.kind.as_type_str()), None));
    }
    Ok(Value::create_list(ListKind::ConsList(elem_val.clone(), list_val.clone())))
}

pub fn builtinfunc_first(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    match val.kind {
        ValueKind::ListValue(ListKind::EmptyList) => Ok(Value::create_nil()),
        ValueKind::ListValue(ListKind::ConsList(ref car, _)) => Ok(car.clone()),
        ValueKind::VectorValue(ref vector) => if vector.len() == 0 {
            Ok(Value::create_nil())
        } else {
            Ok(vector[0].clone())
        }
        _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException("Seq", val.kind.as_type_str()), None))
    }
}

pub fn builtinfunc_rest(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    match val.kind {
        ValueKind::ListValue(ListKind::ConsList(_, ref cdr)) => Ok(cdr.clone()),
        ValueKind::ListValue(ListKind::EmptyList) => Ok(val.clone()),
        ValueKind::NilValue => Ok(Value::create_list(ListKind::EmptyList)),
        ValueKind::VectorValue(_) => {
            let mut iter = Value::iter(val);
            iter.next();
            Ok(iter.rest())
        }
        _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException("Seq", val.kind.as_type_str()), None))
    }
}

pub fn builtinfunc_nil_q(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    Ok(Value::create_boolean(val.kind.is_nil()))
}

pub fn builtinfunc_println(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    println!("{}", val.to_string());
    Ok(Value::create_nil())
}
