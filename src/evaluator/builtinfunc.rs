use core::value::{Value, ValueKind, ValuePtr};
use core::exception::{Exception, ExceptionKind};
use core::env::EnvPtr;

enum IntegerBuiltinOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
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
        Add => Value::create_integer(lhs_int + rhs_int),
        Sub => Value::create_integer(lhs_int - rhs_int),
        Mul => Value::create_integer(lhs_int * rhs_int),
        Div => Value::create_integer(lhs_int / rhs_int),
        Lt => Value::create_boolean(lhs_int < rhs_int),
        Gt => Value::create_boolean(lhs_int > rhs_int),
    };
    Ok(val)
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

pub fn op_lt_integer(env: EnvPtr) -> Result<ValuePtr, Exception> {
    op_common_integer(IntegerBuiltinOperatorKind::Lt, env)
}

pub fn op_gt_integer(env: EnvPtr) -> Result<ValuePtr, Exception> {
    op_common_integer(IntegerBuiltinOperatorKind::Gt, env)
}

pub fn op_equal(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let x_val = env.lookup_nth_param(1).unwrap();
    let y_val = env.lookup_nth_param(2).unwrap();
    Ok(Value::create_boolean(x_val == y_val))
}

pub fn cons(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let elem_val = env.lookup_nth_param(1).unwrap();
    let list_val = env.lookup_nth_param(2).unwrap();
    if !list_val.is_list() {
        return Err(Exception::new(ExceptionKind::EvaluatorTypeException(ValueKind::type_str_list(), list_val.kind.as_type_str()), None));
    }
    Ok(Value::create_list(list_val.get_as_list().unwrap().clone().cons(elem_val.clone())))
}

pub fn builtinfunc_first(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    match val.kind {
        ValueKind::ListValue(ref list) => Ok(list.car().map_or(Value::create_nil(), |car| car.clone())),
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
        ValueKind::ListValue(ref list) => Ok(list.cdr().map_or(Value::create_nil(), |cdr|
            Value::create_list(cdr))),
        ValueKind::NilValue => Ok(Value::create_list_empty()),
        ValueKind::VectorValue(_) => {
            let mut iter = val.iter();
            iter.next();
            Ok(iter.rest())
        }
        _ => Err(Exception::new(ExceptionKind::EvaluatorTypeException("Seq", val.kind.as_type_str()), None))
    }
}

pub fn builtinfunc_list_q(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    Ok(Value::create_boolean(val.is_list()))
}

pub fn builtinfunc_nil_q(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    Ok(Value::create_boolean(val.is_nil()))
}

pub fn builtinfunc_println(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    println!("{}", val.to_string());
    Ok(Value::create_nil())
}

pub fn builtinfunc_get(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let obj = env.lookup_nth_param(1).unwrap();
    let key = env.lookup_nth_param(2).unwrap();
    let val = match obj.kind {
        ValueKind::MapValue(ref map) => map.lookup(key).or(Some(&Value::create_nil())).unwrap().clone(),
        ValueKind::VectorValue(ref vector) if key.is_integer() => {
            let index = key.get_as_integer().unwrap().clone();
            if 0 <= index && index < vector.len() as isize {
                vector[index as usize].clone()
            } else {
                Value::create_nil()
            }
        }
        _ => Value::create_nil(),
    };
    Ok(val)
}

pub fn builtinfunc_vector(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let args = env.lookup_nth_param(1).unwrap();
    let values = args.iter().collect();
    Ok(Value::create_vector(values))
}

pub fn builtinfunc_vec(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let collection = env.lookup_nth_param(1).unwrap();
    let mut iter = collection.iter();
    let mut values = vec![];
    while let Some(ref item) = iter.next() {
        let item = match item.kind {
            ValueKind::InternalPairValue(ref pair) => Value::create_vector(
                vec![pair.first.clone(), pair.second.clone()]),
            _ => item.clone(),
        };
        values.push(item);
    }
    Ok(Value::create_vector(values))
}

pub fn builtinfunc_boolean(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let val = env.lookup_nth_param(1).unwrap();
    let b = match val.kind {
        ValueKind::BooleanValue(false) => false,
        ValueKind::NilValue => false,
        _ => true,
    };
    Ok(Value::create_boolean(b))
}

pub fn builtinfunc_conj(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let collection = env.lookup_nth_param(1).unwrap();
    let values = env.lookup_nth_param(2).unwrap();
    match collection.kind {
        ValueKind::ListValue(ref list) => {
            let mut new_collection = list.clone();
            for val in values.iter() {
                new_collection = new_collection.cons(val);
            }
            Ok(Value::create_list(new_collection))
        }
        ValueKind::VectorValue(ref vector) => {
            let mut new_collection = vector.clone();
            new_collection.append(&mut values.iter().collect());
            Ok(Value::create_vector(new_collection))
        }
        _ => unimplemented!(),
    }
}

pub fn builtinfunc_set(env: EnvPtr) -> Result<ValuePtr, Exception> {
    let collection = env.lookup_nth_param(1).unwrap();
    let values = collection.iter().collect();
    Ok(Value::create_set(values))
}
