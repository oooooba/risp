use core::value::{Value, ValuePtr, BuiltinFuncType, ApplicableBodyKind, Applicable, Pattern};

#[derive(Debug, PartialEq, Eq)]
enum Color {
    Red,
    Black,
}

#[derive(Debug, PartialEq, Eq)]
struct Node {
    color: Color,
    key: ValuePtr,
    value: ValuePtr,
    left: ValuePtr, // must be MapValue
    right: ValuePtr, // must be MapValue
}

#[derive(Debug, PartialEq, Eq)]
enum MapKind {
    Leaf,
    Node(Node),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Map(MapKind);