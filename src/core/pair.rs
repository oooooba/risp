#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Pair<F: Clone + Ord, S: Clone + Ord> {
    pub first: F,
    pub second: S,
}

impl<F: Clone + Ord, S: Clone + Ord> Pair<F, S> {
    pub fn new(first: F, second: S) -> Pair<F, S> {
        Pair { first: first, second: second }
    }
}
