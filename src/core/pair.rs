#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pair<F: Clone + Ord, S: Clone> {
    pub first: F,
    pub second: S,
}

impl<F: Clone + Ord, S: Clone> Pair<F, S> {
    pub fn new(first: F, second: S) -> Pair<F, S> {
        Pair { first: first, second: second }
    }
}
