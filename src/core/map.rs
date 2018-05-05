/*
references
http://wwwa.pikara.ne.jp/okojisan/haskell-sort/avl-treesort.html
https://www.cs.usfca.edu/~galles/visualization/AVLtree.html
*/

use std::cmp::Ordering;
use std::rc::Rc;

use core::value::{Value, ValuePtr};

use self::SubTreeState::*;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Pair {
    key: ValuePtr,
    value: ValuePtr,
}

impl Pair {
    fn new(key: ValuePtr, value: ValuePtr) -> Pair {
        Pair { key: key, value: value }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum SubTreeState {
    LeftIsHigher,
    HeightIsEqual,
    RightIsHigher,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Node {
    state: SubTreeState,
    pair: Pair,
    left: AVLTree,
    right: AVLTree,
}

/*
Some : Node
None : Leaf
*/
type TreeKind = Option<Node>;

#[derive(Debug, PartialEq, Eq, Clone)]
struct AVLTree(Rc<TreeKind>);

impl Node {
    fn balance_left(self) -> (bool, Self) {
        match self.state {
            HeightIsEqual => return (true, Node { state: LeftIsHigher, ..self }),
            RightIsHigher => return (false, Node { state: HeightIsEqual, ..self }),
            _ => (),
        }

        assert_eq!(self.state, LeftIsHigher);
        let l_node = (*self.left.0).clone().unwrap();
        match l_node.state {
            HeightIsEqual => return (true, Node {
                state: RightIsHigher,
                right: AVLTree::create_node(Node { state: LeftIsHigher, left: l_node.right, ..self }),
                ..l_node
            }),
            LeftIsHigher => return (false, Node {
                state: HeightIsEqual,
                right: AVLTree::create_node(Node { state: HeightIsEqual, left: l_node.right, ..self }),
                ..l_node
            }),
            _ => (),
        }

        assert_eq!(l_node.state, RightIsHigher);
        let lr_node = (*l_node.right.0).clone().unwrap();
        match lr_node.state {
            LeftIsHigher => (false, Node {
                state: HeightIsEqual,
                pair: lr_node.pair,
                left: AVLTree::create_node(Node { state: HeightIsEqual, right: lr_node.left, ..l_node }),
                right: AVLTree::create_node(Node { state: RightIsHigher, left: lr_node.right, ..self }),
            }),
            RightIsHigher => (false, Node {
                state: HeightIsEqual,
                pair: lr_node.pair,
                left: AVLTree::create_node(Node { state: LeftIsHigher, right: lr_node.left, ..l_node }),
                right: AVLTree::create_node(Node { state: HeightIsEqual, left: lr_node.right, ..self }),
            }),
            HeightIsEqual => (false, Node {
                state: HeightIsEqual,
                pair: lr_node.pair,
                left: AVLTree::create_node(Node { state: HeightIsEqual, right: lr_node.left, ..l_node }),
                right: AVLTree::create_node(Node { state: HeightIsEqual, left: lr_node.right, ..self }),
            }),
        }
    }

    fn balance_right(self) -> (bool, Self) {
        match self.state {
            HeightIsEqual => return (true, Node { state: RightIsHigher, ..self }),
            LeftIsHigher => return (false, Node { state: HeightIsEqual, ..self }),
            _ => (),
        }

        assert_eq!(self.state, RightIsHigher);
        let r_node = (*self.right.0).clone().unwrap();
        match r_node.state {
            HeightIsEqual => return (true, Node {
                state: LeftIsHigher,
                left: AVLTree::create_node(Node { state: RightIsHigher, right: r_node.left, ..self }),
                ..r_node
            }),
            RightIsHigher => return (false, Node {
                state: HeightIsEqual,
                left: AVLTree::create_node(Node { state: HeightIsEqual, right: r_node.left, ..self }),
                ..r_node
            }),
            _ => (),
        }

        assert_eq!(r_node.state, RightIsHigher);
        let rl_node = (*r_node.right.0).clone().unwrap();
        match rl_node.state {
            LeftIsHigher => (false, Node {
                state: HeightIsEqual,
                pair: rl_node.pair,
                left: AVLTree::create_node(Node { state: HeightIsEqual, right: rl_node.left, ..self }),
                right: AVLTree::create_node(Node { state: RightIsHigher, left: rl_node.right, ..r_node }),
            }),
            RightIsHigher => (false, Node {
                state: HeightIsEqual,
                pair: rl_node.pair,
                left: AVLTree::create_node(Node { state: LeftIsHigher, right: rl_node.left, ..self }),
                right: AVLTree::create_node(Node { state: HeightIsEqual, left: rl_node.right, ..r_node }),
            }),
            HeightIsEqual => (false, Node {
                state: HeightIsEqual,
                pair: rl_node.pair,
                left: AVLTree::create_node(Node { state: HeightIsEqual, right: rl_node.left, ..self }),
                right: AVLTree::create_node(Node { state: HeightIsEqual, left: rl_node.right, ..r_node }),
            }),
        }
    }

    fn insert_helper_balance_left(self, balances: bool) -> (bool, AVLTree) {
        if balances {
            let (balances, new_node) = self.balance_left();
            (balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn insert_helper_balance_right(self, balances: bool) -> (bool, AVLTree) {
        if balances {
            let (balances, new_node) = self.balance_right();
            (balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn delete_helper_balance_left(self, balances: bool) -> (bool, AVLTree) {
        if balances {
            let (balances, new_node) = self.balance_right();
            (!balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn delete_helper_balance_right(self, balances: bool) -> (bool, AVLTree) {
        if balances {
            let (balances, new_node) = self.balance_left();
            (!balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn delete_helper_delete_rightmost(&self) -> (Pair, (bool, AVLTree)) {
        match *self.right.0 {
            None => (self.pair.clone(), (true, self.left.clone())),
            Some(ref r_node) => {
                let (max_pair, (balances, newtree)) = r_node.delete_helper_delete_rightmost();
                (max_pair, Node { right: newtree, ..self.clone() }.delete_helper_balance_right(balances))
            }
        }
    }
}

impl AVLTree {
    fn new(kind: TreeKind) -> AVLTree {
        AVLTree(Rc::new(kind))
    }

    fn create_leaf() -> AVLTree {
        AVLTree::new(None)
    }

    fn create_node(node: Node) -> AVLTree {
        AVLTree::new(Some(node))
    }

    fn create_empty() -> AVLTree {
        AVLTree::create_leaf()
    }

    fn compare(lhs: &ValuePtr, rhs: &ValuePtr) -> Ordering {
        lhs.cmp(rhs)
    }

    fn insert_helper(&self, pair: Pair) -> (bool, AVLTree, Option<ValuePtr>) {
        match *self.0 {
            None => (true, AVLTree::create_node(Node { state: HeightIsEqual, pair: pair, left: AVLTree::create_leaf(), right: AVLTree::create_leaf() }), None),
            Some(ref node) => match AVLTree::compare(&pair.key, &node.pair.key) {
                Ordering::Equal => (false, AVLTree::create_node(Node { pair: pair, ..node.clone() }), Some(node.pair.value.clone())),
                Ordering::Less => {
                    let (balances, newtree, prev_val) = node.left.insert_helper(pair);
                    let (balances, newtree) = Node { left: newtree, ..node.clone() }
                        .insert_helper_balance_left(balances);
                    (balances, newtree, prev_val)
                }
                Ordering::Greater => {
                    let (balances, newtree, prev_val) = node.right.insert_helper(pair);
                    let (balances, newtree) = Node { right: newtree, ..node.clone() }
                        .insert_helper_balance_right(balances);
                    (balances, newtree, prev_val)
                }
            }
        }
    }


    fn insert(&self, key: ValuePtr, value: ValuePtr) -> (AVLTree, Option<ValuePtr>) {
        let r = self.insert_helper(Pair::new(key, value));
        (r.1, r.2)
    }

    fn delete_helper(&self, key: &ValuePtr) -> (bool, AVLTree, Option<ValuePtr>) {
        match *self.0 {
            None => (false, AVLTree::create_leaf(), None),
            Some(ref node) => match AVLTree::compare(key, &node.pair.key) {
                Ordering::Equal => {
                    match *node.left.0 {
                        None => (true, node.right.clone(), Some(node.pair.value.clone())),
                        Some(ref l_node) => {
                            let (pair, (balances, newtree)) = l_node.delete_helper_delete_rightmost();
                            let (balances, newtree) = Node { pair: pair, left: newtree, ..node.clone() }
                                .delete_helper_balance_left(balances);
                            (balances, newtree, Some(node.pair.value.clone()))
                        }
                    }
                }
                Ordering::Less => {
                    let (balances, newtree, prev_val) = node.left.delete_helper(key);
                    let (balances, newtree) = Node { left: newtree, ..node.clone() }
                        .delete_helper_balance_left(balances);
                    (balances, newtree, prev_val)
                }
                Ordering::Greater => {
                    let (balances, newtree, prev_val) = node.right.delete_helper(key);
                    let (balances, newtree) = Node { right: newtree, ..node.clone() }
                        .delete_helper_balance_right(balances);
                    (balances, newtree, prev_val)
                }
            }
        }
    }

    fn delete(&self, key: &ValuePtr) -> (AVLTree, Option<ValuePtr>) {
        let r = self.delete_helper(key);
        (r.1, r.2)
    }
}

// public interface
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TreeMap(AVLTree);

impl TreeMap {
    pub fn create_empty() -> ValuePtr {
        Value::create_mapx(TreeMap(AVLTree::create_empty()))
    }

    pub fn insert(&self, key: ValuePtr, value: ValuePtr) -> (ValuePtr, Option<ValuePtr>) {
        let r = self.0.insert(key, value);
        (Value::create_mapx(TreeMap(r.0)), r.1)
    }

    pub fn delete(&self, key: &ValuePtr) -> (ValuePtr, Option<ValuePtr>) {
        let r = self.0.delete(key);
        (Value::create_mapx(TreeMap(r.0)), r.1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn l() -> AVLTree {
        AVLTree::create_leaf()
    }

    fn n(kv: isize, state: SubTreeState, left: AVLTree, right: AVLTree) -> AVLTree {
        AVLTree::create_node(Node { state: state, pair: Pair::new(i(kv), i(kv)), left: left, right: right })
    }

    fn i(i: isize) -> ValuePtr {
        Value::create_integer(i)
    }

    const H: SubTreeState = HeightIsEqual;
    const L: SubTreeState = LeftIsHigher;
    const R: SubTreeState = RightIsHigher;

    #[test]
    fn test_basic() {
        {
            let t0 = AVLTree::create_empty();
            assert_eq!(t0, l());
            let (t1, p1) = t0.insert(i(1), i(1));
            assert_eq!(p1, None);
            assert_eq!(t1, n(1, H, l(), l()));
            let (t2, p2) = t1.insert(i(2), i(2));
            assert_eq!(p2, None);
            assert_eq!(t2, n(1, R, l(), n(2, H, l(), l())));
            let (t3, p3) = t2.insert(i(3), i(3));
            assert_eq!(p3, None);
            assert_eq!(t3, n(2, H,
                             n(1, H, l(), l()),
                             n(3, H, l(), l())));
            let (t4, p4) = t3.insert(i(4), i(4));
            assert_eq!(p4, None);
            assert_eq!(t4, n(2, R,
                             n(1, H, l(), l()),
                             n(3, R, l(), n(4, H, l(), l()))));
            let (t5, p5) = t4.insert(i(5), i(5));
            assert_eq!(p5, None);
            assert_eq!(t5, n(2, R,
                             n(1, H, l(), l()),
                             n(4, H,
                               n(3, H, l(), l()),
                               n(5, H, l(), l()))));
            let (t6, p6) = t5.insert(i(6), i(6));
            assert_eq!(p6, None);
            assert_eq!(t6, n(4, H,
                             n(2, H,
                               n(1, H, l(), l()),
                               n(3, H, l(), l())),
                             n(5, R,
                               l(),
                               n(6, H, l(), l()))));
            let (t7, p7) = t6.insert(i(7), i(7));
            assert_eq!(p7, None);
            assert_eq!(t7, n(4, H,
                             n(2, H,
                               n(1, H, l(), l()),
                               n(3, H, l(), l())),
                             n(6, H,
                               n(5, H, l(), l()),
                               n(7, H, l(), l()))));
            let (t8, p8) = t7.delete(&i(7));
            assert_eq!(p8, Some(i(7)));
            assert_eq!(t8, n(4, H,
                             n(2, H,
                               n(1, H, l(), l()),
                               n(3, H, l(), l())),
                             n(6, L,
                               n(5, H, l(), l()),
                               l())));
            let (t9, p9) = t8.delete(&i(6));
            assert_eq!(p9, Some(i(6)));
            assert_eq!(t9, n(4, L,
                             n(2, H,
                               n(1, H, l(), l()),
                               n(3, H, l(), l())),
                             n(5, H, l(), l())));
            let (t10, p10) = t9.delete(&i(5));
            assert_eq!(p10, Some(i(5)));
            assert_eq!(t10, n(2, R,
                              n(1, H, l(), l()),
                              n(4, L,
                                n(3, H, l(), l()),
                                l())));
            let (t11, p11) = t10.delete(&i(4));
            assert_eq!(p11, Some(i(4)));
            assert_eq!(t11, n(2, H,
                              n(1, H, l(), l()),
                              n(3, H, l(), l())));
            let (t12, p12) = t11.delete(&i(3));
            assert_eq!(p12, Some(i(3)));
            assert_eq!(t12, n(2, L, n(1, H, l(), l()), l()));
            let (t13, p13) = t12.delete(&i(2));
            assert_eq!(p13, Some(i(2)));
            assert_eq!(t13, n(1, H, l(), l()));
            let (t14, p14) = t13.delete(&i(1));
            assert_eq!(p14, Some(i(1)));
            assert_eq!(t14, l());
        }
        {
            let t0 = AVLTree::create_empty();
            assert_eq!(t0, l());
            let (t1, p1) = t0.insert(i(7), i(7));
            assert_eq!(p1, None);
            assert_eq!(t1, n(7, H, l(), l()));
            let (t2, p2) = t1.insert(i(6), i(6));
            assert_eq!(p2, None);
            assert_eq!(t2, n(7, L, n(6, H, l(), l()), l()));
            let (t3, p3) = t2.insert(i(5), i(5));
            assert_eq!(p3, None);
            assert_eq!(t3, n(6, H,
                             n(5, H, l(), l()),
                             n(7, H, l(), l())));
            let (t4, p4) = t3.insert(i(4), i(4));
            assert_eq!(p4, None);
            assert_eq!(t4, n(6, L,
                             n(5, L, n(4, H, l(), l()), l()),
                             n(7, H, l(), l())));
            let (t5, p5) = t4.insert(i(3), i(3));
            assert_eq!(p5, None);
            assert_eq!(t5, n(6, L,
                             n(4, H,
                               n(3, H, l(), l()),
                               n(5, H, l(), l())),
                             n(7, H, l(), l())));
            let (t6, p6) = t5.insert(i(2), i(2));
            assert_eq!(p6, None);
            assert_eq!(t6, n(4, H,
                             n(3, L,
                               n(2, H, l(), l()), l()),
                             n(6, H,
                               n(5, H, l(), l()),
                               n(7, H, l(), l()))));
            let (t7, p7) = t6.insert(i(1), i(1));
            assert_eq!(p7, None);
            assert_eq!(t7, n(4, H,
                             n(2, H,
                               n(1, H, l(), l()),
                               n(3, H, l(), l())),
                             n(6, H,
                               n(5, H, l(), l()),
                               n(7, H, l(), l()))));
            let (t8, p8) = t7.delete(&i(1));
            assert_eq!(p8, Some(i(1)));
            assert_eq!(t8, n(4, H,
                             n(2, R,
                               l(),
                               n(3, H, l(), l())),
                             n(6, H,
                               n(5, H, l(), l()),
                               n(7, H, l(), l()))));
            let (t9, p9) = t8.delete(&i(2));
            assert_eq!(p9, Some(i(2)));
            assert_eq!(t9, n(4, R,
                             n(3, H,
                               l(),
                               l()),
                             n(6, H,
                               n(5, H, l(), l()),
                               n(7, H, l(), l()))));
            let (t10, p10) = t9.delete(&i(3));
            assert_eq!(p10, Some(i(3)));
            assert_eq!(t10, n(6, L,
                              n(4, R,
                                l(),
                                n(5, H, l(), l())),
                              n(7, H, l(), l())));
            let (t11, p11) = t10.delete(&i(4));
            assert_eq!(p11, Some(i(4)));
            assert_eq!(t11, n(6, H,
                              n(5, H, l(), l()),
                              n(7, H, l(), l())));
            let (t12, p12) = t11.delete(&i(5));
            assert_eq!(p12, Some(i(5)));
            assert_eq!(t12, n(6, R,
                              l(),
                              n(7, H, l(), l())));
            let (t13, p13) = t12.delete(&i(6));
            assert_eq!(p13, Some(i(6)));
            assert_eq!(t13, n(7, H, l(), l()));
            let (t14, p14) = t13.delete(&i(7));
            assert_eq!(p14, Some(i(7)));
            assert_eq!(t14, l());
        }
    }
}
