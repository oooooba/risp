/*
references
http://wwwa.pikara.ne.jp/okojisan/haskell-sort/avl-treesort.html
https://www.cs.usfca.edu/~galles/visualization/AVLtree.html
*/

use std::cmp::Ordering;
use std::rc::Rc;
use std::iter::Iterator;

use core::pair::Pair;

use self::SubTreeState::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum SubTreeState {
    LeftIsHigher,
    HeightIsEqual,
    RightIsHigher,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Node<K: Clone + Ord, V: Clone + Ord> {
    state: SubTreeState,
    pair: Pair<K, V>,
    left: AVLTree<K, V>,
    right: AVLTree<K, V>,
}

/*
Some : Node
None : Leaf
*/
type TreeKind<K, V> = Option<Node<K, V>>;

#[derive(Debug, Eq, Clone)]
struct AVLTree<K: Clone + Ord, V: Clone + Ord>(Rc<TreeKind<K, V>>);

impl<K: Clone + Ord, V: Clone + Ord> Node<K, V> {
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

        assert_eq!(r_node.state, LeftIsHigher);
        let rl_node = (*r_node.left.0).clone().unwrap();
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

    fn insert_helper_balance_left(self, balances: bool) -> (bool, AVLTree<K, V>) {
        if balances {
            let (balances, new_node) = self.balance_left();
            (balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn insert_helper_balance_right(self, balances: bool) -> (bool, AVLTree<K, V>) {
        if balances {
            let (balances, new_node) = self.balance_right();
            (balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn delete_helper_balance_left(self, balances: bool) -> (bool, AVLTree<K, V>) {
        if balances {
            let (balances, new_node) = self.balance_right();
            (!balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn delete_helper_balance_right(self, balances: bool) -> (bool, AVLTree<K, V>) {
        if balances {
            let (balances, new_node) = self.balance_left();
            (!balances, AVLTree::create_node(new_node))
        } else {
            (false, AVLTree::create_node(self))
        }
    }

    fn delete_helper_delete_rightmost(&self) -> (Pair<K, V>, (bool, AVLTree<K, V>)) {
        match *self.right.0 {
            None => (self.pair.clone(), (true, self.left.clone())),
            Some(ref r_node) => {
                let (max_pair, (balances, newtree)) = r_node.delete_helper_delete_rightmost();
                (max_pair, Node { right: newtree, ..self.clone() }.delete_helper_balance_right(balances))
            }
        }
    }
}

impl<K: Clone + Ord, V: Clone + Ord> PartialEq for AVLTree<K, V> {
    fn eq(&self, other: &Self) -> bool {
        let mut lhs_items: Vec<Pair<K, V>> = self.iter().collect();
        let mut rhs_items: Vec<Pair<K, V>> = other.iter().collect();
        if lhs_items.len() != rhs_items.len() {
            return false;
        }
        lhs_items.sort();
        rhs_items.sort();
        return lhs_items == rhs_items;
    }
}

impl<K: Clone + Ord, V: Clone + Ord> AVLTree<K, V> {
    fn new(kind: TreeKind<K, V>) -> AVLTree<K, V> {
        AVLTree(Rc::new(kind))
    }

    fn create_leaf() -> AVLTree<K, V> {
        AVLTree::new(None)
    }

    fn create_node(node: Node<K, V>) -> AVLTree<K, V> {
        AVLTree::new(Some(node))
    }

    fn create_empty() -> AVLTree<K, V> {
        AVLTree::create_leaf()
    }

    fn create(mut items: Vec<(K, V)>) -> AVLTree<K, V> {
        let mut tree = AVLTree::create_leaf();
        items.reverse();
        while let Some(item) = items.pop() {
            tree = tree.insert(item.0, item.1).0;
        }
        tree
    }

    fn compare(lhs: &K, rhs: &K) -> Ordering {
        lhs.cmp(rhs)
    }

    fn insert_helper(&self, pair: Pair<K, V>) -> (bool, AVLTree<K, V>, Option<V>) {
        match *self.0 {
            None => (true, AVLTree::create_node(Node { state: HeightIsEqual, pair: pair, left: AVLTree::create_leaf(), right: AVLTree::create_leaf() }), None),
            Some(ref node) => match <AVLTree<K, V>>::compare(&pair.first, &node.pair.first) {
                Ordering::Equal => (false, AVLTree::create_node(Node { pair: pair, ..node.clone() }), Some(node.pair.second.clone())),
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

    fn insert(&self, key: K, value: V) -> (AVLTree<K, V>, Option<V>) {
        let r = self.insert_helper(Pair::new(key, value));
        (r.1, r.2)
    }

    fn delete_helper(&self, key: &K) -> (bool, AVLTree<K, V>, Option<V>) {
        match *self.0 {
            None => (false, AVLTree::create_leaf(), None),
            Some(ref node) => match <AVLTree<K, V>>::compare(key, &node.pair.first) {
                Ordering::Equal => {
                    match *node.left.0 {
                        None => (true, node.right.clone(), Some(node.pair.second.clone())),
                        Some(ref l_node) => {
                            let (pair, (balances, newtree)) = l_node.delete_helper_delete_rightmost();
                            let (balances, newtree) = Node { pair: pair, left: newtree, ..node.clone() }
                                .delete_helper_balance_left(balances);
                            (balances, newtree, Some(node.pair.second.clone()))
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

    fn delete(&self, key: &K) -> (AVLTree<K, V>, Option<V>) {
        let r = self.delete_helper(key);
        (r.1, r.2)
    }

    fn lookup(&self, key: &K) -> Option<&V> {
        match *self.0 {
            None => None,
            Some(ref node) => match <AVLTree<K, V>>::compare(&key, &node.pair.first) {
                Ordering::Equal => Some(&node.pair.second),
                Ordering::Less => node.left.lookup(key),
                Ordering::Greater => node.right.lookup(key),
            }
        }
    }

    fn iter(&self) -> AVLTreeIterator<K, V> {
        let mut stack = vec![];
        let mut cur = self.clone();
        while let Some(node) = (*cur.0).clone() {
            stack.push(cur);
            cur = node.left;
        }
        AVLTreeIterator(stack)
    }
}

#[derive(Debug)]
struct AVLTreeIterator<K: Clone + Ord, V: Clone + Ord>(Vec<AVLTree<K, V>>);

impl<K: Clone + Ord, V: Clone + Ord> Iterator for AVLTreeIterator<K, V> {
    type Item = Pair<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.0.pop() {
            Some(ref tree) => (*tree.0).clone().unwrap(),
            None => return None,
        };
        if node.right.0.is_some() {
            let mut cur = node.right;
            while let Some(node) = (*cur.0).clone() {
                self.0.push(cur);
                cur = node.left;
            }
        }
        Some(node.pair)
    }
}

// public interfaces
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TreeMap<K: Clone + Ord, V: Clone + Ord>(AVLTree<K, V>);

#[derive(Debug)]
pub struct TreeMapIterator<K: Clone + Ord, V: Clone + Ord>(AVLTreeIterator<K, V>);

impl<K: Clone + Ord, V: Clone + Ord> TreeMap<K, V> {
    pub fn create_empty() -> TreeMap<K, V> {
        TreeMap(AVLTree::create_empty())
    }

    pub fn create(items: Vec<(K, V)>) -> TreeMap<K, V> {
        TreeMap(AVLTree::create(items))
    }

    pub fn insert(&self, key: K, value: V) -> (TreeMap<K, V>, Option<V>) {
        let r = self.0.insert(key, value);
        (TreeMap(r.0), r.1)
    }

    pub fn delete(&self, key: &K) -> (TreeMap<K, V>, Option<V>) {
        let r = self.0.delete(key);
        (TreeMap(r.0), r.1)
    }

    pub fn lookup(&self, key: &K) -> Option<&V> {
        self.0.lookup(key)
    }

    pub fn iter(&self) -> TreeMapIterator<K, V> {
        TreeMapIterator(self.0.iter())
    }
}

impl<K: Clone + Ord, V: Clone + Ord> Iterator for TreeMapIterator<K, V> {
    type Item = Pair<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::value::{Value, ValuePtr};

    fn l() -> AVLTree<ValuePtr, ValuePtr> {
        AVLTree::create_leaf()
    }

    fn n(kv: isize, state: SubTreeState, left: AVLTree<ValuePtr, ValuePtr>, right: AVLTree<ValuePtr, ValuePtr>) -> AVLTree<ValuePtr, ValuePtr> {
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

            assert_eq!(t7.lookup(&i(0)), None);
            assert_eq!(t7.lookup(&i(1)), Some(&i(1)));
            assert_eq!(t7.lookup(&i(2)), Some(&i(2)));
            assert_eq!(t7.lookup(&i(3)), Some(&i(3)));
            assert_eq!(t7.lookup(&i(4)), Some(&i(4)));
            assert_eq!(t7.lookup(&i(5)), Some(&i(5)));
            assert_eq!(t7.lookup(&i(6)), Some(&i(6)));
            assert_eq!(t7.lookup(&i(7)), Some(&i(7)));
            assert_eq!(t7.lookup(&i(8)), None);

            let items: Vec<Pair<ValuePtr, ValuePtr>> = t7.iter().collect();
            assert_eq!(items, vec![
                Pair::new(i(1), i(1)),
                Pair::new(i(2), i(2)),
                Pair::new(i(3), i(3)),
                Pair::new(i(4), i(4)),
                Pair::new(i(5), i(5)),
                Pair::new(i(6), i(6)),
                Pair::new(i(7), i(7)),
            ]);

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

            let items: Vec<Pair<ValuePtr, ValuePtr>> = t14.iter().collect();
            assert_eq!(items, vec![]);

            assert_eq!(t6, t8);
            assert_eq!(t5, t9);
            assert_eq!(t4, t10);
            assert_eq!(t3, t11);
            assert_eq!(t2, t12);
            assert_eq!(t1, t13);
            assert_eq!(t0, t14);
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

            assert_eq!(t7.lookup(&i(0)), None);
            assert_eq!(t7.lookup(&i(1)), Some(&i(1)));
            assert_eq!(t7.lookup(&i(2)), Some(&i(2)));
            assert_eq!(t7.lookup(&i(3)), Some(&i(3)));
            assert_eq!(t7.lookup(&i(4)), Some(&i(4)));
            assert_eq!(t7.lookup(&i(5)), Some(&i(5)));
            assert_eq!(t7.lookup(&i(6)), Some(&i(6)));
            assert_eq!(t7.lookup(&i(7)), Some(&i(7)));
            assert_eq!(t7.lookup(&i(8)), None);

            let items: Vec<Pair<ValuePtr, ValuePtr>> = t7.iter().collect();
            assert_eq!(items, vec![
                Pair::new(i(1), i(1)),
                Pair::new(i(2), i(2)),
                Pair::new(i(3), i(3)),
                Pair::new(i(4), i(4)),
                Pair::new(i(5), i(5)),
                Pair::new(i(6), i(6)),
                Pair::new(i(7), i(7)),
            ]);

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

            let items: Vec<Pair<ValuePtr, ValuePtr>> = t14.iter().collect();
            assert_eq!(items, vec![]);

            assert_eq!(t6, t8);
            assert_eq!(t5, t9);
            assert_eq!(t4, t10);
            assert_eq!(t3, t11);
            assert_eq!(t2, t12);
            assert_eq!(t1, t13);
            assert_eq!(t0, t14);
        }
    }
}
