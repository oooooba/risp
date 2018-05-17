use std::iter::Iterator;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Cell<T: Clone> {
    car: T,
    cdr: CellPtr<T>,
}

impl<T: Clone> Cell<T> {
    fn new(car: T, cdr: CellPtr<T>) -> Cell<T> {
        Cell { car: car, cdr: cdr }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct CellPtr<T: Clone>(Option<Rc<Cell<T>>>);

impl<T: Clone> CellPtr<T> {
    fn create_nil() -> Self {
        CellPtr(None)
    }

    fn cons(car: T, cdr: Self) -> Self {
        CellPtr(Some(Rc::new(Cell::new(car, cdr))))
    }
}

#[derive(Debug)]
struct CellIterator<T: Clone>(CellPtr<T>);

impl<T: Clone> Iterator for CellIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let cell = match (self.0).0 {
            None => return None,
            Some(ref p) => p.clone(),
        };
        let item = cell.car.clone();
        self.0 = cell.cdr.clone();
        Some(item)
    }
}

// public interfaces
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct List<T: Clone>(CellPtr<T>);

impl<T: Clone> List<T> {
    pub fn create_empty() -> List<T> {
        List(CellPtr::create_nil())
    }

    pub fn cons(&self, item: T) -> List<T> {
        List(CellPtr::cons(item, self.0.clone()))
    }

    pub fn iter(&self) -> ListIterator<T> {
        ListIterator(CellIterator(self.0.clone()))
    }
}

#[derive(Debug)]
pub struct ListIterator<T: Clone>(CellIterator<T>);

impl<T: Clone> Iterator for ListIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.0.next()
    }
}
