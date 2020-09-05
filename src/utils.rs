use crate::{
    common::*,
    var::{TypeVar, WherePredicateVar},
};

#[derive(Debug, Clone)]
pub struct Shared<T>(ByAddress<Rc<T>>);

impl<T> Shared<T> {
    pub fn new(value: T) -> Shared<T> {
        Shared(ByAddress(Rc::new(value)))
    }
}

impl<T> PartialEq<Shared<T>> for Shared<T> {
    fn eq(&self, other: &Shared<T>) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> Eq for Shared<T> {}

impl<T> PartialOrd<Shared<T>> for Shared<T> {
    fn partial_cmp(&self, other: &Shared<T>) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Ord for Shared<T> {
    fn cmp(&self, other: &Shared<T>) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for Shared<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> Borrow<T> for Shared<T> {
    fn borrow(&self) -> &T {
        self.0.deref().deref()
    }
}

impl<T> Deref for Shared<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref().deref()
    }
}

#[derive(Debug, Clone)]
pub struct SharedCell<T>(ByAddress<Rc<RefCell<T>>>);

impl<T> SharedCell<T> {
    pub fn new(value: T) -> SharedCell<T> {
        SharedCell(ByAddress(Rc::new(RefCell::new(value))))
    }

    pub fn borrow(&self) -> Ref<T> {
        self.0.deref().deref().borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<T> {
        self.0.deref().deref().borrow_mut()
    }

    pub fn deep_clone(&self) -> SharedCell<T>
    where
        T: Clone,
    {
        SharedCell(ByAddress(Rc::new(RefCell::new(
            (**self.0).borrow().clone(),
        ))))
    }
}

impl<T> PartialEq<SharedCell<T>> for SharedCell<T> {
    fn eq(&self, other: &SharedCell<T>) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> Eq for SharedCell<T> {}

impl<T> PartialOrd<SharedCell<T>> for SharedCell<T> {
    fn partial_cmp(&self, other: &SharedCell<T>) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Ord for SharedCell<T> {
    fn cmp(&self, other: &SharedCell<T>) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for SharedCell<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

pub trait VecTranspose<T> {
    fn transpose(&self) -> Vec<Vec<T>>
    where
        T: Clone;
    fn transpose_inplace(self) -> Vec<Vec<T>>
    where
        T: Clone;
}

impl<T> VecTranspose<T> for Vec<Vec<T>> {
    fn transpose(&self) -> Vec<Vec<T>>
    where
        T: Clone,
    {
        if self.is_empty() {
            return vec![];
        }

        let n_rows = self.len();
        let n_cols = self[0].len();

        (0..n_cols)
            .map(|col_idx| {
                (0..n_rows)
                    .map(|row_idx| self[row_idx][col_idx].clone())
                    .collect()
            })
            .collect()
    }

    fn transpose_inplace(self) -> Vec<Vec<T>>
    where
        T: Clone,
    {
        if self.is_empty() {
            return self;
        }

        let n_rows = self.len();
        let n_cols = self[0].len();
        self.iter().for_each(|row| assert_eq!(row.len(), n_cols));

        let mut buffer = vec![
            unsafe {
                let mut v = Vec::with_capacity(n_rows);
                v.set_len(n_rows);
                v
            };
            n_cols
        ];
        self.into_iter().enumerate().for_each(|(row_index, row)| {
            row.into_iter().enumerate().for_each(|(col_index, value)| {
                buffer[col_index][row_index] = value;
            })
        });

        buffer
    }
}

pub trait IntoRc {
    type Inner;

    fn into_rc(self) -> Rc<Self::Inner>;
}

impl IntoRc for TypeVar {
    type Inner = TypeVar;

    fn into_rc(self) -> Rc<Self::Inner> {
        Rc::new(self)
    }
}

impl IntoRc for Rc<TypeVar> {
    type Inner = TypeVar;

    fn into_rc(self) -> Rc<Self::Inner> {
        self
    }
}

impl IntoRc for WherePredicateVar {
    type Inner = WherePredicateVar;

    fn into_rc(self) -> Rc<Self::Inner> {
        Rc::new(self)
    }
}

impl IntoRc for Rc<WherePredicateVar> {
    type Inner = WherePredicateVar;

    fn into_rc(self) -> Rc<Self::Inner> {
        self
    }
}
