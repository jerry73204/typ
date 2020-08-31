use crate::common::*;

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

pub fn transpose<T>(values: &Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    if values.is_empty() {
        return vec![];
    }

    let n_rows = values.len();
    let n_cols = values[0].len();

    (0..n_cols)
        .map(|col_idx| {
            (0..n_rows)
                .map(|row_idx| values[row_idx][col_idx].clone())
                .collect()
        })
        .collect()
}

pub trait IntoOwnedTokens {
    fn into_owned_tokens(self) -> TokenStream;
}

impl IntoOwnedTokens for TokenStream {
    fn into_owned_tokens(self) -> TokenStream {
        self
    }
}

impl IntoOwnedTokens for Rc<TokenStream> {
    fn into_owned_tokens(self) -> TokenStream {
        (*self).to_owned()
    }
}

impl IntoOwnedTokens for &TokenStream {
    fn into_owned_tokens(self) -> TokenStream {
        self.to_owned()
    }
}
