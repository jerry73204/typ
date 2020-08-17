use crate::{
    common::*,
    path::{TraitBoundsVar, TypeVar},
};

#[derive(Debug)]
pub struct Scope {
    state: Rc<RefCell<ScopeState>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            state: Rc::new(RefCell::new(ScopeState {
                parent: None,
                var_counter: Rc::new(RefCell::new(0)),
                bounded_quantifiers: HashMap::new(),
                substituted_quantifiers: HashSet::new(),
                trait_bounds: Rc::new(RefCell::new(TraitBoundDict::new())),
                quantifiers: Rc::new(RefCell::new(QuantifierDict::new())),
            })),
        }
    }

    pub fn insert_initial_quantifier(&self, ident: Ident) {
        assert!(matches!(self.state.borrow().parent, None));
        let value = TypeVar::Path(vec![(ident.clone(), vec![])]);
        self.insert_quantifier(ident, value, false);
    }

    pub fn insert_quantifier(&self, ident: Ident, value: TypeVar, is_mut: bool) {
        assert!(self.is_path_bonuded(&value), "please report bug");

        let var_id = self.create_var_id();
        let ScopeState {
            bounded_quantifiers,
            quantifiers,
            substituted_quantifiers,
            ..
        } = &mut *self.state.borrow_mut();

        if let Some(prev_var_id) = bounded_quantifiers.insert(ident, var_id) {
            substituted_quantifiers.insert(prev_var_id);
        }

        quantifiers.borrow_mut().insert(var_id, value, is_mut);
    }

    pub fn insert_trait_bounds(&self, predicate: TypeVar, bounds: TraitBoundsVar) {
        self.state
            .borrow()
            .trait_bounds
            .borrow_mut()
            .insert(predicate, bounds);
    }

    pub fn assign_quantifier(&self, ident: Ident, value: TypeVar) -> bool {
        assert!(self.is_path_bonuded(&value), "please report bug");
        Self::recursive_assign_quantifier(self.state.clone(), ident, value)
    }

    pub fn sub_scope<F, T>(&self, scoped_f: F) -> T
    where
        F: FnOnce(&Self) -> T,
    {
        let mut sub_scope = {
            let ScopeState {
                var_counter,
                trait_bounds,
                quantifiers,
                ..
            } = &*self.state.borrow();

            Self {
                state: Rc::new(RefCell::new(ScopeState {
                    parent: Some(self.state.clone()),
                    bounded_quantifiers: HashMap::new(),
                    substituted_quantifiers: HashSet::new(),
                    var_counter: var_counter.clone(),
                    trait_bounds: trait_bounds.clone(),
                    quantifiers: quantifiers.clone(),
                })),
            }
        };

        scoped_f(&mut sub_scope)
    }

    pub fn get_var_id(&self, ident: &Ident) -> Option<usize> {
        Self::recursive_get_var_id(self.state.clone(), ident)
    }

    fn is_path_bonuded(&self, path: &TypeVar) -> bool {
        Self::recursive_check_is_path_bounded(self.state.borrow().quantifiers.clone(), path)
    }

    fn create_var_id(&self) -> usize {
        self.state.borrow().create_var_id()
    }

    fn recursive_get_var_id(state: Rc<RefCell<ScopeState>>, ident: &Ident) -> Option<usize> {
        let ScopeState {
            parent,
            bounded_quantifiers,
            ..
        } = &*state.borrow();

        if let Some(var_id) = bounded_quantifiers.get(ident) {
            return Some(*var_id);
        }

        match parent {
            Some(parent) => Self::recursive_get_var_id(parent.clone(), ident),
            None => None,
        }
    }

    fn recursive_check_is_path_bounded(
        quantifiers: Rc<RefCell<QuantifierDict>>,
        path: &TypeVar,
    ) -> bool {
        match path {
            TypeVar::Var(var_id) => quantifiers.borrow().contains(var_id),
            TypeVar::Path(segments) => segments.iter().all(|(_ident, generic_args)| {
                generic_args
                    .iter()
                    .all(|arg| Self::recursive_check_is_path_bounded(quantifiers.clone(), arg))
            }),
        }
    }

    fn recursive_assign_quantifier(
        state: Rc<RefCell<ScopeState>>,
        ident: Ident,
        value: TypeVar,
    ) -> bool {
        match state.borrow().bounded_quantifiers.get(&ident).copied() {
            Some(prev_var_id) => {
                let quantifiers = state.borrow().quantifiers.clone();
                let (is_mut, _) = quantifiers.borrow().get(&prev_var_id);

                if is_mut {
                    let new_var_id = state.borrow().create_var_id();
                    quantifiers.borrow_mut().insert(new_var_id, value, true);
                    state
                        .borrow_mut()
                        .bounded_quantifiers
                        .insert(ident, new_var_id);
                    true
                } else {
                    false
                }
            }
            None => match &state.borrow().deref().parent {
                Some(parent) => Self::recursive_assign_quantifier(parent.clone(), ident, value),
                None => false,
            },
        }
    }
}

#[derive(Clone, Debug)]
struct ScopeState {
    parent: Option<Rc<RefCell<ScopeState>>>,
    bounded_quantifiers: HashMap<Ident, usize>,
    substituted_quantifiers: HashSet<usize>,
    var_counter: Rc<RefCell<usize>>,
    trait_bounds: Rc<RefCell<TraitBoundDict>>,
    quantifiers: Rc<RefCell<QuantifierDict>>,
}

impl ScopeState {
    fn create_var_id(&self) -> usize {
        let var_counter = &mut *self.var_counter.borrow_mut();
        let ret = *var_counter;
        *var_counter += 1;
        ret
    }
}

#[derive(Clone, Debug)]
pub struct QuantifierDict {
    // var_id -> (is_mut, value)
    map: HashMap<usize, (bool, TypeVar)>,
}

impl QuantifierDict {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn contains<K>(&self, var_id: K) -> bool
    where
        K: std::borrow::Borrow<usize>,
    {
        self.map.contains_key(var_id.borrow())
    }

    pub fn insert(&mut self, var_id: usize, value: TypeVar, is_mut: bool) {
        let prev = self.map.insert(var_id, (is_mut, value));
        assert!(matches!(prev, Some(_)), "please report bug");
    }

    pub fn get<K>(&self, var_id: K) -> (bool, &TypeVar)
    where
        K: std::borrow::Borrow<usize>,
    {
        let (is_mut, path) = self.map.get(var_id.borrow()).expect("please report bug");
        (*is_mut, path)
    }
}

#[derive(Clone, Debug)]
pub struct TraitBoundDict {
    bounds: HashMap<TypeVar, TraitBoundsVar>,
}

impl TraitBoundDict {
    pub fn new() -> Self {
        Self {
            bounds: HashMap::new(),
        }
    }

    pub fn insert(&mut self, predicate: TypeVar, bounds: TraitBoundsVar) {
        use std::collections::hash_map::Entry;

        match self.bounds.entry(predicate) {
            Entry::Occupied(mut entry) => *entry.get_mut() += bounds,
            Entry::Vacant(entry) => {
                entry.insert(bounds);
            }
        }
    }
}

impl FromIterator<(TypeVar, TraitBoundsVar)> for TraitBoundDict {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (TypeVar, TraitBoundsVar)>,
    {
        let bounds: HashMap<_, _> = iter
            .into_iter()
            .into_group_map()
            .into_iter()
            .map(|(type_, bounds_vec)| {
                let bounds: TraitBoundsVar = bounds_vec.into_iter().sum();
                (type_, bounds)
            })
            .collect();
        Self { bounds }
    }
}

impl Extend<(TypeVar, TraitBoundsVar)> for TraitBoundDict {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (TypeVar, TraitBoundsVar)>,
    {
        iter.into_iter().for_each(|(predicate, bound)| {
            self.insert(predicate, bound);
        });
    }
}
