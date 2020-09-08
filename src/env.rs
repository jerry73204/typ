use crate::{
    common::*,
    utils::{IntoRc, Shared, SharedCell},
    var::{TypeVar, Var, WherePredicateVar},
};

pub use env::*;

mod env {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct Env {
        self_name: Rc<Ident>,
        predicates: IndexSet<Rc<WherePredicateVar>>,
        namespace: Vec<HashMap<Rc<Ident>, Shared<Variable>>>,
    }

    impl Env {
        pub fn new(self_name: Ident) -> Self {
            Self {
                self_name: Rc::new(self_name),
                predicates: IndexSet::new(),
                namespace: vec![HashMap::new()],
            }
        }

        pub fn get_variable(&self, ident: &Ident) -> Option<Shared<Variable>> {
            // locale the variable
            self.namespace
                .iter()
                .rev()
                .find_map(|variables| variables.get(ident).cloned())
        }

        pub fn branch(&self, self_name: Ident) -> Self {
            let mut env = self.clone();
            env.self_name = Rc::new(self_name);
            env
        }

        pub fn insert_free_quantifier(&mut self, ident: Ident) {
            // create a new variable
            let ident = Rc::new(ident);
            let var = Shared::new(Variable {
                is_mut: false,
                value: None,
            });

            // insert to namespace
            self.namespace
                .last_mut()
                .unwrap()
                .insert(ident.clone(), var.clone());
        }

        pub fn insert_bounded_quantifier<T>(&mut self, ident: Ident, is_mut: bool, value: T)
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // create a new variable
            let ident = Rc::new(ident);
            let var = Shared::new(Variable {
                is_mut,
                value: Some(value.into_rc()),
            });

            // insert to namespace
            self.namespace
                .last_mut()
                .unwrap()
                .insert(ident.clone(), var);
        }

        pub fn insert_predicate<T>(&mut self, predicate: T)
        where
            T: IntoRc<Inner = WherePredicateVar>,
        {
            self.predicates.insert(predicate.into_rc());
        }

        pub fn assign_quantifier<T>(&mut self, ident: &Ident, value: T) -> syn::Result<()>
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // locate the variable in namespace
            let opt =
                self.namespace
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(scope_index, variables)| {
                        variables
                            .get(ident)
                            .cloned()
                            .map(|var| (scope_index, var.is_mut))
                    });

            match opt {
                Some((scope_index, true)) => {
                    let ident = Rc::new(ident.to_owned());
                    let var = Shared::new(Variable {
                        is_mut: true,
                        value: Some(value.into_rc()),
                    });
                    self.namespace[scope_index].insert(ident.clone(), var);
                    Ok(())
                }
                Some((_, false)) => Err(Error::new(ident.span(), "the variable is not mutable")),
                None => Err(Error::new(ident.span(), "the variable is not defined")),
            }
        }

        pub fn sub_scope<F, T>(&mut self, f: F) -> T
        where
            F: FnOnce(&mut Env) -> T,
        {
            // append one scope on namesapce
            self.namespace.push(HashMap::new());

            // apply
            let ret = f(self);

            // pop subscope
            self.namespace.pop();

            ret
        }

        /// Compile trait bounds for each branch in form of [[ty, bounds]].
        pub fn predicates(&self) -> &IndexSet<Rc<WherePredicateVar>> {
            &self.predicates
        }
    }

    #[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
    pub struct Variable {
        pub is_mut: bool,
        // some -> bounded, none -> free
        pub value: Option<Rc<TypeVar>>,
    }

    impl Variable {
        pub fn is_free(&self) -> bool {
            matches!(self.value, None)
        }
    }
}
