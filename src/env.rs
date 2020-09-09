use crate::{
    common::*,
    utils::{IntoRc, Shared, SharedCell},
    var::{PredicateTypeVar, TypeParamBoundVar, TypeVar, Var, WherePredicateVar},
};

pub use env::*;

mod env {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct Env {
        self_name: Rc<Ident>,
        variables: SharedCell<IndexSet<Shared<Variable>>>,
        type_predicates: HashMap<TypeVar, HashSet<TypeParamBoundVar>>,
        namespace: Vec<HashMap<Rc<Ident>, Shared<Variable>>>,
        trait_name_prefixes: SharedCell<Trie<String, usize>>,
    }

    impl Env {
        pub fn new(self_name: Ident) -> Self {
            Self {
                self_name: Rc::new(self_name),
                variables: SharedCell::new(IndexSet::new()),
                type_predicates: HashMap::new(),
                namespace: vec![HashMap::new()],
                trait_name_prefixes: SharedCell::new(Trie::new()),
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

        pub fn insert_free_quantifier(&mut self, ident: Ident) -> Shared<Variable> {
            // create a new variable
            let ident = Rc::new(ident);
            let var = Shared::new(Variable {
                is_mut: false,
                value: None,
            });

            // insert to var list
            self.variables.borrow_mut().insert(var.clone());

            // insert to namespace
            self.namespace
                .last_mut()
                .unwrap()
                .insert(ident.clone(), var.clone());

            var
        }

        pub fn insert_bounded_quantifier<T>(
            &mut self,
            ident: Ident,
            is_mut: bool,
            value: T,
        ) -> Shared<Variable>
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // create a new variable
            let ident = Rc::new(ident);
            let var = Shared::new(Variable {
                is_mut,
                value: Some(value.into_rc()),
            });

            // insert to var list
            self.variables.borrow_mut().insert(var.clone());

            // insert to namespace
            self.namespace
                .last_mut()
                .unwrap()
                .insert(ident.clone(), var.clone());

            var
        }

        pub fn insert_predicate(&mut self, predicate: WherePredicateVar) {
            match predicate {
                WherePredicateVar::Type(PredicateTypeVar { bounded_ty, bounds }) => {
                    self.type_predicates
                        .entry(bounded_ty)
                        .or_insert_with(|| HashSet::new())
                        .extend(bounds);
                }
            }
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

        pub fn mutable_quantifiers(&self) -> HashMap<Rc<Ident>, Shared<Variable>> {
            let (_shadowed, output) = self.namespace.iter().rev().fold(
                (HashSet::new(), HashMap::new()),
                |mut state, variables| {
                    let (shadowed, output) = &mut state;

                    variables.iter().for_each(|(ident, var)| {
                        if shadowed.insert(ident.to_owned()) {
                            if var.is_mut {
                                output.insert(ident.to_owned(), var.clone());
                            }
                        }
                    });

                    state
                },
            );
            output
        }

        pub fn free_quantifiers(&self) -> Vec<Shared<Variable>> {
            self.variables
                .borrow()
                .iter()
                .filter_map(|var| {
                    if var.is_free() {
                        Some(var.clone())
                    } else {
                        None
                    }
                })
                .collect()
        }

        pub fn predicates(&self) -> Vec<WherePredicateVar> {
            let type_predicates = self.type_predicates.iter().map(|(bounded_ty, bounds)| {
                let bounds: Vec<_> = bounds.iter().cloned().collect();
                WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty: bounded_ty.clone(),
                    bounds,
                })
            });
            type_predicates.collect()
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

        pub fn register_trait_name(&mut self, prefix: &str) -> Option<Ident> {
            let count = {
                let mut prefixes = self.trait_name_prefixes.borrow_mut();

                // check if the prefix is a proper prefix of existing prefixes
                if let Some(_) = prefixes.subtrie_mut(prefix) {
                    return None;
                }
                prefixes.map_with_default(prefix.to_string(), |count| *count += 1, 0);
                *prefixes.get(prefix).unwrap()
            };

            Some(format_ident!("{}{}", prefix, count))
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
