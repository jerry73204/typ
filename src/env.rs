use crate::{
    common::*,
    utils::{IntoRc, Shared, SharedCell},
    var::{TypeVar, WherePredicateVar},
};

pub use env::*;

mod env {
    use super::*;

    #[derive(Debug)]
    pub struct Brancher<'a> {
        // cond id vs. tokens
        condition_ids: Vec<usize>,
        orig: &'a mut Env,
        // target vs. scope set
        branches: Vec<Env>,
    }

    impl<'a> Brancher<'a> {
        pub fn branch<T, F, R>(&mut self, target: T, f: F) -> R
        where
            T: IntoRc<Inner = TypeVar>,
            F: FnOnce(&mut Env) -> R,
        {
            // deep clone local states
            let locals: Vec<_> = self
                .orig
                .locals
                .iter()
                .map(|local| local.deep_clone())
                .collect();

            // save conditions on every branch
            let target = target.into_rc();
            locals
                .iter()
                .cloned()
                .zip_eq(self.condition_ids.iter().cloned())
                .for_each(|(local, cond_id)| {
                    local
                        .borrow_mut()
                        .condition_targets
                        .insert(cond_id, target.clone());
                });

            // build branched scope set
            let mut branched = Env {
                global: self.orig.global.clone(),
                locals,
            };
            let ret = f(&mut branched);
            self.branches.push(branched);
            ret
        }

        pub fn merge(self) -> &'a mut Env {
            // pop one subscope
            self.orig.locals = self
                .branches
                .into_iter()
                .flat_map(|branched| {
                    let locals = branched.locals;
                    locals.iter().cloned().for_each(|local| {
                        local.borrow_mut().namespace.pop();
                    });
                    locals
                })
                .collect();
            self.orig
        }
    }

    #[derive(Debug)]
    pub struct LocalEnv {
        global: SharedCell<GlobalState>,
        local: SharedCell<LocalState>,
    }

    #[derive(Debug)]
    pub struct Env {
        global: SharedCell<GlobalState>,
        locals: Vec<SharedCell<LocalState>>,
    }

    impl Env {
        fn next_var_id(&self) -> usize {
            self.global.borrow().variables.len()
        }

        fn branches(&self) -> Vec<LocalEnv> {
            self.locals
                .iter()
                .cloned()
                .map(|local| LocalEnv {
                    global: self.global.clone(),
                    local,
                })
                .collect()
        }

        pub fn new() -> Self {
            Self {
                global: SharedCell::new(GlobalState::new()),
                locals: vec![SharedCell::new(LocalState::new())],
            }
        }

        pub fn num_branches(&self) -> usize {
            self.locals.len()
        }

        pub fn num_conditions(&self) -> usize {
            self.global.borrow().conditions.len()
        }

        pub fn insert_condition<T>(&self, cond: T) -> usize
        where
            T: IntoRc<Inner = TypeVar>,
        {
            let mut global = self.global.borrow_mut();
            let id = global.conditions.len();
            global.conditions.push(cond.into_rc());
            id
        }

        pub fn get_variable(&self, ident: &Ident) -> Option<Vec<Shared<Variable>>> {
            // locale the variable
            self.locals[0]
                .borrow()
                .namespace
                .iter()
                .enumerate()
                .rev()
                .find_map(|(scope_index, variables)| {
                    variables.get(ident).cloned().map(|var| scope_index)
                })
                .map(|scope_index| {
                    self.locals
                        .iter()
                        .map(|local| local.borrow().namespace[scope_index][ident].clone())
                        .collect()
                })
        }

        pub fn insert_free_quantifier(&mut self, ident: Ident) -> syn::Result<()> {
            // create a new variable
            let ident = Rc::new(ident);
            let var = Shared::new(Variable {
                is_mut: false,
                value: None,
            });

            // insert to namespace
            self.locals.iter().for_each(|local| {
                local
                    .borrow_mut()
                    .namespace
                    .last_mut()
                    .unwrap()
                    .insert(ident.clone(), var.clone());
            });
            Ok(())
        }

        pub fn insert_bounded_quantifier<T>(&mut self, ident: Ident, is_mut: bool, values: Vec<T>)
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // sanity check
            assert_eq!(
                values.len(),
                self.locals.len(),
                "please report bug: the number of values does not match"
            );

            // create a new variable
            let ident = Rc::new(ident);

            // insert to namespace
            let mut global = self.global.borrow_mut();
            self.locals
                .iter_mut()
                .zip_eq(values)
                .for_each(|(local, value)| {
                    let var = {
                        let var = Shared::new(Variable {
                            is_mut,
                            value: Some(value.into_rc()),
                        });
                        global.variables.insert(var.clone());
                        var
                    };

                    local
                        .to_owned()
                        .borrow_mut()
                        .namespace
                        .last_mut()
                        .unwrap()
                        .insert(ident.clone(), var);
                });
        }

        pub fn insert_predicate<T>(&mut self, predicates: Vec<T>)
        where
            T: IntoRc<Inner = WherePredicateVar>,
        {
            // sanity check
            assert_eq!(
                predicates.len(),
                self.locals.len(),
                "please report bug: the number of values does not match"
            );

            self.locals
                .iter_mut()
                .zip_eq(predicates)
                .for_each(|(local, prediate)| {
                    local
                        .to_owned()
                        .borrow_mut()
                        .predicates
                        .insert(prediate.into_rc());
                });
        }

        pub fn assign_quantifier<T>(&mut self, ident: &Ident, values: Vec<T>) -> syn::Result<()>
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // sanity check
            assert_eq!(
                values.len(),
                self.locals.len(),
                "please report bug: the number of values does not match"
            );

            // locate the variable in namespace
            let opt = self.locals[0]
                .borrow()
                .namespace
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

                    // insert to namespace
                    self.locals
                        .iter()
                        .cloned()
                        .zip_eq(values)
                        .for_each(|(local, value)| {
                            let var = Shared::new(Variable {
                                is_mut: true,
                                value: Some(value.into_rc()),
                            });
                            local.borrow_mut().namespace[scope_index].insert(ident.clone(), var);
                        });

                    Ok(())
                }
                Some((_, false)) => Err(Error::new(ident.span(), "the variable is not mutable")),
                None => Err(Error::new(ident.span(), "the variable is not defined")),
            }
        }

        pub fn push<T>(&mut self, values: Vec<T>) -> usize
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // sanity check
            assert_eq!(
                values.len(),
                self.locals.len(),
                "please report bug: the number of values does not match"
            );

            // increase counter
            let mut global = self.global.borrow_mut();
            let register_id = global.register_counter;
            global.register_counter += 1;

            // save values
            self.locals
                .iter()
                .cloned()
                .zip_eq(values)
                .for_each(|(local, value)| {
                    local
                        .borrow_mut()
                        .registers
                        .insert(register_id, value.into_rc());
                });

            register_id
        }

        pub fn pop(&mut self, register_id: usize) -> Vec<Rc<TypeVar>> {
            self.locals
                .iter()
                .cloned()
                .map(|local| {
                    local
                        .borrow_mut()
                        .registers
                        .remove(&register_id)
                        .expect("please report bug: the registe id is not valid")
                })
                .collect()
        }

        pub fn sub_scope<F, T>(&mut self, f: F) -> T
        where
            F: FnOnce(&mut Env) -> T,
        {
            // append one scope on namesapce
            self.locals.iter().cloned().for_each(|local| {
                local.borrow_mut().namespace.push(HashMap::new());
            });

            // apply
            let ret = f(self);

            // pop subscope
            self.locals.iter().cloned().for_each(|local| {
                local.borrow_mut().namespace.pop();
            });

            ret
        }

        /// Compile trait bounds for each branch in form of [[ty, bounds]].
        pub fn predicates(&self) -> Vec<IndexSet<Rc<WherePredicateVar>>> {
            self.locals
                .iter()
                .cloned()
                .map(|local| local.borrow().predicates.clone())
                .collect()
        }

        /// Compile conditions for each branch in form of ([condition], [[target?]]).
        pub fn conditions(&self) -> (Vec<Rc<TypeVar>>, Vec<Vec<Option<Rc<TypeVar>>>>) {
            let global = self.global.borrow();
            let num_conditions = global.conditions.len();
            let conditions: Vec<_> = global.conditions.iter().cloned().collect();
            let targets_per_branch = self
                .locals
                .iter()
                .cloned()
                .map(|local| {
                    let local = local.borrow();
                    let targets: Vec<_> = (0..num_conditions)
                        .map(|cond_id| local.condition_targets.get(&cond_id).map(Clone::clone))
                        .collect();
                    targets
                })
                .collect();
            (conditions, targets_per_branch)
        }

        pub fn into_brancher<T>(&mut self, conditions: Vec<T>) -> Brancher<'_>
        where
            T: IntoRc<Inner = TypeVar>,
        {
            // sanity check
            assert_eq!(
                conditions.len(),
                self.locals.len(),
                "please report bug: the number of conditions does not match"
            );

            // append one subscope
            self.locals.iter().cloned().for_each(|local| {
                local.borrow_mut().namespace.push(HashMap::new());
            });

            // save conditions on global state
            let condition_ids: Vec<_> = conditions
                .into_iter()
                .map(|cond| self.insert_condition(cond.into_rc()))
                .collect();

            Brancher {
                condition_ids,
                orig: self,
                branches: vec![],
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct LocalState {
        predicates: IndexSet<Rc<WherePredicateVar>>,
        namespace: Vec<HashMap<Rc<Ident>, Shared<Variable>>>,
        registers: HashMap<usize, Rc<TypeVar>>,
        // cond id -> (condition, target)
        condition_targets: HashMap<usize, Rc<TypeVar>>,
    }

    impl LocalState {
        pub fn new() -> Self {
            Self {
                predicates: IndexSet::new(),
                namespace: vec![HashMap::new()],
                registers: HashMap::new(),
                condition_targets: HashMap::new(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct GlobalState {
        variables: IndexSet<Shared<Variable>>,
        register_counter: usize,
        conditions: Vec<Rc<TypeVar>>,
    }

    impl GlobalState {
        pub fn new() -> Self {
            Self {
                variables: IndexSet::new(),
                register_counter: 0,
                conditions: vec![],
            }
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
