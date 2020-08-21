use crate::{
    common::*,
    path::{TraitBoundsVar, TypeVar},
};
pub use quantifier_dict::*;
pub use scope::*;
pub use trait_bounds_dict::*;
pub use var_builder::*;

mod scope {
    use super::*;

    pub trait Scope
    where
        Self: Sized,
    {
        fn take_state(&mut self) -> ScopeState;

        fn set_state(&mut self, state: ScopeState);

        fn state(&self) -> &ScopeState;

        fn state_mut(&mut self) -> &mut ScopeState;

        fn bounded_quantifiers(&self) -> &HashMap<Ident, usize>;

        fn bounded_quantifiers_mut(&mut self) -> &mut HashMap<Ident, usize>;

        fn get_var_id(&self, ident: &Ident) -> Option<usize>;

        fn assign_quantifier_on_parent(&mut self, ident: Ident, value: Rc<TypeVar>) -> bool;

        fn insert_initial_quantifier(&mut self, ident: Ident) {
            let value = self.type_var_builder().from_exact_ident(&ident);
            self.insert_quantifier(ident, value, false);
        }

        fn insert_quantifier(&mut self, ident: Ident, value: Rc<TypeVar>, is_mut: bool) {
            assert!(self.state().type_vars.contains(&value), "please report bug");

            // obtain a new variable id
            let var_id = self.create_var_id();
            self.bounded_quantifiers_mut().insert(ident, var_id);
            self.state_mut().quantifiers.insert(var_id, value, is_mut);
        }

        fn insert_trait_bounds(&mut self, predicate: Rc<TypeVar>, bounds: Rc<TraitBoundsVar>) {
            self.state_mut().trait_bounds.insert(predicate, bounds);
        }

        fn assign_quantifier(&mut self, ident: Ident, value: Rc<TypeVar>) -> bool {
            // check if the identifier is defined in this scope
            match self.bounded_quantifiers().get(&ident).copied() {
                Some(prev_var_id) => {
                    let (is_mut, _) = self.state().quantifiers.get(&prev_var_id);

                    // check if the quantifier is mutable
                    if is_mut {
                        let new_var_id = self.create_var_id();
                        self.state_mut().quantifiers.insert(new_var_id, value, true);
                        self.bounded_quantifiers_mut().insert(ident, new_var_id);
                        true
                    } else {
                        false
                    }
                }
                None => self.assign_quantifier_on_parent(ident, value),
            }
        }

        fn create_var_id(&mut self) -> usize {
            let var_counter = &mut self.state_mut().var_counter;
            let ret = *var_counter;
            *var_counter += 1;
            ret
        }

        fn type_var_builder<'a>(&'a mut self) -> TypeVarBuilder<'a, Self> {
            TypeVarBuilder::new(self)
        }

        fn trait_bounds_var_builder<'a>(&'a mut self) -> TraitVarBuilder<'a, Self> {
            TraitVarBuilder::new(self)
        }

        fn add_type_var(&mut self, var: TypeVar) -> Rc<TypeVar> {
            let var = Rc::new(var);
            self.state_mut().type_vars.insert(var.clone());
            var
        }

        fn add_trait_bounds_var(&mut self, var: TraitBoundsVar) -> Rc<TraitBoundsVar> {
            let var = Rc::new(var);
            self.state_mut().trait_bounds_vars.insert(var.clone());
            var
        }

        fn sub_scope<'a, F, T>(&'a mut self, f: F) -> T
        where
            F: FnOnce(&mut SubScope<'a, Self>) -> T,
        {
            let (me, ret, state) = {
                let state = self.take_state();
                let mut sub_scope = SubScope {
                    parent: self,
                    bounded_quantifiers: HashMap::new(),
                    state: Some(state),
                };

                let ret = f(&mut sub_scope);
                let me = sub_scope.parent;
                let state = sub_scope.state.expect("please report bug");
                (me, ret, state)
            };
            me.set_state(state);
            ret
        }
    }

    #[derive(Debug)]
    pub struct RootScope {
        bounded_quantifiers: HashMap<Ident, usize>,
        state: Option<ScopeState>,
    }

    impl RootScope {
        pub fn new() -> Self {
            Self {
                bounded_quantifiers: HashMap::new(),
                state: Some(ScopeState::new()),
            }
        }
    }

    impl Scope for RootScope {
        fn take_state(&mut self) -> ScopeState {
            self.state.take().expect("please report bug")
        }

        fn set_state(&mut self, state: ScopeState) {
            self.state = Some(state)
        }

        fn state(&self) -> &ScopeState {
            self.state.as_ref().unwrap()
        }

        fn state_mut(&mut self) -> &mut ScopeState {
            self.state.as_mut().unwrap()
        }

        fn bounded_quantifiers(&self) -> &HashMap<Ident, usize> {
            &self.bounded_quantifiers
        }

        fn bounded_quantifiers_mut(&mut self) -> &mut HashMap<Ident, usize> {
            &mut self.bounded_quantifiers
        }

        fn assign_quantifier_on_parent(&mut self, _ident: Ident, _value: Rc<TypeVar>) -> bool {
            false
        }

        fn get_var_id(&self, ident: &Ident) -> Option<usize> {
            self.bounded_quantifiers().get(ident).copied()
        }
    }

    #[derive(Debug)]
    pub struct SubScope<'a, S>
    where
        S: Scope,
    {
        parent: &'a mut S,
        bounded_quantifiers: HashMap<Ident, usize>,
        state: Option<ScopeState>,
    }

    impl<'a, S> Scope for SubScope<'a, S>
    where
        S: Scope,
    {
        fn take_state(&mut self) -> ScopeState {
            self.state.take().expect("please report bug")
        }

        fn set_state(&mut self, state: ScopeState) {
            self.state = Some(state)
        }

        fn state(&self) -> &ScopeState {
            self.state.as_ref().unwrap()
        }

        fn state_mut(&mut self) -> &mut ScopeState {
            self.state.as_mut().unwrap()
        }

        fn bounded_quantifiers(&self) -> &HashMap<Ident, usize> {
            &self.bounded_quantifiers
        }

        fn bounded_quantifiers_mut(&mut self) -> &mut HashMap<Ident, usize> {
            &mut self.bounded_quantifiers
        }

        fn assign_quantifier_on_parent(&mut self, ident: Ident, value: Rc<TypeVar>) -> bool {
            self.parent.assign_quantifier(ident, value)
        }

        fn get_var_id(&self, ident: &Ident) -> Option<usize> {
            self.bounded_quantifiers()
                .get(ident)
                .copied()
                .or_else(|| self.parent.get_var_id(ident))
        }
    }

    #[derive(Debug)]
    pub struct ScopeState {
        var_counter: usize,
        trait_bounds: TraitBoundDict,
        quantifiers: QuantifierDict,
        type_vars: HashSet<Rc<TypeVar>>,
        trait_bounds_vars: HashSet<Rc<TraitBoundsVar>>,
    }

    impl ScopeState {
        pub fn new() -> Self {
            Self {
                var_counter: 0,
                trait_bounds: TraitBoundDict::new(),
                quantifiers: QuantifierDict::new(),
                type_vars: HashSet::new(),
                trait_bounds_vars: HashSet::new(),
            }
        }
    }

}

mod var_builder {
    use super::*;

    #[derive(Debug)]
    pub struct TypeVarBuilder<'a, S>
    where
        S: Scope,
    {
        scope: &'a mut S,
    }

    impl<'a, S> TypeVarBuilder<'a, S>
    where
        S: Scope,
    {
        pub fn new(scope: &'a mut S) -> Self {
            Self { scope }
        }

        pub fn from_exact_ident(&mut self, ident: &Ident) -> Rc<TypeVar>
        where
            S: Scope,
        {
            let var = TypeVar::Path(vec![(ident.to_owned(), vec![])]);
            self.scope.add_type_var(var)
        }

        pub fn from_quantifier(&mut self, ident: &Ident) -> Option<Rc<TypeVar>>
        where
            S: Scope,
        {
            let var_id = self.scope.get_var_id(ident)?;
            let var = TypeVar::Var(var_id);
            Some(self.scope.add_type_var(var))
        }

        pub fn from_scoped_path(&mut self, path: &Path) -> syn::Result<Rc<TypeVar>>
        where
            S: Scope,
        {
            let var = type_var_fn::from_scoped_path(self.scope, path)?;
            Ok(self.scope.add_type_var(var))
        }

        pub fn from_scoped_pat(&mut self, pat: &Pat) -> syn::Result<Rc<TypeVar>>
        where
            S: Scope,
        {
            let var = type_var_fn::from_scoped_pat(self.scope, pat)?;
            Ok(self.scope.add_type_var(var))
        }
    }

    #[derive(Debug)]
    pub struct TraitVarBuilder<'a, S>
    where
        S: Scope,
    {
        scope: &'a mut S,
    }

    impl<'a, S> TraitVarBuilder<'a, S>
    where
        S: Scope,
    {
        pub fn new(scope: &'a mut S) -> Self {
            Self { scope }
        }

        pub fn empty(&mut self) -> Rc<TraitBoundsVar> {
            let var = TraitBoundsVar::empty();
            self.scope.add_trait_bounds_var(var)
        }

        pub fn from_scoped_path(&mut self, path: &Path) -> syn::Result<Rc<TraitBoundsVar>> {
            let var = trait_bounds_var_fn::from_scoped_path(self.scope, path)?;
            Ok(self.scope.add_trait_bounds_var(var))
        }

        pub fn from_scoped_paths(&mut self, paths: &[&Path]) -> syn::Result<Rc<TraitBoundsVar>> {
            let var = trait_bounds_var_fn::from_scoped_paths(self.scope, paths)?;
            Ok(self.scope.add_trait_bounds_var(var))
        }

        pub fn from_scoped_type(&mut self, ty: &Type) -> syn::Result<Rc<TraitBoundsVar>> {
            let var = trait_bounds_var_fn::from_scoped_type(self.scope, ty)?;
            Ok(self.scope.add_trait_bounds_var(var))
        }

        pub fn from_scoped_type_param_bounds(
            &mut self,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<Rc<TraitBoundsVar>> {
            let var = trait_bounds_var_fn::from_scoped_type_param_bounds(self.scope, bounds)?;
            Ok(self.scope.add_trait_bounds_var(var))
        }
    }

    mod type_var_fn {
        use super::*;

        pub fn from_scoped_path<S>(scope: &S, path: &Path) -> syn::Result<TypeVar>
        where
            S: Scope,
        {
            // check if the path coincides with quantifiers
            if let Some(ident) = path.get_ident() {
                if let Some(var_id) = scope.get_var_id(ident) {
                    let var = TypeVar::Var(var_id);
                    return Ok(var);
                }
            }

            let segments: Vec<_> = path
                .segments
                .iter()
                .map(|segment| {
                    let ident = segment.ident.to_owned();
                    let generic_args = match &segment.arguments {
                        PathArguments::None => vec![],
                        PathArguments::Parenthesized(_) => {
                            return Err(Error::new(
                                segment.arguments.span(),
                                "parenthesized arguments are not allowed",
                            ));
                        }
                        PathArguments::AngleBracketed(args) => {
                            let generic_args: Vec<_> = args
                                .args
                                .iter()
                                .map(|arg| -> syn::Result<_> {
                                    match arg {
                                        GenericArgument::Type(Type::Path(ty_path)) => {
                                            from_scoped_path(scope, &ty_path.path)
                                        }
                                        _ => Err(Error::new(
                                            arg.span(),
                                            "non-type argument is not allowed",
                                        )),
                                    }
                                })
                                .try_collect()?;
                            generic_args
                        }
                    };

                    Ok((ident, generic_args))
                })
                .try_collect()?;

            let var = TypeVar::Path(segments);
            Ok(var)
        }

        pub fn from_scoped_pat<S>(scope: &S, pat: &Pat) -> syn::Result<TypeVar>
        where
            S: Scope,
        {
            match pat {
                Pat::Ident(pat_ident) => {
                    let ident = &pat_ident.ident;
                    let var = match scope.get_var_id(ident) {
                        Some(var_id) => TypeVar::Var(var_id),
                        None => TypeVar::Path(vec![(ident.to_owned(), vec![])]),
                    };
                    Ok(var)
                }
                Pat::Path(pat_path) => from_scoped_path(scope, &pat_path.path),
                _ => Err(Error::new(pat.span(), "not an type")),
            }
        }
    }

    mod trait_bounds_var_fn {
        use super::*;

        pub fn from_scoped_path<S>(scope: &S, path: &Path) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
        {
            let segments: Vec<_> = path
                .segments
                .iter()
                .map(|segment| {
                    let ident = segment.ident.to_owned();
                    let generic_args = match &segment.arguments {
                        PathArguments::None => vec![],
                        PathArguments::Parenthesized(_) => {
                            return Err(Error::new(
                                segment.arguments.span(),
                                "parenthesized arguments are not allowed",
                            ));
                        }
                        PathArguments::AngleBracketed(args) => {
                            let generic_args: Vec<_> = args
                                .args
                                .iter()
                                .map(|arg| -> syn::Result<_> {
                                    match arg {
                                        GenericArgument::Type(Type::Path(ty_path)) => {
                                            type_var_fn::from_scoped_path(scope, &ty_path.path)
                                        }
                                        _ => Err(Error::new(
                                            arg.span(),
                                            "non-type argument is not allowed",
                                        )),
                                    }
                                })
                                .try_collect()?;
                            generic_args
                        }
                    };

                    Ok((ident, generic_args))
                })
                .try_collect()?;

            let paths: BTreeSet<_> = vec![segments].into_iter().collect();
            let var = TraitBoundsVar { paths };
            Ok(var)
        }

        pub fn from_scoped_paths<S>(scope: &S, paths: &[&Path]) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
        {
            let paths: BTreeSet<_> = paths
                .iter()
                .copied()
                .map(|path| -> syn::Result<_> {
                    let segments: Vec<_> = path
                        .segments
                        .iter()
                        .map(|segment| {
                            let ident = segment.ident.to_owned();
                            let generic_args = match &segment.arguments {
                                PathArguments::None => vec![],
                                PathArguments::Parenthesized(_) => {
                                    return Err(Error::new(
                                        segment.arguments.span(),
                                        "parenthesized arguments are not allowed",
                                    ));
                                }
                                PathArguments::AngleBracketed(args) => {
                                    let generic_args: Vec<_> = args
                                        .args
                                        .iter()
                                        .map(|arg| -> syn::Result<_> {
                                            match arg {
                                                GenericArgument::Type(Type::Path(ty_path)) => {
                                                    type_var_fn::from_scoped_path(
                                                        scope,
                                                        &ty_path.path,
                                                    )
                                                }
                                                _ => Err(Error::new(
                                                    arg.span(),
                                                    "non-type argument is not allowed",
                                                )),
                                            }
                                        })
                                        .try_collect()?;
                                    generic_args
                                }
                            };

                            Ok((ident, generic_args))
                        })
                        .try_collect()?;

                    Ok(segments)
                })
                .try_collect()?;

            let var = TraitBoundsVar { paths };
            Ok(var)
        }

        pub fn from_scoped_type<S>(scope: &S, ty: &Type) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
        {
            let paths = match ty {
                Type::Infer(_) => vec![],
                Type::Path(path) => vec![&path.path],
                Type::TraitObject(tobj) => {
                    let paths: Vec<_> = tobj
                        .bounds
                        .iter()
                        .map(|param_bound| match param_bound {
                            TypeParamBound::Trait(bound) => Ok(&bound.path),
                            TypeParamBound::Lifetime(lifetime) => {
                                Err(Error::new(lifetime.span(), "lifetime is not allowed"))
                            }
                        })
                        .try_collect()?;
                    paths
                }
                _ => {
                    return Err(Error::new(ty.span(), "not a trait bound"));
                }
            };

            from_scoped_paths(scope, &paths)
        }

        pub fn from_scoped_type_param_bounds<S>(
            scope: &S,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
        {
            let paths: Vec<_> = bounds
                .iter()
                .map(|bound| match bound {
                    TypeParamBound::Trait(trait_bound) => Ok(&trait_bound.path),
                    TypeParamBound::Lifetime(lifetime) => {
                        Err(Error::new(lifetime.span(), "lifetime is not allowed"))
                    }
                })
                .try_collect()?;
            from_scoped_paths(scope, &paths)
        }
    }
}

mod quantifier_dict {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct QuantifierDict {
        // var_id -> (is_mut, value)
        map: HashMap<usize, QuantifierState>,
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

        pub fn insert(&mut self, var_id: usize, value: Rc<TypeVar>, is_mut: bool) {
            let prev = self.map.insert(var_id, QuantifierState { is_mut, value });
            assert!(matches!(prev, Some(_)), "please report bug");
        }

        pub fn get<K>(&self, var_id: K) -> (bool, &TypeVar)
        where
            K: std::borrow::Borrow<usize>,
        {
            let QuantifierState { is_mut, value } =
                self.map.get(var_id.borrow()).expect("please report bug");
            (*is_mut, value)
        }
    }

    impl IntoIterator for QuantifierDict {
        type Item = (usize, Rc<TypeVar>);
        type IntoIter = Box<dyn Iterator<Item = (usize, Rc<TypeVar>)>>;

        fn into_iter(self) -> Self::IntoIter {
            let iter = self
                .map
                .into_iter()
                .map(|(var_id, QuantifierState { value, .. })| (var_id, value));
            Box::new(iter)
        }
    }

    impl Default for QuantifierDict {
        fn default() -> Self {
            Self {
                map: HashMap::default(),
            }
        }
    }

    #[derive(Clone, Debug)]
    struct QuantifierState {
        pub is_mut: bool,
        pub value: Rc<TypeVar>,
    }
}

mod trait_bounds_dict {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct TraitBoundDict {
        bounds: HashMap<Rc<TypeVar>, Rc<TraitBoundsVar>>,
    }

    impl TraitBoundDict {
        pub fn new() -> Self {
            Self {
                bounds: HashMap::new(),
            }
        }

        pub fn insert(&mut self, predicate: Rc<TypeVar>, bounds: Rc<TraitBoundsVar>) {
            use std::collections::hash_map::Entry;

            match self.bounds.entry(predicate) {
                Entry::Occupied(mut entry) => {
                    *entry.get_mut() = Rc::new(&**entry.get() + &*bounds);
                }
                Entry::Vacant(entry) => {
                    entry.insert(bounds);
                }
            }
        }
    }

    impl FromIterator<(Rc<TypeVar>, Rc<TraitBoundsVar>)> for TraitBoundDict {
        fn from_iter<T>(iter: T) -> Self
        where
            T: IntoIterator<Item = (Rc<TypeVar>, Rc<TraitBoundsVar>)>,
        {
            let bounds: HashMap<_, _> = iter
                .into_iter()
                .into_group_map()
                .into_iter()
                .map(|(type_, bounds_vec)| {
                    let bounds: TraitBoundsVar = bounds_vec
                        .into_iter()
                        .map(|bounds| bounds.as_ref().to_owned())
                        .sum();
                    (type_, Rc::new(bounds))
                })
                .collect();
            Self { bounds }
        }
    }

    impl Extend<(Rc<TypeVar>, Rc<TraitBoundsVar>)> for TraitBoundDict {
        fn extend<T>(&mut self, iter: T)
        where
            T: IntoIterator<Item = (Rc<TypeVar>, Rc<TraitBoundsVar>)>,
        {
            iter.into_iter().for_each(|(predicate, bound)| {
                self.insert(predicate, bound);
            });
        }
    }

    impl IntoIterator for TraitBoundDict {
        type Item = (Rc<TypeVar>, Rc<TraitBoundsVar>);
        type IntoIter = std::collections::hash_map::IntoIter<Rc<TypeVar>, Rc<TraitBoundsVar>>;

        fn into_iter(self) -> Self::IntoIter {
            self.bounds.into_iter()
        }
    }
}
