use crate::{
    common::*,
    var::{Scoped, SegmentVar, TraitBoundsVar, TraitVar, TypeVar},
};

pub use quantifier_dict::*;
pub use scope::*;
pub use trait_bounds_dict::*;
pub use var_builder::*;

mod scope {
    use super::*;

    pub trait Scope
    where
        Self: Sized + Debug,
    {
        fn state(&self) -> Ref<ScopeState>;

        fn state_mut(&mut self) -> RefMut<ScopeState>;

        fn clone_state(&self) -> SharedScopeState;

        fn bounded_quantifiers(&self) -> &HashMap<Ident, usize>;

        fn bounded_quantifiers_mut(&mut self) -> &mut HashMap<Ident, usize>;

        fn get_var_id(&self, ident: &Ident) -> Option<usize>;

        fn assign_quantifier_on_parent(&mut self, ident: &Ident, value: Scoped<TypeVar>) -> bool;

        fn recursive_local_quantifiers(
            &self,
            shadowed: &mut HashSet<Ident>,
            found: &mut HashMap<Ident, Quantifier>,
        );

        fn branch(&self) -> RootScope;

        fn insert_initial_quantifier(&mut self, ident: Ident) {
            let value = self.type_var_builder().from_ident(&ident);
            self.insert_quantifier(ident, value, false);
        }

        fn insert_quantifier(&mut self, ident: Ident, value: Scoped<TypeVar>, is_mut: bool) {
            let state = self.clone_state();
            assert_eq!(state, value.scope);

            // obtain a new variable id
            let var_id = self.create_var_id();
            self.bounded_quantifiers_mut().insert(ident, var_id);
            self.state_mut()
                .quantifiers
                .insert(var_id, value.var, is_mut);
        }

        fn insert_trait_bounds(
            &mut self,
            predicate: Scoped<TypeVar>,
            bounds: Scoped<TraitBoundsVar>,
        ) {
            let state = self.clone_state();
            assert_eq!(state, predicate.scope);
            assert_eq!(state, bounds.scope);
            self.state_mut()
                .trait_bounds
                .insert(predicate.var, bounds.var);
        }

        fn assign_quantifier(&mut self, ident: &Ident, value: Scoped<TypeVar>) -> bool {
            assert_eq!(self.clone_state(), value.scope);

            // check if the identifier is defined in this scope
            match self.bounded_quantifiers().get(ident).copied() {
                Some(prev_var_id) => {
                    let Quantifier { is_mut, .. } = *self
                        .state()
                        .quantifiers
                        .get(&prev_var_id)
                        .expect("please report bug: missing quantifier value");

                    // check if the quantifier is mutable
                    if is_mut {
                        let new_var_id = self.create_var_id();
                        self.state_mut()
                            .quantifiers
                            .insert(new_var_id, value.var, true);
                        self.bounded_quantifiers_mut()
                            .insert(ident.to_owned(), new_var_id);
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

        fn trait_var_builder<'a>(&'a mut self) -> TraitVarBuilder<'a, Self> {
            TraitVarBuilder::new(self)
        }

        fn trait_bounds_var_builder<'a>(&'a mut self) -> TraitBoundsVarBuilder<'a, Self> {
            TraitBoundsVarBuilder::new(self)
        }

        fn local_quantifiers(&self) -> HashMap<Ident, Quantifier> {
            let mut shadowed = HashSet::new();
            let mut found = HashMap::new();
            self.recursive_local_quantifiers(&mut shadowed, &mut found);
            found
        }

        fn sub_scope<'a, F, T>(&'a mut self, f: F) -> T
        where
            F: FnOnce(&mut SubScope<'a, Self>) -> T,
        {
            let state = self.clone_state();
            let mut sub_scope = SubScope {
                parent: self,
                bounded_quantifiers: HashMap::new(),
                state,
            };
            f(&mut sub_scope)
        }
    }

    #[derive(Debug, Clone)]
    pub struct RootScope {
        bounded_quantifiers: HashMap<Ident, usize>,
        state: SharedScopeState,
    }

    impl RootScope {
        pub fn new() -> Self {
            Self {
                bounded_quantifiers: HashMap::new(),
                state: SharedScopeState::new(),
            }
        }
    }

    impl Scope for RootScope {
        fn state(&self) -> Ref<ScopeState> {
            self.state.borrow()
        }

        fn state_mut(&mut self) -> RefMut<ScopeState> {
            self.state.borrow_mut()
        }

        fn clone_state(&self) -> SharedScopeState {
            self.state.clone()
        }

        fn bounded_quantifiers(&self) -> &HashMap<Ident, usize> {
            &self.bounded_quantifiers
        }

        fn bounded_quantifiers_mut(&mut self) -> &mut HashMap<Ident, usize> {
            &mut self.bounded_quantifiers
        }

        fn assign_quantifier_on_parent(&mut self, _ident: &Ident, _value: Scoped<TypeVar>) -> bool {
            false
        }

        fn get_var_id(&self, ident: &Ident) -> Option<usize> {
            self.bounded_quantifiers().get(ident).copied()
        }

        fn branch(&self) -> RootScope {
            self.clone()
        }

        fn recursive_local_quantifiers(
            &self,
            shadowed: &mut HashSet<Ident>,
            found: &mut HashMap<Ident, Quantifier>,
        ) {
            self.bounded_quantifiers()
                .iter()
                .for_each(|(ident, var_id)| {
                    shadowed.get_or_insert_with(ident, |_| {
                        let quantifier = self
                            .state()
                            .quantifiers
                            .get(var_id)
                            .expect("please report bug: missing quantifier value")
                            .to_owned();
                        found.insert(ident.to_owned(), quantifier);
                        ident.to_owned()
                    });
                });
        }
    }

    #[derive(Debug)]
    pub struct SubScope<'a, S>
    where
        S: Scope,
    {
        parent: &'a mut S,
        bounded_quantifiers: HashMap<Ident, usize>,
        state: SharedScopeState,
    }

    impl<'a, S> Scope for SubScope<'a, S>
    where
        S: Scope,
    {
        fn state(&self) -> Ref<ScopeState> {
            self.state.borrow()
        }

        fn state_mut(&mut self) -> RefMut<ScopeState> {
            self.state.borrow_mut()
        }

        fn clone_state(&self) -> SharedScopeState {
            self.state.clone()
        }

        fn bounded_quantifiers(&self) -> &HashMap<Ident, usize> {
            &self.bounded_quantifiers
        }

        fn bounded_quantifiers_mut(&mut self) -> &mut HashMap<Ident, usize> {
            &mut self.bounded_quantifiers
        }

        fn assign_quantifier_on_parent(&mut self, ident: &Ident, value: Scoped<TypeVar>) -> bool {
            self.parent.assign_quantifier(ident, value)
        }

        fn get_var_id(&self, ident: &Ident) -> Option<usize> {
            self.bounded_quantifiers()
                .get(ident)
                .copied()
                .or_else(|| self.parent.get_var_id(ident))
        }

        fn branch(&self) -> RootScope {
            let Self {
                parent,
                bounded_quantifiers,
                state,
            } = self;

            let mut scope = parent.branch();
            scope.bounded_quantifiers = scope
                .bounded_quantifiers
                .into_iter()
                .chain(bounded_quantifiers.clone().into_iter())
                .collect();
            scope.state = state.clone();
            scope
        }

        fn recursive_local_quantifiers(
            &self,
            shadowed: &mut HashSet<Ident>,
            found: &mut HashMap<Ident, Quantifier>,
        ) {
            self.bounded_quantifiers()
                .iter()
                .for_each(|(ident, var_id)| {
                    shadowed.get_or_insert_with(ident, |_| {
                        let quantifier = self
                            .state()
                            .quantifiers
                            .get(var_id)
                            .expect("please report bug: missing quantifier value")
                            .to_owned();
                        found.insert(ident.to_owned(), quantifier);
                        ident.to_owned()
                    });
                });
            self.parent.recursive_local_quantifiers(shadowed, found);
        }
    }

    #[derive(Debug, Clone)]
    pub struct ScopeState {
        var_counter: usize,
        trait_bounds: TraitBoundDict,
        quantifiers: QuantifierDict,
    }

    impl ScopeState {
        pub fn new() -> Self {
            Self {
                var_counter: 0,
                trait_bounds: TraitBoundDict::new(),
                quantifiers: QuantifierDict::new(),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct SharedScopeState(ByAddress<Rc<RefCell<ScopeState>>>);

    impl SharedScopeState {
        pub fn new() -> Self {
            Self(ByAddress(Rc::new(RefCell::new(ScopeState::new()))))
        }

        fn borrow(&self) -> Ref<ScopeState> {
            self.0.deref().deref().borrow()
        }

        fn borrow_mut(&mut self) -> RefMut<ScopeState> {
            self.0.deref().deref().borrow_mut()
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

        pub fn from_ident(&mut self, ident: &Ident) -> Scoped<TypeVar>
        where
            S: Scope,
        {
            Scoped {
                scope: self.scope.clone_state(),
                var: TypeVar::Path {
                    segments: vec![SegmentVar {
                        ident: ident.to_owned(),
                        generic_args: vec![],
                    }],
                },
            }
        }

        pub fn from_quantifier(&mut self, ident: &Ident) -> Option<Scoped<TypeVar>>
        where
            S: Scope,
        {
            Some(Scoped {
                scope: self.scope.clone_state(),
                var: TypeVar::Var {
                    id: self.scope.get_var_id(ident)?,
                },
            })
        }

        pub fn from_path(
            &mut self,
            qself: Option<&QSelf>,
            path: &Path,
        ) -> syn::Result<Scoped<TypeVar>>
        where
            S: Scope,
        {
            Ok(Scoped {
                scope: self.scope.clone_state(),
                var: type_var_fn::from_path(self.scope, qself, path)?,
            })
        }

        pub fn from_pat(&mut self, pat: &Pat) -> syn::Result<Scoped<TypeVar>>
        where
            S: Scope,
        {
            Ok(Scoped {
                scope: self.scope.clone_state(),
                var: type_var_fn::from_pat(self.scope, pat)?,
            })
        }

        pub fn make_tuple<I>(&mut self, paths: I) -> Scoped<TypeVar>
        where
            S: Scope,
            I: IntoIterator<Item = Scoped<TypeVar>>,
        {
            Scoped {
                scope: self.scope.clone_state(),
                var: TypeVar::Tuple {
                    types: paths
                        .into_iter()
                        .map(|ty| {
                            let Scoped { scope, var } = ty;
                            var
                        })
                        .collect(),
                },
            }
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

        pub fn from_path(&mut self, path: &Path) -> syn::Result<Scoped<TraitVar>> {
            Ok(Scoped {
                scope: self.scope.clone_state(),
                var: trait_var_fn::from_path(self.scope, path)?,
            })
        }
    }

    #[derive(Debug)]
    pub struct TraitBoundsVarBuilder<'a, S>
    where
        S: Scope,
    {
        scope: &'a mut S,
    }

    impl<'a, S> TraitBoundsVarBuilder<'a, S>
    where
        S: Scope,
    {
        pub fn new(scope: &'a mut S) -> Self {
            Self { scope }
        }

        pub fn empty(&mut self) -> Scoped<TraitBoundsVar> {
            Scoped {
                scope: self.scope.clone_state(),
                var: TraitBoundsVar {
                    traits: BTreeSet::new(),
                },
            }
        }
        pub fn from_type(&mut self, ty: &Type) -> syn::Result<Scoped<TraitBoundsVar>> {
            Ok(Scoped {
                scope: self.scope.clone_state(),
                var: trait_bounds_var_fn::from_type(self.scope, ty)?,
            })
        }

        pub fn from_type_param_bounds(
            &mut self,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<Scoped<TraitBoundsVar>> {
            Ok(Scoped {
                scope: self.scope.clone_state(),
                var: trait_bounds_var_fn::from_type_param_bounds(self.scope, bounds)?,
            })
        }
    }

    mod segment_var_fn {
        use super::*;

        pub fn from_segments<S, I, P>(scope: &S, segments: I) -> syn::Result<Vec<SegmentVar>>
        where
            S: Scope,
            I: IntoIterator<Item = P>,
            P: Borrow<PathSegment>,
        {
            segments
                .into_iter()
                .map(|segment| {
                    let PathSegment {
                        ident, arguments, ..
                    } = segment.borrow();

                    let generic_args = match &arguments {
                        PathArguments::None => vec![],
                        PathArguments::Parenthesized(_) => {
                            return Err(Error::new(
                                arguments.span(),
                                "parenthesized arguments are not allowed",
                            ));
                        }
                        PathArguments::AngleBracketed(args) => {
                            let generic_args: Vec<_> = args
                                .args
                                .iter()
                                .map(|arg| -> syn::Result<_> {
                                    match arg {
                                        GenericArgument::Type(ty) => {
                                            type_var_fn::from_type(scope, ty)
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

                    Ok(SegmentVar {
                        ident: ident.to_owned(),
                        generic_args,
                    })
                })
                .try_collect()
        }
    }

    mod type_var_fn {
        use super::*;

        pub fn from_type<S>(scope: &S, ty: &Type) -> syn::Result<TypeVar>
        where
            S: Scope,
        {
            match ty {
                Type::Path(TypePath { qself, path, .. }) => from_path(scope, qself.as_ref(), path),
                Type::Paren(TypeParen { elem, .. }) => from_type(scope, &*elem),
                Type::Tuple(TypeTuple { elems, .. }) => {
                    let types: Vec<_> = elems
                        .iter()
                        .map(|elem| from_type(scope, elem))
                        .try_collect()?;
                    Ok(TypeVar::Tuple { types })
                }
                _ => return Err(Error::new(ty.span(), "unsupported type variant")),
            }
        }

        pub fn from_path<S>(scope: &S, qself: Option<&QSelf>, path: &Path) -> syn::Result<TypeVar>
        where
            S: Scope,
        {
            let var = match qself {
                Some(QSelf { ty, position, .. }) => {
                    let position = *position;
                    let ty = from_type(scope, &**ty)?;
                    let iter = path.segments.iter();
                    let trait_ =
                        trait_var_fn::from_path_segments(scope, iter.clone().take(position))?;
                    let associated = segment_var_fn::from_segments(scope, iter.skip(position))?;
                    TypeVar::QSelf {
                        ty: Box::new(ty),
                        trait_,
                        associated,
                    }
                }
                None => {
                    // check if the path coincides with quantifiers
                    if let Some(ident) = path.get_ident() {
                        if let Some(var_id) = scope.get_var_id(ident) {
                            let var = TypeVar::Var { id: var_id };
                            return Ok(var);
                        }
                    }

                    let segments = segment_var_fn::from_segments(scope, &path.segments)?;
                    TypeVar::Path { segments }
                }
            };

            Ok(var)
        }

        pub fn from_pat<S>(scope: &S, pat: &Pat) -> syn::Result<TypeVar>
        where
            S: Scope,
        {
            match pat {
                Pat::Ident(PatIdent { ident, .. }) => {
                    let var = match scope.get_var_id(ident) {
                        Some(var_id) => TypeVar::Var { id: var_id },
                        None => TypeVar::Path {
                            segments: vec![SegmentVar {
                                ident: ident.to_owned(),
                                generic_args: vec![],
                            }],
                        },
                    };
                    Ok(var)
                }
                Pat::Path(PatPath { qself, path, .. }) => from_path(scope, qself.as_ref(), path),
                _ => Err(Error::new(pat.span(), "not an type")),
            }
        }
    }

    mod trait_var_fn {
        use super::*;

        pub fn from_path<S>(scope: &S, Path { segments, .. }: &Path) -> syn::Result<TraitVar>
        where
            S: Scope,
        {
            let segments = segment_var_fn::from_segments(scope, segments)?;
            Ok(TraitVar { segments })
        }

        pub fn from_path_segments<S, I, P>(scope: &S, segments: I) -> syn::Result<TraitVar>
        where
            S: Scope,
            I: IntoIterator<Item = P>,
            P: Borrow<PathSegment>,
        {
            Ok(TraitVar {
                segments: segment_var_fn::from_segments(scope, segments)?,
            })
        }
    }

    mod trait_bounds_var_fn {
        use super::*;

        pub fn from_paths<S, I, P>(scope: &S, paths: I) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
            I: IntoIterator<Item = P>,
            P: Borrow<Path>,
        {
            let traits: BTreeSet<_> = paths
                .into_iter()
                .map(|path| trait_var_fn::from_path(scope, path.borrow()))
                .try_collect()?;
            let var = TraitBoundsVar { traits };
            Ok(var)
        }

        pub fn from_type<S>(scope: &S, ty: &Type) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
        {
            let paths = match ty {
                Type::Infer(_) => vec![],
                Type::Path(TypePath { qself: Some(_), .. }) => {
                    return Err(Error::new(ty.span(), "not a trait"));
                }
                Type::Path(TypePath {
                    path, qself: None, ..
                }) => vec![path],
                Type::TraitObject(tobj) => {
                    if let Some(token) = tobj.dyn_token {
                        return Err(Error::new(token.span(), "dyn token is not allowed"));
                    }

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

            from_paths(scope, paths)
        }

        pub fn from_type_param_bounds<S>(
            scope: &S,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<TraitBoundsVar>
        where
            S: Scope,
        {
            let paths: Vec<_> = bounds
                .iter()
                .map(|bound| match bound {
                    // TODO: check modifier
                    TypeParamBound::Trait(trait_bound) => Ok(&trait_bound.path),
                    TypeParamBound::Lifetime(lifetime) => {
                        Err(Error::new(lifetime.span(), "lifetime is not allowed"))
                    }
                })
                .try_collect()?;
            from_paths(scope, paths)
        }
    }
}

mod quantifier_dict {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct QuantifierDict {
        map: HashMap<usize, Quantifier>,
    }

    impl QuantifierDict {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        // pub fn contains<K>(&self, var_id: K) -> bool
        // where
        //     K: std::borrow::Borrow<usize>,
        // {
        //     self.map.contains_key(var_id.borrow())
        // }

        pub fn insert(&mut self, var_id: usize, value: TypeVar, is_mut: bool) {
            let prev = self.map.insert(
                var_id,
                Quantifier {
                    id: var_id,
                    is_mut,
                    value,
                },
            );
            if let Some(_) = prev {
                panic!("please report bug: the type varaible is initialized more than once");
            }
        }

        pub fn get<K>(&self, var_id: K) -> Option<&Quantifier>
        where
            K: std::borrow::Borrow<usize>,
        {
            self.map.get(var_id.borrow())
        }
    }

    impl IntoIterator for QuantifierDict {
        type Item = (usize, TypeVar);
        type IntoIter = Box<dyn Iterator<Item = (usize, TypeVar)>>;

        fn into_iter(self) -> Self::IntoIter {
            let iter = self
                .map
                .into_iter()
                .map(|(var_id, Quantifier { value, .. })| (var_id, value));
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
    pub struct Quantifier {
        pub id: usize,
        pub is_mut: bool,
        pub value: TypeVar,
    }
}

mod trait_bounds_dict {
    use super::*;

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
                Entry::Occupied(mut entry) => {
                    *entry.get_mut() += bounds;
                }
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

    impl IntoIterator for TraitBoundDict {
        type Item = (TypeVar, TraitBoundsVar);
        type IntoIter = std::collections::hash_map::IntoIter<TypeVar, TraitBoundsVar>;

        fn into_iter(self) -> Self::IntoIter {
            self.bounds.into_iter()
        }
    }
}
