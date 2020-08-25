use crate::{
    common::*,
    var::{Scoped, SegmentVar, TraitBoundsVar, TraitVar, TypeVar},
};

pub use scope::*;
pub use var_builder::*;

mod scope {
    use super::*;

    #[derive(Debug)]
    pub struct Scope {
        state: SharedScopeState,
    }

    impl Clone for Scope {
        fn clone(&self) -> Self {
            Self {
                state: self.state.deep_clone(),
            }
        }
    }

    impl Scope {
        pub fn new() -> Self {
            Self {
                state: SharedScopeState::new(),
            }
        }

        fn state(&self) -> Ref<ScopeState> {
            self.state.borrow()
        }

        fn state_mut(&self) -> RefMut<ScopeState> {
            self.state.borrow_mut()
        }

        fn local_quantifiers(&self) -> Ref<HashMap<Ident, usize>> {
            Ref::map(self.state(), |state| {
                state.bounded_quantifiers.last().unwrap()
            })
        }

        fn local_quantifiers_mut(&mut self) -> RefMut<HashMap<Ident, usize>> {
            RefMut::map(self.state_mut(), |state| {
                state.bounded_quantifiers.last_mut().unwrap()
            })
        }

        pub fn mutable_quantifiers(&self) -> HashMap<Ident, Variable> {
            let (_shadowed, found) = self.state().bounded_quantifiers.iter().rev().fold(
                (HashSet::new(), HashMap::new()),
                |state, quantifiers| {
                    quantifiers
                        .iter()
                        .fold(state, |mut state, (ident, var_id)| {
                            let (shadowed, found) = &mut state;

                            // insert if the variable is not shadowed yet
                            shadowed.get_or_insert_with(ident, |_| {
                                let quantifier = self
                                    .state()
                                    .variables
                                    .get(var_id)
                                    .expect("please report bug: missing variable")
                                    .to_owned();

                                // add to result set if it is mutable
                                if quantifier.is_mut {
                                    found.insert(ident.to_owned(), quantifier);
                                }

                                ident.to_owned()
                            });
                            state
                        })
                },
            );
            found
        }

        pub fn free_quantifiers(&self) -> IndexMap<Ident, Variable> {
            self.state()
                .free_quantifiers
                .iter()
                .map(|(ident, var_id)| {
                    let var = self
                        .state()
                        .variables
                        .get(var_id)
                        .expect("please report bug: missing variable")
                        .to_owned();
                    (ident.to_owned(), var)
                })
                .collect()
        }

        pub fn trait_bounds(&self) -> Ref<Vec<(TokenStream, TokenStream)>> {
            Ref::map(self.state(), |state| &state.trait_bounds)
        }

        pub fn shared_state(&mut self) -> SharedScopeState {
            self.state.clone()
        }

        pub fn get_var_id(&self, ident: &Ident) -> Option<usize> {
            // search from bounded quantifiers
            for quantifiers in self.state().bounded_quantifiers.iter().rev() {
                if let Some(id) = quantifiers.get(ident) {
                    return Some(*id);
                }
            }

            // search from free quantifiers
            if let Some(id) = self.state().free_quantifiers.get(ident) {
                return Some(*id);
            }

            None
        }

        pub fn get_quantifier(&self, ident: &Ident) -> Option<Variable> {
            let state = self.state();

            state
                .bounded_quantifiers
                .iter()
                .rev()
                .find_map(|quantifiers| {
                    quantifiers
                        .get(ident)
                        .map(|var_id| state.variables[var_id].to_owned())
                })
        }

        pub fn generate_trait_bounds_tokens(&self) -> TokenStream {
            let bounds: Vec<_> = self
                .state()
                .trait_bounds
                .iter()
                .map(|(predicate, bounds)| {
                    quote! { #predicate: #bounds }
                })
                .collect();
            quote! { #(#bounds),* }
        }

        pub fn insert_free_quantifier(&mut self, ident: Ident) -> syn::Result<()> {
            // check if the identifier is already taken
            if self.state_mut().free_quantifiers.contains_key(&ident) {
                return Err(Error::new(
                    ident.span(),
                    "the name of free quantifier is already taken",
                ));
            }

            // create a new variable
            let var_id = self.state_mut().create_var_id();
            self.state.borrow_mut().variables.insert(
                var_id,
                Variable {
                    id: var_id,
                    value: quote! { #ident },
                    is_mut: false,
                },
            );

            self.state_mut().free_quantifiers.insert(ident, var_id);
            Ok(())
        }

        pub fn insert_bounded_quantifier(
            &mut self,
            ident: Ident,
            value: TokenStream,
            is_mut: bool,
        ) {
            // create a new variable
            let var_id = self.state_mut().create_var_id();
            self.state.borrow_mut().variables.insert(
                var_id,
                Variable {
                    id: var_id,
                    value,
                    is_mut,
                },
            );

            // add the identifier to current scop
            self.local_quantifiers_mut().insert(ident, var_id);
        }

        pub fn insert_trait_bounds(&mut self, predicate: TokenStream, bounds: TokenStream) {
            self.state
                .borrow_mut()
                .trait_bounds
                .push((predicate, bounds));
        }

        pub fn assign_quantifier(&mut self, ident: &Ident, value: TokenStream) -> syn::Result<()> {
            let opt = self
                .state()
                .bounded_quantifiers
                .iter()
                .enumerate()
                .rev()
                .find_map(|(index, quantifiers)| {
                    quantifiers
                        .get(ident)
                        .copied()
                        .map(|var_id| (index, var_id))
                });

            match opt {
                Some((index, prev_var_id)) => {
                    // check if the quantifier is mutable
                    let is_mut = self
                        .state()
                        .variables
                        .get(&prev_var_id)
                        .expect("please report bug: missing quantifier value")
                        .is_mut;

                    if is_mut {
                        let mut state = self.state_mut();

                        // create a new quantifier instance
                        let new_var_id = state.create_var_id();
                        state.variables.insert(
                            new_var_id,
                            Variable {
                                id: new_var_id,
                                value,
                                is_mut: true,
                            },
                        );

                        // replace the id that the identifier points to
                        state.bounded_quantifiers[index].insert(ident.to_owned(), new_var_id);

                        Ok(())
                    } else {
                        Err(Error::new(ident.span(), "the quantifier is not mutable"))
                    }
                }
                None => Err(Error::new(ident.span(), "the quantifier is not defined")),
            }
        }

        pub fn type_var_builder<'a>(&'a mut self) -> TypeVarBuilder<'a> {
            TypeVarBuilder::new(self)
        }

        pub fn trait_var_builder<'a>(&'a mut self) -> TraitVarBuilder<'a> {
            TraitVarBuilder::new(self)
        }

        pub fn trait_bounds_var_builder<'a>(&'a mut self) -> TraitBoundsVarBuilder<'a> {
            TraitBoundsVarBuilder::new(self)
        }

        pub fn sub_scope<F, T>(&mut self, f: F) -> T
        where
            F: FnOnce(&mut Scope) -> T,
        {
            let mut sub_scope = Box::new(Scope {
                state: self.state.clone(),
            });
            f(&mut sub_scope)
        }
    }

    #[derive(Debug, Clone)]
    pub struct ScopeState {
        var_counter: usize,
        trait_bounds: Vec<(TokenStream, TokenStream)>,
        variables: HashMap<usize, Variable>,
        free_quantifiers: IndexMap<Ident, usize>,
        bounded_quantifiers: Vec<HashMap<Ident, usize>>,
    }

    impl ScopeState {
        pub fn new() -> Self {
            Self {
                var_counter: 0,
                trait_bounds: vec![],
                variables: HashMap::new(),
                free_quantifiers: IndexMap::new(),
                bounded_quantifiers: vec![HashMap::new()],
            }
        }

        pub fn get_variable(&self, id: &usize) -> Option<&Variable> {
            self.variables.get(id)
        }

        pub fn create_var_id(&mut self) -> usize {
            let var_counter = &mut self.var_counter;
            let ret = *var_counter;
            *var_counter += 1;
            ret
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct SharedScopeState(ByAddress<Rc<RefCell<ScopeState>>>);

    impl SharedScopeState {
        pub fn new() -> Self {
            Self(ByAddress(Rc::new(RefCell::new(ScopeState::new()))))
        }

        pub fn borrow(&self) -> Ref<ScopeState> {
            self.0.deref().deref().borrow()
        }

        pub fn borrow_mut(&self) -> RefMut<ScopeState> {
            self.0.deref().deref().borrow_mut()
        }

        pub fn deep_clone(&self) -> Self {
            Self(ByAddress(Rc::new(RefCell::new(
                (**self.0).borrow().to_owned(),
            ))))
        }
    }

    #[derive(Debug, Clone)]
    pub struct Variable {
        pub id: usize,
        pub is_mut: bool,
        pub value: TokenStream,
    }
}

mod var_builder {
    use super::*;

    #[derive(Debug)]
    pub struct TypeVarBuilder<'a> {
        scope: &'a mut Scope,
    }

    impl<'a> TypeVarBuilder<'a> {
        pub fn new(scope: &'a mut Scope) -> Self {
            Self { scope }
        }

        pub fn from_ident(&mut self, ident: &Ident) -> Scoped<TypeVar> {
            Scoped {
                scope: self.scope.shared_state(),
                var: TypeVar::Path {
                    segments: vec![SegmentVar {
                        ident: ident.to_owned(),
                        generic_args: vec![],
                    }],
                },
            }
        }

        pub fn from_quantifier(&mut self, ident: &Ident) -> Option<Scoped<TypeVar>> {
            Some(Scoped {
                scope: self.scope.shared_state(),
                var: TypeVar::Var {
                    id: self.scope.get_var_id(ident)?,
                },
            })
        }

        pub fn from_path(
            &mut self,
            qself: Option<&QSelf>,
            path: &Path,
        ) -> syn::Result<Scoped<TypeVar>> {
            Ok(Scoped {
                scope: self.scope.shared_state(),
                var: type_var_fn::from_path(self.scope, qself, path)?,
            })
        }

        pub fn from_pat(&mut self, pat: &Pat) -> syn::Result<Scoped<TypeVar>> {
            Ok(Scoped {
                scope: self.scope.shared_state(),
                var: type_var_fn::from_pat(self.scope, pat)?,
            })
        }

        pub fn make_tuple<I>(&mut self, paths: I) -> Scoped<TypeVar>
        where
            I: IntoIterator<Item = Scoped<TypeVar>>,
        {
            let state = self.scope.shared_state();
            Scoped {
                scope: self.scope.shared_state(),
                var: TypeVar::Tuple {
                    types: paths
                        .into_iter()
                        .map(|ty| {
                            let Scoped { scope, var } = ty;
                            assert_eq!(state, scope);
                            var
                        })
                        .collect(),
                },
            }
        }
    }

    #[derive(Debug)]
    pub struct TraitVarBuilder<'a> {
        scope: &'a mut Scope,
    }

    impl<'a> TraitVarBuilder<'a> {
        pub fn new(scope: &'a mut Scope) -> Self {
            Self { scope }
        }

        pub fn from_path(&mut self, path: &Path) -> syn::Result<Scoped<TraitVar>> {
            Ok(Scoped {
                scope: self.scope.shared_state(),
                var: trait_var_fn::from_path(self.scope, path)?,
            })
        }
    }

    #[derive(Debug)]
    pub struct TraitBoundsVarBuilder<'a> {
        scope: &'a mut Scope,
    }

    impl<'a> TraitBoundsVarBuilder<'a> {
        pub fn new(scope: &'a mut Scope) -> Self {
            Self { scope }
        }

        pub fn empty(&mut self) -> Scoped<TraitBoundsVar> {
            Scoped {
                scope: self.scope.shared_state(),
                var: TraitBoundsVar {
                    traits: BTreeSet::new(),
                },
            }
        }
        pub fn from_type(&mut self, ty: &Type) -> syn::Result<Scoped<TraitBoundsVar>> {
            Ok(Scoped {
                scope: self.scope.shared_state(),
                var: trait_bounds_var_fn::from_type(self.scope, ty)?,
            })
        }

        pub fn from_type_param_bounds(
            &mut self,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<Scoped<TraitBoundsVar>> {
            Ok(Scoped {
                scope: self.scope.shared_state(),
                var: trait_bounds_var_fn::from_type_param_bounds(self.scope, bounds)?,
            })
        }
    }

    mod segment_var_fn {
        use super::*;

        pub fn from_segments<I, P>(scope: &Scope, segments: I) -> syn::Result<Vec<SegmentVar>>
        where
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

        pub fn from_type(scope: &Scope, ty: &Type) -> syn::Result<TypeVar> {
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

        pub fn from_path(
            scope: &Scope,
            qself: Option<&QSelf>,
            path: &Path,
        ) -> syn::Result<TypeVar> {
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

        pub fn from_pat(scope: &Scope, pat: &Pat) -> syn::Result<TypeVar> {
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

        pub fn from_path(scope: &Scope, Path { segments, .. }: &Path) -> syn::Result<TraitVar> {
            let segments = segment_var_fn::from_segments(scope, segments)?;
            Ok(TraitVar { segments })
        }

        pub fn from_path_segments<I, P>(scope: &Scope, segments: I) -> syn::Result<TraitVar>
        where
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

        pub fn from_paths<I, P>(scope: &Scope, paths: I) -> syn::Result<TraitBoundsVar>
        where
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

        pub fn from_type(scope: &Scope, ty: &Type) -> syn::Result<TraitBoundsVar> {
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

        pub fn from_type_param_bounds(
            scope: &Scope,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<TraitBoundsVar> {
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
