use crate::{
    common::*,
    utils::{IntoOwnedTokens, Shared, SharedCell},
    var::{ParseTypeVar, TypeVar},
};

pub use env::*;
pub use pure_trans::*;
pub use substitute::*;

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
            T: IntoOwnedTokens,
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
            let target = Rc::new(target.into_owned_tokens());
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
            self.orig.global.borrow_mut().bounded_quantifiers.pop();
            self.orig.locals = self
                .branches
                .into_iter()
                .flat_map(|branched| {
                    let locals = branched.locals;
                    locals.iter().cloned().for_each(|local| {
                        local.borrow_mut().bounded_qvs.pop();
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

    impl LocalEnv {
        // pub fn substitute_type<T>(&self, type_: &T) -> syn::Result<TokenStream>
        // where
        //     T: TypeSubst,
        // {
        //     type_.substitute_type(self)
        // }

        // pub fn substitute_trait<T>(&self, trait_: &T) -> syn::Result<TokenStream>
        // where
        //     T: TraitSubst,
        // {
        //     trait_.substitute_trait(self)
        // }

        // pub fn substitute_trait_bounds<T>(&self, trait_bounds: &T) -> syn::Result<TokenStream>
        // where
        //     T: TraitBoundsSubst,
        // {
        //     trait_bounds.substitute_trait_bounds(self)
        // }

        pub fn get_quantifier_value(&self, ident: &Ident) -> Option<Rc<TokenStream>> {
            let opt = self
                .local
                .borrow()
                .bounded_qvs
                .iter()
                .rev()
                .find_map(|qvs| qvs.get(ident).cloned());

            opt.or_else(|| {
                self.global
                    .borrow()
                    .free_quantifiers
                    .get(ident)
                    .map(|_| Rc::new(quote! { #ident }))
            })
        }
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

        pub fn parse_type<T>(&self, ty: T) -> syn::Result<TypeVar>
        where
            T: ParseTypeVar,
        {
            ty.parse_type_var(self)
        }

        pub fn get_quantifier(&self, ident: &Ident) -> Option<Shared<Variable>> {
            let opt = self
                .global
                .borrow()
                .bounded_quantifiers
                .iter()
                .rev()
                .find_map(|quantifiers| quantifiers.get(ident).cloned());

            opt.or_else(|| self.global.borrow().free_quantifiers.get(ident).cloned())
        }

        // pub fn mutable_quantifiers(&self) -> IndexSet<Rc<Ident>> {
        //     let (_shadowed, found) = self.global.borrow().bounded_quantifiers.iter().rev().fold(
        //         (HashSet::new(), IndexSet::new()),
        //         |mut state, quantifiers| {
        //             let (shadowed, found) = &mut state;
        //             quantifiers.iter().for_each(|(ident, var)| {
        //                 if shadowed.insert(ident.clone()) {
        //                     if var.is_mut {
        //                         found.insert(ident.clone());
        //                     }
        //                 }
        //             });
        //             state
        //         },
        //     );
        //     found
        // }

        pub fn insert_condition<T>(&self, cond: T) -> usize
        where
            T: IntoOwnedTokens,
        {
            let mut global = self.global.borrow_mut();
            let id = global.conditions.len();
            global.conditions.push(Rc::new(cond.into_owned_tokens()));
            id
        }

        pub fn substitute_type<T>(&self, type_: &T) -> syn::Result<Vec<TokenStream>>
        where
            T: TypeSubst,
        {
            self.branches()
                .into_iter()
                .map(|scope| type_.substitute_type(&scope))
                .try_collect()
        }

        pub fn substitute_trait<T>(&self, trait_: &T) -> syn::Result<Vec<TokenStream>>
        where
            T: TraitSubst,
        {
            self.branches()
                .into_iter()
                .map(|scope| trait_.substitute_trait(&scope))
                .try_collect()
        }

        pub fn substitute_trait_bounds<T>(&self, trait_bounds: &T) -> syn::Result<Vec<TokenStream>>
        where
            T: TraitBoundsSubst,
        {
            self.branches()
                .into_iter()
                .map(|scope| trait_bounds.substitute_trait_bounds(&scope))
                .try_collect()
        }

        pub fn insert_free_quantifier(&mut self, ident: Ident) -> syn::Result<()> {
            let ident = Rc::new(ident);

            // check if the identifier is already taken
            if self.global.borrow().free_quantifiers.contains_key(&ident) {
                return Err(Error::new(
                    ident.span(),
                    "the name of free quantifier is already taken",
                ));
            }

            // create a new variable
            let var_id = self.next_var_id();
            let var = Shared::new(Variable {
                id: var_id,
                is_bounded: false,
                is_mut: false,
            });

            let mut global = self.global.borrow_mut();
            global.variables.push(var.clone());
            global.free_quantifiers.insert(ident, var);
            Ok(())
        }

        pub fn insert_bounded_quantifier<T>(&mut self, ident: Ident, is_mut: bool, values: Vec<T>)
        where
            T: IntoOwnedTokens,
        {
            // sanity check
            assert_eq!(
                values.len(),
                self.locals.len(),
                "please report bug: the number of values does not match"
            );

            // create a new variable
            let ident = Rc::new(ident);
            let var = {
                let var_id = self.next_var_id();
                let var = Shared::new(Variable {
                    id: var_id,
                    is_bounded: true,
                    is_mut,
                });
                self.global.borrow_mut().variables.push(var.clone());
                var
            };

            // insert to global state
            self.global
                .borrow_mut()
                .bounded_quantifiers
                .last_mut()
                .unwrap()
                .insert(ident.clone(), var);

            // insert to local states
            self.locals
                .iter_mut()
                .zip_eq(values)
                .for_each(|(local, value)| {
                    let value = value.into_owned_tokens();
                    local
                        .to_owned()
                        .borrow_mut()
                        .bounded_qvs
                        .last_mut()
                        .unwrap()
                        .insert(ident.clone(), Rc::new(value));
                });
        }

        pub fn insert_trait_bounds<T1, T2>(&mut self, predicates: Vec<(T1, T2)>)
        where
            T1: IntoOwnedTokens,
            T2: IntoOwnedTokens,
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
                .for_each(|(local, (ty, bounds))| {
                    let ty = ty.into_owned_tokens();
                    let bounds = bounds.into_owned_tokens();
                    local
                        .to_owned()
                        .borrow_mut()
                        .trait_bounds
                        .push((Rc::new(ty), Rc::new(bounds)));
                });
        }

        pub fn assign_quantifier<T>(&mut self, ident: &Ident, values: Vec<T>) -> syn::Result<()>
        where
            T: IntoOwnedTokens,
        {
            // sanity check
            assert_eq!(
                values.len(),
                self.locals.len(),
                "please report bug: the number of values does not match"
            );

            let opt = self
                .global
                .borrow()
                .bounded_quantifiers
                .iter()
                .enumerate()
                .rev()
                .find_map(|(index, variables)| {
                    variables.get(ident).cloned().map(|var| (index, var.is_mut))
                });

            match opt {
                Some((index, true)) => {
                    // create a new quantifier instance
                    let ident = Rc::new(ident.to_owned());
                    let var = Shared::new(Variable {
                        id: self.next_var_id(),
                        is_mut: true,
                        is_bounded: true,
                    });

                    {
                        let mut global = self.global.borrow_mut();
                        global.variables.push(var.clone());
                        global.bounded_quantifiers[index].insert(ident.clone(), var);
                    }

                    self.locals
                        .iter()
                        .cloned()
                        .zip_eq(values)
                        .for_each(|(local, value)| {
                            let value = value.into_owned_tokens();
                            local.borrow_mut().bounded_qvs[index]
                                .insert(ident.clone(), Rc::new(value));
                        });

                    Ok(())
                }
                Some((_, false)) => Err(Error::new(ident.span(), "the quantifier is not mutable")),
                None => Err(Error::new(ident.span(), "the quantifier is not defined")),
            }
        }

        pub fn push<T>(&mut self, values: Vec<T>) -> usize
        where
            T: IntoOwnedTokens,
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
                    let value = value.into_owned_tokens();
                    local
                        .borrow_mut()
                        .registers
                        .insert(register_id, Rc::new(value));
                });

            register_id
        }

        pub fn pop(&mut self, register_id: usize) -> Vec<Rc<TokenStream>> {
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
            // append one subscope
            self.global
                .borrow_mut()
                .bounded_quantifiers
                .push(HashMap::new());
            self.locals.iter().cloned().for_each(|local| {
                local.borrow_mut().bounded_qvs.push(HashMap::new());
            });

            // apply
            let ret = f(self);

            // pop subscope
            self.global.borrow_mut().bounded_quantifiers.pop();
            self.locals.iter().cloned().for_each(|local| {
                local.borrow_mut().bounded_qvs.pop();
            });

            ret
        }

        /// Compile trait bounds for each branch in form of [[ty, bounds]].
        pub fn trait_bounds(&self) -> Vec<Vec<(Rc<TokenStream>, Rc<TokenStream>)>> {
            self.locals
                .iter()
                .cloned()
                .map(|local| local.borrow().trait_bounds.clone())
                .collect()
        }

        /// Compile conditions for each branch in form of ([condition], [[target?]]).
        pub fn conditions(&self) -> (Vec<Rc<TokenStream>>, Vec<Vec<Option<Rc<TokenStream>>>>) {
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
            T: IntoOwnedTokens,
        {
            // sanity check
            assert_eq!(
                conditions.len(),
                self.locals.len(),
                "please report bug: the number of conditions does not match"
            );

            // append one subscope
            self.global
                .borrow_mut()
                .bounded_quantifiers
                .push(HashMap::new());
            self.locals.iter().cloned().for_each(|local| {
                local.borrow_mut().bounded_qvs.push(HashMap::new());
            });

            // save conditions on global state
            let condition_ids: Vec<_> = conditions
                .into_iter()
                .map(|cond| {
                    let cond = Rc::new(cond.into_owned_tokens());
                    let id = self.insert_condition(cond.clone());
                    id
                })
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
        trait_bounds: Vec<(Rc<TokenStream>, Rc<TokenStream>)>,
        bounded_qvs: Vec<HashMap<Rc<Ident>, Rc<TokenStream>>>,
        registers: HashMap<usize, Rc<TokenStream>>,
        // cond id -> (condition, target)
        condition_targets: HashMap<usize, Rc<TokenStream>>,
    }

    impl LocalState {
        pub fn new() -> Self {
            Self {
                trait_bounds: vec![],
                bounded_qvs: vec![HashMap::new()],
                registers: HashMap::new(),
                condition_targets: HashMap::new(),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct GlobalState {
        variables: Vec<Shared<Variable>>,
        free_quantifiers: IndexMap<Rc<Ident>, Shared<Variable>>,
        bounded_quantifiers: Vec<HashMap<Rc<Ident>, Shared<Variable>>>,
        register_counter: usize,
        conditions: Vec<Rc<TokenStream>>,
    }

    impl GlobalState {
        pub fn new() -> Self {
            Self {
                variables: vec![],
                free_quantifiers: IndexMap::new(),
                bounded_quantifiers: vec![HashMap::new()],
                register_counter: 0,
                conditions: vec![],
            }
        }
    }

    #[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
    pub struct Variable {
        pub id: usize,
        pub is_mut: bool,
        pub is_bounded: bool,
    }
}

mod pure_trans {
    use super::*;

    // pure type
    pub trait IntoPureType {
        fn into_pure_type(&self) -> syn::Result<TokenStream>;
    }

    impl IntoPureType for Ident {
        fn into_pure_type(&self) -> syn::Result<TokenStream> {
            pure_type::from_ident(self)
        }
    }

    impl IntoPureType for Pat {
        fn into_pure_type(&self) -> syn::Result<TokenStream> {
            pure_type::from_pat(self)
        }
    }

    // pure trait
    pub trait IntoPureTrait {
        fn into_pure_trait(&self) -> syn::Result<TokenStream>;
    }

    impl IntoPureTrait for Path {
        fn into_pure_trait(&self) -> syn::Result<TokenStream> {
            pure_trait::from_path(self)
        }
    }

    impl IntoPureTrait for [PathSegment] {
        fn into_pure_trait(&self) -> syn::Result<TokenStream> {
            pure_trait::from_path_segments(self.iter())
        }
    }

    // trait bounds substitution

    pub trait IntoPureTraitBounds {
        fn into_pure_trait_bounds(&self) -> syn::Result<TokenStream>;
    }

    impl IntoPureTraitBounds for &[Path] {
        fn into_pure_trait_bounds(&self) -> syn::Result<TokenStream> {
            pure_trait_bounds::from_paths(self.iter())
        }
    }

    impl IntoPureTraitBounds for Type {
        fn into_pure_trait_bounds(&self) -> syn::Result<TokenStream> {
            pure_trait_bounds::from_type(self)
        }
    }

    impl IntoPureTraitBounds for Punctuated<TypeParamBound, syn::token::Add> {
        fn into_pure_trait_bounds(&self) -> syn::Result<TokenStream> {
            pure_trait_bounds::from_type_param_bounds(self)
        }
    }

    mod pure_type {
        use super::*;

        pub fn from_ident(ident: &Ident) -> syn::Result<TokenStream> {
            Ok(quote! { #ident })
        }

        pub fn from_type(ty: &Type) -> syn::Result<TokenStream> {
            match ty {
                Type::Path(TypePath { qself, path, .. }) => from_path(qself.as_ref(), path),
                Type::Paren(TypeParen { elem, .. }) => from_type(&*elem),
                Type::Tuple(TypeTuple { elems, .. }) => {
                    let types: Vec<_> = elems.iter().map(|elem| from_type(elem)).try_collect()?;
                    Ok(quote! { ( #(#types),* ) })
                }
                _ => return Err(Error::new(ty.span(), "unsupported type variant")),
            }
        }

        pub fn from_path(qself: Option<&QSelf>, path: &Path) -> syn::Result<TokenStream> {
            match qself {
                Some(QSelf { ty, position, .. }) => {
                    let position = *position;
                    let remaining = path.segments.len() - position;
                    let ty = from_type(&**ty)?;
                    let iter = path.segments.iter();
                    let trait_ = pure_trait::from_path_segments(iter.clone().take(position))?;
                    let associated = pure_segment::from_segments(iter.skip(position))?;
                    let expanded = if remaining == 0 {
                        quote! { < #ty as #trait_ > }
                    } else {
                        quote! { < #ty as #trait_ > :: #associated }
                    };
                    Ok(expanded)
                }
                None => {
                    if let Some(ident) = path.get_ident() {
                        from_ident(ident)
                    } else {
                        pure_segment::from_segments(&path.segments)
                    }
                }
            }
        }

        pub fn from_pat(pat: &Pat) -> syn::Result<TokenStream> {
            match pat {
                Pat::Ident(PatIdent { ident, .. }) => from_ident(ident),
                Pat::Path(PatPath { qself, path, .. }) => from_path(qself.as_ref(), path),
                _ => Err(Error::new(pat.span(), "not an type")),
            }
        }
    }

    mod pure_segment {
        use super::*;

        pub fn from_segments<I, P>(segments: I) -> syn::Result<TokenStream>
        where
            I: IntoIterator<Item = P>,
            P: Borrow<PathSegment>,
        {
            let segments: Vec<_> = segments
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
                                        GenericArgument::Type(ty) => pure_type::from_type(ty),
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

                    let expanded = if generic_args.is_empty() {
                        quote! { #ident }
                    } else {
                        quote! { #ident < #(#generic_args),* > }
                    };

                    Ok(expanded)
                })
                .try_collect()?;

            Ok(quote! { #(#segments)::* })
        }
    }

    mod pure_trait {
        use super::*;

        pub fn from_path(Path { segments, .. }: &Path) -> syn::Result<TokenStream> {
            pure_segment::from_segments(segments)
        }

        pub fn from_path_segments<I, P>(segments: I) -> syn::Result<TokenStream>
        where
            I: IntoIterator<Item = P>,
            P: Borrow<PathSegment>,
        {
            pure_segment::from_segments(segments)
        }
    }

    mod pure_trait_bounds {
        use super::*;

        pub fn from_paths<I, P>(paths: I) -> syn::Result<TokenStream>
        where
            I: IntoIterator<Item = P>,
            P: Borrow<Path>,
        {
            let traits: Vec<_> = paths
                .into_iter()
                .map(|path| pure_trait::from_path(path.borrow()))
                .try_collect()?;
            Ok(quote! { #(#traits)+* })
        }

        pub fn from_type(ty: &Type) -> syn::Result<TokenStream> {
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

            from_paths(paths)
        }

        pub fn from_type_param_bounds(
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<TokenStream> {
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
            from_paths(paths)
        }
    }
}

mod substitute {
    use super::*;

    // type substitution

    pub trait TypeSubst {
        fn substitute_type(&self, scope: &LocalEnv) -> syn::Result<TokenStream>;
    }

    impl TypeSubst for Ident {
        fn substitute_type(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            type_subs::from_ident(scope, self)
        }
    }

    impl TypeSubst for Type {
        fn substitute_type(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            type_subs::from_type(scope, self)
        }
    }

    impl TypeSubst for ExprPath {
        fn substitute_type(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            let ExprPath { qself, path, .. } = self;
            type_subs::from_path(scope, qself.as_ref(), path)
        }
    }

    impl TypeSubst for Pat {
        fn substitute_type(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            type_subs::from_pat(scope, self)
        }
    }

    // trait substitution

    pub trait TraitSubst {
        fn substitute_trait(&self, scope: &LocalEnv) -> syn::Result<TokenStream>;
    }

    impl TraitSubst for Path {
        fn substitute_trait(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            trait_subs::from_path(scope, self)
        }
    }

    impl TraitSubst for &[PathSegment] {
        fn substitute_trait(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            trait_subs::from_path_segments(scope, self.iter())
        }
    }

    // trait bounds substitution

    pub trait TraitBoundsSubst {
        fn substitute_trait_bounds(&self, scope: &LocalEnv) -> syn::Result<TokenStream>;
    }

    impl TraitBoundsSubst for &[Path] {
        fn substitute_trait_bounds(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            trait_bounds_subs::from_paths(scope, self.iter())
        }
    }

    impl TraitBoundsSubst for Type {
        fn substitute_trait_bounds(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            trait_bounds_subs::from_type(scope, self)
        }
    }

    impl TraitBoundsSubst for Punctuated<TypeParamBound, syn::token::Add> {
        fn substitute_trait_bounds(&self, scope: &LocalEnv) -> syn::Result<TokenStream> {
            trait_bounds_subs::from_type_param_bounds(scope, self)
        }
    }

    mod segment_subs {
        use super::*;

        pub fn from_segments<I, P>(scope: &LocalEnv, segments: I) -> syn::Result<TokenStream>
        where
            I: IntoIterator<Item = P>,
            P: Borrow<PathSegment>,
        {
            let segments: Vec<_> = segments
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
                                            type_subs::from_type(scope, ty)
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

                    let expanded = if generic_args.is_empty() {
                        quote! { #ident }
                    } else {
                        quote! { #ident < #(#generic_args),* > }
                    };

                    Ok(expanded)
                })
                .try_collect()?;

            Ok(quote! { #(#segments)::* })
        }
    }

    mod type_subs {
        use super::*;

        pub fn from_ident(scope: &LocalEnv, ident: &Ident) -> syn::Result<TokenStream> {
            let expanded = scope
                .get_quantifier_value(ident)
                .map(|value| (*value).to_owned())
                .unwrap_or_else(|| quote! { #ident });
            Ok(expanded)
        }

        pub fn from_type(scope: &LocalEnv, ty: &Type) -> syn::Result<TokenStream> {
            match ty {
                Type::Path(TypePath { qself, path, .. }) => from_path(scope, qself.as_ref(), path),
                Type::Paren(TypeParen { elem, .. }) => from_type(scope, &*elem),
                Type::Tuple(TypeTuple { elems, .. }) => {
                    let types: Vec<_> = elems
                        .iter()
                        .map(|elem| from_type(scope, elem))
                        .try_collect()?;
                    Ok(quote! { ( #(#types),* ) })
                }
                _ => return Err(Error::new(ty.span(), "unsupported type variant")),
            }
        }

        pub fn from_path(
            scope: &LocalEnv,
            qself: Option<&QSelf>,
            path: &Path,
        ) -> syn::Result<TokenStream> {
            match qself {
                Some(QSelf { ty, position, .. }) => {
                    let position = *position;
                    let remaining = path.segments.len() - position;
                    let ty = from_type(scope, &**ty)?;
                    let iter = path.segments.iter();
                    let trait_ =
                        trait_subs::from_path_segments(scope, iter.clone().take(position))?;
                    let associated = segment_subs::from_segments(scope, iter.skip(position))?;
                    let expanded = if remaining == 0 {
                        quote! { < #ty as #trait_ > }
                    } else {
                        quote! { < #ty as #trait_ > :: #associated }
                    };
                    Ok(expanded)
                }
                None => {
                    // check if the path coincides with quantifiers
                    if let Some(ident) = path.get_ident() {
                        from_ident(scope, ident)
                    } else {
                        segment_subs::from_segments(scope, &path.segments)
                    }
                }
            }
        }

        pub fn from_pat(scope: &LocalEnv, pat: &Pat) -> syn::Result<TokenStream> {
            match pat {
                Pat::Ident(PatIdent { ident, .. }) => from_ident(scope, ident),
                Pat::Path(PatPath { qself, path, .. }) => from_path(scope, qself.as_ref(), path),
                _ => Err(Error::new(pat.span(), "not an type")),
            }
        }
    }

    mod trait_subs {
        use super::*;

        pub fn from_path(
            scope: &LocalEnv,
            Path { segments, .. }: &Path,
        ) -> syn::Result<TokenStream> {
            segment_subs::from_segments(scope, segments)
        }

        pub fn from_path_segments<I, P>(scope: &LocalEnv, segments: I) -> syn::Result<TokenStream>
        where
            I: IntoIterator<Item = P>,
            P: Borrow<PathSegment>,
        {
            segment_subs::from_segments(scope, segments)
        }
    }

    mod trait_bounds_subs {
        use super::*;

        pub fn from_paths<I, P>(scope: &LocalEnv, paths: I) -> syn::Result<TokenStream>
        where
            I: IntoIterator<Item = P>,
            P: Borrow<Path>,
        {
            let traits: Vec<_> = paths
                .into_iter()
                .map(|path| trait_subs::from_path(scope, path.borrow()))
                .try_collect()?;
            Ok(quote! { #(#traits)+* })
        }

        pub fn from_type(scope: &LocalEnv, ty: &Type) -> syn::Result<TokenStream> {
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
            scope: &LocalEnv,
            bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        ) -> syn::Result<TokenStream> {
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
