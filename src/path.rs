use crate::{common::*, scope::Scope};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeVar {
    Var(usize),
    Path(Vec<(Ident, Vec<TypeVar>)>),
}

impl TypeVar {
    pub fn from_scoped_path(path: &Path, scope: &Scope) -> syn::Result<Self> {
        // check if the path coincides with quantifiers
        if let Some(ident) = path.get_ident() {
            if let Some(var_id) = scope.get_var_id(ident) {
                return Ok(TypeVar::Var(var_id));
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
                                        Self::from_scoped_path(&ty_path.path, scope)
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

        Ok(TypeVar::Path(segments))
    }

    pub fn from_scoped_pat(pat: &Pat, scope: &Scope) -> syn::Result<Self> {
        match pat {
            Pat::Ident(pat_ident) => {
                let ident = &pat_ident.ident;
                let var = match scope.get_var_id(ident) {
                    Some(var_id) => TypeVar::Var(var_id),
                    None => TypeVar::Path(vec![(ident.to_owned(), vec![])]),
                };
                Ok(var)
            }
            Pat::Path(pat_path) => Self::from_scoped_path(&pat_path.path, scope),
            _ => Err(Error::new(pat.span(), "not an type")),
        }
    }
}

impl From<&Ident> for TypeVar {
    fn from(ident: &Ident) -> Self {
        Self::Path(vec![(ident.to_owned(), vec![])])
    }
}

impl TryFrom<&Path> for TypeVar {
    type Error = syn::Error;

    fn try_from(path: &Path) -> syn::Result<Self> {
        let segments: Vec<_> = path
            .segments
            .iter()
            .map(|segment| {
                let PathSegment {
                    ident, arguments, ..
                } = segment;

                let generic_args = match &arguments {
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
                                        TypeVar::try_from(&ty_path.path)
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

                Ok((ident.to_owned(), generic_args))
            })
            .try_collect()?;

        Ok(TypeVar::Path(segments))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBoundsVar {
    pub(crate) paths: BTreeSet<Vec<(Ident, Vec<TypeVar>)>>,
}

impl TraitBoundsVar {
    pub fn new() -> Self {
        Self {
            paths: BTreeSet::new(),
        }
    }

    pub fn from_scoped_path(path: &Path, scope: &Scope) -> syn::Result<Self> {
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
                                        TypeVar::from_scoped_path(&ty_path.path, scope)
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

        Ok(TraitBoundsVar { paths })
    }

    pub fn from_scoped_paths(paths: &[&Path], scope: &Scope) -> syn::Result<Self> {
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
                                                TypeVar::from_scoped_path(&ty_path.path, scope)
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

        Ok(TraitBoundsVar { paths })
    }

    pub fn from_scoped_type(ty: &Type, scope: &Scope) -> syn::Result<Self> {
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

        Self::from_scoped_paths(&paths, scope)
    }

    pub fn from_scoped_type_param_bounds(
        bounds: &Punctuated<TypeParamBound, syn::token::Add>,
        scope: &Scope,
    ) -> syn::Result<Self> {
        let paths: Vec<_> = bounds
            .iter()
            .map(|bound| match bound {
                TypeParamBound::Trait(trait_bound) => Ok(&trait_bound.path),
                TypeParamBound::Lifetime(lifetime) => {
                    Err(Error::new(lifetime.span(), "lifetime is not allowed"))
                }
            })
            .try_collect()?;
        Self::from_scoped_paths(&paths, scope)
    }
}

impl IntoIterator for TraitBoundsVar {
    type Item = Vec<(Ident, Vec<TypeVar>)>;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.paths.into_iter()
    }
}

impl Add<Self> for TraitBoundsVar {
    type Output = TraitBoundsVar;

    fn add(self, rhs: Self) -> Self::Output {
        let paths: BTreeSet<_> = self
            .paths
            .into_iter()
            .chain(rhs.paths.into_iter())
            .collect();
        Self { paths }
    }
}

impl AddAssign<Self> for TraitBoundsVar {
    fn add_assign(&mut self, other: TraitBoundsVar) {
        self.paths.extend(other.into_iter());
    }
}

impl Sum<Self> for TraitBoundsVar {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let paths: BTreeSet<_> = iter.map(|var| var.paths.into_iter()).flatten().collect();
        Self { paths }
    }
}

impl Extend<Vec<(Ident, Vec<TypeVar>)>> for TraitBoundsVar {
    fn extend<T: IntoIterator<Item = Vec<(Ident, Vec<TypeVar>)>>>(&mut self, iter: T) {
        iter.into_iter().for_each(|item| {
            self.paths.insert(item);
        });
    }
}

// impl ToTokens for TypePathBuf {
//     fn to_tokens(&self, tokens: &mut TokenStream) {
//         let Self { segments } = self;

//         let segment_tokens: Vec<_> = segments
//             .iter()
//             .map(|(ident, generic_args)| {
//                 if generic_args.is_empty() {
//                     quote! {
//                         #ident
//                     }
//                 } else {
//                     quote! {
//                         #ident<#(#generic_args),*>
//                     }
//                 }
//             })
//             .collect();

//         let expanded = quote! {
//             #(#segment_tokens)::*
//         };

//         tokens.extend(expanded);
//     }
// }
