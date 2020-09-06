use crate::{
    common::*,
    env::{Env, Variable},
    utils::{Shared, VecTranspose},
};

// parse without substitutions

pub trait ParsePureType {
    fn parse_pure_type(&self) -> syn::Result<TypeVar>;
}

impl ParsePureType for Ident {
    fn parse_pure_type(&self) -> syn::Result<TypeVar> {
        Ok(TypeVar::Path(TypePathVar {
            qself: None,
            path: self.parse_pure_path()?,
        }))
    }
}

impl ParsePureType for Type {
    fn parse_pure_type(&self) -> syn::Result<TypeVar> {
        let var = match self {
            Type::Path(TypePath { qself, path }) => match qself {
                Some(QSelf { ty, position, .. }) => TypeVar::Path(TypePathVar {
                    qself: Some(QSelfVar {
                        ty: Box::new(ty.parse_pure_type()?),
                        position: *position,
                    }),
                    path: path.parse_pure_path()?,
                }),
                None => TypeVar::Path(TypePathVar {
                    qself: None,
                    path: path.parse_pure_path()?,
                }),
            },
            Type::Tuple(TypeTuple { elems, .. }) => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|elem| elem.parse_pure_type())
                    .try_collect()?;
                TypeVar::Tuple(TypeTupleVar { elems })
            }
            Type::Paren(TypeParen { elem, .. }) => elem.parse_pure_type()?,
            _ => return Err(Error::new(self.span(), "unsupported type variant")),
        };
        Ok(var)
    }
}

pub trait ParsePurePath {
    fn parse_pure_path(&self) -> syn::Result<PathVar>;
}

impl ParsePurePath for Ident {
    fn parse_pure_path(&self) -> syn::Result<PathVar> {
        let segments = vec![self.parse_pure_segment()?];
        Ok(PathVar { segments })
    }
}

impl ParsePurePath for Path {
    fn parse_pure_path(&self) -> syn::Result<PathVar> {
        let Path { segments, .. } = self;
        let segments: Vec<_> = segments
            .iter()
            .map(|segment| segment.parse_pure_segment())
            .try_collect()?;
        Ok(PathVar { segments })
    }
}

pub trait ParsePureSegment {
    fn parse_pure_segment(&self) -> syn::Result<SegmentVar>;
}

impl ParsePureSegment for Ident {
    fn parse_pure_segment(&self) -> syn::Result<SegmentVar> {
        Ok(SegmentVar {
            ident: self.to_owned(),
            arguments: PathArgumentsVar::None,
        })
    }
}

impl ParsePureSegment for PathSegment {
    fn parse_pure_segment(&self) -> syn::Result<SegmentVar> {
        let PathSegment { ident, arguments } = self;
        let arguments = match arguments {
            PathArguments::None => PathArgumentsVar::None,
            PathArguments::AngleBracketed(args) => {
                let args: Vec<_> = args
                    .args
                    .iter()
                    .map(|arg| match arg {
                        GenericArgument::Type(ty) => ty.parse_pure_type(),
                        _ => Err(Error::new(arg.span(), "unsupported generic variant")),
                    })
                    .try_collect()?;
                PathArgumentsVar::AngleBracketed(args)
            }
            PathArguments::Parenthesized(args) => {
                let inputs: Vec<_> = args
                    .inputs
                    .iter()
                    .map(|ty| ty.parse_pure_type())
                    .try_collect()?;
                PathArgumentsVar::Parenthesized(inputs)
            }
        };

        Ok(SegmentVar {
            ident: ident.to_owned(),
            arguments,
        })
    }
}

pub trait ParsePureWherePredicate {
    fn parse_pure_where_predicate(&self) -> syn::Result<WherePredicateVar>;
}

impl ParsePureWherePredicate for GenericParam {
    fn parse_pure_where_predicate(&self) -> syn::Result<WherePredicateVar> {
        match self {
            GenericParam::Type(TypeParam { ident, bounds, .. }) => {
                Ok(WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty: ident.parse_pure_type()?,
                    bounds: bounds
                        .iter()
                        .map(|bound| bound.parse_pure_type_param_bound())
                        .try_collect()?,
                }))
            }
            GenericParam::Lifetime(_) => {
                return Err(Error::new(self.span(), "lifetime is not supported"));
            }
            GenericParam::Const(_) => {
                return Err(Error::new(self.span(), "const generics is not supported"));
            }
        }
    }
}

impl ParsePureWherePredicate for WherePredicate {
    fn parse_pure_where_predicate(&self) -> syn::Result<WherePredicateVar> {
        match self {
            WherePredicate::Type(PredicateType {
                bounded_ty, bounds, ..
            }) => Ok(WherePredicateVar::Type(PredicateTypeVar {
                bounded_ty: bounded_ty.parse_pure_type()?,
                bounds: bounds
                    .iter()
                    .map(|bound| bound.parse_pure_type_param_bound())
                    .try_collect()?,
            })),
            WherePredicate::Lifetime(_) => {
                return Err(Error::new(self.span(), "lifetime bound is not supported"));
            }
            WherePredicate::Eq(_) => {
                return Err(Error::new(self.span(), "binding is not supported"));
            }
        }
    }
}

pub trait ParsePureTypeParamBound {
    fn parse_pure_type_param_bound(&self) -> syn::Result<TypeParamBoundVar>;
}

impl ParsePureTypeParamBound for TypeParamBound {
    fn parse_pure_type_param_bound(&self) -> syn::Result<TypeParamBoundVar> {
        match self {
            TypeParamBound::Trait(bound) => {
                Ok(TypeParamBoundVar::Trait(bound.parse_pure_trait_bound()?))
            }
            TypeParamBound::Lifetime(_) => {
                Err(Error::new(self.span(), "lifetime bound is not supported"))
            }
        }
    }
}

pub trait ParsePureTraitBound {
    fn parse_pure_trait_bound(&self) -> syn::Result<TraitBoundVar>;
}

impl ParsePureTraitBound for TraitBound {
    fn parse_pure_trait_bound(&self) -> syn::Result<TraitBoundVar> {
        let TraitBound { modifier, path, .. } = self;
        Ok(TraitBoundVar {
            modifier: modifier.into(),
            path: path.parse_pure_path()?,
        })
    }
}

// parse with substitutions

pub trait ParseTypeVar {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<Vec<TypeVar>>;
}

impl ParseTypeVar for Type {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<Vec<TypeVar>> {
        let var: Vec<_> = match self {
            Type::Path(TypePath { qself, path }) => match (qself, path.get_ident()) {
                (Some(QSelf { ty, position, .. }), _) => {
                    let tys = ty.parse_type_var(scope)?;
                    let paths = path.parse_path_var(scope)?;
                    tys.into_iter()
                        .zip_eq(paths)
                        .map(|(ty, path)| {
                            TypeVar::Path(TypePathVar {
                                qself: Some(QSelfVar {
                                    ty: Box::new(ty),
                                    position: *position,
                                }),
                                path,
                            })
                        })
                        .collect()
                }
                (None, Some(ident)) => match scope.get_variable(ident) {
                    Some(vars) => vars.into_iter().map(|var| TypeVar::Var(var)).collect(),
                    None => {
                        let paths = path.parse_path_var(scope)?;
                        paths
                            .into_iter()
                            .map(|path| TypeVar::Path(TypePathVar { qself: None, path }))
                            .collect()
                    }
                },
                (None, None) => {
                    let paths = path.parse_path_var(scope)?;
                    paths
                        .into_iter()
                        .map(|path| TypeVar::Path(TypePathVar { qself: None, path }))
                        .collect()
                }
            },
            Type::Tuple(TypeTuple { elems, .. }) => {
                let elems_per_branch: Vec<_> = elems
                    .iter()
                    .map(|elem| elem.parse_type_var(scope))
                    .try_collect::<_, Vec<_>, _>()?
                    .transpose_inplace();

                elems_per_branch
                    .into_iter()
                    .map(|elems| TypeVar::Tuple(TypeTupleVar { elems }))
                    .collect()
            }
            Type::Paren(TypeParen { elem, .. }) => elem.parse_type_var(scope)?,
            _ => return Err(Error::new(self.span(), "unsupported type variant")),
        };
        Ok(var)
    }
}

impl ParseTypeVar for Pat {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<Vec<TypeVar>> {
        match self {
            Pat::Path(PatPath { qself, path, .. }) => {
                let qselfs = match qself {
                    Some(QSelf { ty, position, .. }) => {
                        let tys = ty.parse_type_var(scope)?;
                        let qselfs: Vec<_> = tys
                            .into_iter()
                            .map(|ty| {
                                Some(QSelfVar {
                                    ty: Box::new(ty),
                                    position: *position,
                                })
                            })
                            .collect();
                        qselfs
                    }
                    None => vec![None; scope.num_branches()],
                };
                let paths = path.parse_path_var(scope)?;
                let tys: Vec<_> = qselfs
                    .into_iter()
                    .zip_eq(paths)
                    .map(|(qself, path)| TypeVar::Path(TypePathVar { qself, path }))
                    .collect();

                Ok(tys)
            }
            Pat::Tuple(PatTuple { elems, .. }) => {
                let tys: Vec<_> = elems
                    .iter()
                    .map(|elem| elem.parse_type_var(scope))
                    .try_collect::<_, Vec<_>, _>()?
                    .transpose_inplace()
                    .into_iter()
                    .map(|elems| TypeVar::Tuple(TypeTupleVar { elems }))
                    .collect();
                Ok(tys)
            }
            _ => Err(Error::new(self.span(), "not a type")),
        }
    }
}

impl ParseTypeVar for Ident {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<Vec<TypeVar>> {
        let vars: Vec<_> = match scope.get_variable(self) {
            Some(vars) => vars.into_iter().map(|var| TypeVar::Var(var)).collect(),
            None => {
                let var = TypeVar::Path(TypePathVar {
                    qself: None,
                    path: PathVar {
                        segments: vec![SegmentVar {
                            ident: self.to_owned(),
                            arguments: PathArgumentsVar::None,
                        }],
                    },
                });
                let num_branches = scope.num_branches();
                (0..num_branches).map(|_| var.clone()).collect()
            }
        };
        Ok(vars)
    }
}

impl ParseTypeVar for ExprPath {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<Vec<TypeVar>> {
        let ExprPath { qself, path, .. } = self;
        let vars: Vec<_> = match (qself, path.get_ident()) {
            (Some(QSelf { ty, position, .. }), _) => {
                let tys = ty.parse_type_var(scope)?;
                let paths = path.parse_path_var(scope)?;
                tys.into_iter()
                    .zip_eq(paths)
                    .map(|(ty, path)| {
                        TypeVar::Path(TypePathVar {
                            qself: Some(QSelfVar {
                                ty: Box::new(ty),
                                position: *position,
                            }),
                            path,
                        })
                    })
                    .collect()
            }
            (None, Some(ident)) => match scope.get_variable(ident) {
                Some(vars) => vars.into_iter().map(|var| TypeVar::Var(var)).collect(),
                None => {
                    let paths = path.parse_path_var(scope)?;
                    paths
                        .into_iter()
                        .map(|path| TypeVar::Path(TypePathVar { qself: None, path }))
                        .collect()
                }
            },
            (None, None) => {
                let paths = path.parse_path_var(scope)?;
                paths
                    .into_iter()
                    .map(|path| TypeVar::Path(TypePathVar { qself: None, path }))
                    .collect()
            }
        };
        Ok(vars)
    }
}

pub trait ParsePathVar {
    fn parse_path_var(&self, scope: &Env) -> syn::Result<Vec<PathVar>>;
}

impl ParsePathVar for Path {
    fn parse_path_var(&self, scope: &Env) -> syn::Result<Vec<PathVar>> {
        let Path { segments, .. } = self;
        let vars: Vec<_> = segments
            .iter()
            .map(|segment| segment.parse_segment_var(scope))
            .try_collect::<_, Vec<_>, _>()?
            .transpose_inplace()
            .into_iter()
            .map(|segments| PathVar { segments })
            .collect();
        Ok(vars)
    }
}

pub trait ParseSegmentVar {
    fn parse_segment_var(&self, scope: &Env) -> syn::Result<Vec<SegmentVar>>;
}

impl ParseSegmentVar for PathSegment {
    fn parse_segment_var(&self, scope: &Env) -> syn::Result<Vec<SegmentVar>> {
        let PathSegment { ident, arguments } = self;
        let arguments_vec: Vec<_> = match arguments {
            PathArguments::None => vec![PathArgumentsVar::None; scope.num_branches()],
            PathArguments::AngleBracketed(args) => args
                .args
                .iter()
                .map(|arg| match arg {
                    GenericArgument::Type(ty) => ty.parse_type_var(scope),
                    _ => Err(Error::new(arg.span(), "unsupported generic variant")),
                })
                .try_collect::<_, Vec<_>, _>()?
                .transpose_inplace()
                .into_iter()
                .map(|args| PathArgumentsVar::AngleBracketed(args))
                .collect(),
            PathArguments::Parenthesized(args) => args
                .inputs
                .iter()
                .map(|ty| ty.parse_type_var(scope))
                .try_collect::<_, Vec<_>, _>()?
                .transpose_inplace()
                .into_iter()
                .map(|inputs| PathArgumentsVar::Parenthesized(inputs))
                .collect(),
        };
        let vars: Vec<_> = arguments_vec
            .into_iter()
            .map(|arguments| SegmentVar {
                ident: ident.to_owned(),
                arguments,
            })
            .collect();

        Ok(vars)
    }
}

pub trait ParseTraitBoundVar {
    fn parse_trait_bound_var(&self, scope: &Env) -> syn::Result<Vec<TraitBoundVar>>;
}

impl ParseTraitBoundVar for TraitBound {
    fn parse_trait_bound_var(&self, scope: &Env) -> syn::Result<Vec<TraitBoundVar>> {
        let TraitBound { modifier, path, .. } = self;
        let vars: Vec<_> = path
            .parse_path_var(scope)?
            .into_iter()
            .map(|path| TraitBoundVar {
                modifier: modifier.into(),
                path,
            })
            .collect();
        Ok(vars)
    }
}

pub trait ParsePredicateTypeVar {
    fn parse_predicate_type_var(&self, scope: &Env) -> syn::Result<Vec<PredicateTypeVar>>;
}

impl ParsePredicateTypeVar for PredicateType {
    fn parse_predicate_type_var(&self, scope: &Env) -> syn::Result<Vec<PredicateTypeVar>> {
        let PredicateType {
            lifetimes,
            bounded_ty,
            bounds,
            ..
        } = self;

        if let Some(life) = lifetimes {
            return Err(Error::new(life.span(), "bounded lifetime is not supported"));
        }

        let bounded_tys = bounded_ty.parse_type_var(scope)?;
        let bounds_vec: Vec<_> = bounds
            .iter()
            .map(|bound| match bound {
                TypeParamBound::Trait(trait_) => {
                    let bounds: Vec<_> = trait_
                        .parse_trait_bound_var(scope)?
                        .into_iter()
                        .map(|trait_bounds| TypeParamBoundVar::Trait(trait_bounds))
                        .collect();
                    Ok(bounds)
                }
                TypeParamBound::Lifetime(lifetime) => {
                    Err(Error::new(lifetime.span(), "lifetime is not supported"))
                }
            })
            .try_collect::<_, Vec<_>, _>()?
            .transpose_inplace();

        let vars: Vec<_> = bounded_tys
            .into_iter()
            .zip_eq(bounds_vec)
            .map(|(bounded_ty, bounds)| PredicateTypeVar { bounded_ty, bounds })
            .collect();
        Ok(vars)
    }
}

pub trait ParseWherePredicateVar {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<Vec<WherePredicateVar>>;
}

impl ParseWherePredicateVar for WherePredicate {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<Vec<WherePredicateVar>> {
        match self {
            WherePredicate::Type(ty) => {
                let predicates = ty.parse_predicate_type_var(scope)?;
                let vars: Vec<_> = predicates
                    .into_iter()
                    .map(|predicate| WherePredicateVar::Type(predicate))
                    .collect();
                Ok(vars)
            }
            WherePredicate::Lifetime(lifetime) => {
                Err(Error::new(lifetime.span(), "lifetime is not supported"))
            }
            WherePredicate::Eq(eq) => {
                Err(Error::new(eq.span(), "equality predidate is not supported"))
            }
        }
    }
}

impl ParseWherePredicateVar for GenericParam {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<Vec<WherePredicateVar>> {
        match self {
            GenericParam::Type(TypeParam { ident, bounds, .. }) => {
                let bounded_ty_vec = ident.parse_type_var(scope)?;
                let bounds_vec = bounds
                    .iter()
                    .map(|bound| bound.parse_type_param_bound_var(scope))
                    .try_collect::<_, Vec<_>, _>()?
                    .transpose_inplace();

                let vars: Vec<_> = bounded_ty_vec
                    .into_iter()
                    .zip_eq(bounds_vec)
                    .map(|(bounded_ty, bounds)| {
                        WherePredicateVar::Type(PredicateTypeVar { bounded_ty, bounds })
                    })
                    .collect();

                Ok(vars)
            }
            GenericParam::Lifetime(_) => {
                return Err(Error::new(self.span(), "lifetime is not supported"));
            }
            GenericParam::Const(_) => {
                return Err(Error::new(self.span(), "const generics is not supported"));
            }
        }
    }
}

impl ParseWherePredicateVar for PatType {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<Vec<WherePredicateVar>> {
        let PatType { pat, ty, .. } = self;
        let bounded_ty_vec = pat.parse_type_var(scope)?;
        let bounds_vec = ty.parse_type_param_bounds_var(scope)?;
        let vars: Vec<_> = bounded_ty_vec
            .into_iter()
            .zip_eq(bounds_vec)
            .map(|(bounded_ty, bounds)| {
                WherePredicateVar::Type(PredicateTypeVar { bounded_ty, bounds })
            })
            .collect();
        Ok(vars)
    }
}

pub trait ParseTypeParamBoundVar {
    fn parse_type_param_bound_var(&self, scope: &Env) -> syn::Result<Vec<TypeParamBoundVar>>;
}

impl ParseTypeParamBoundVar for TypeParamBound {
    fn parse_type_param_bound_var(&self, scope: &Env) -> syn::Result<Vec<TypeParamBoundVar>> {
        match self {
            TypeParamBound::Trait(bound) => {
                let bounds = bound.parse_trait_bound_var(scope)?;
                let vars: Vec<_> = bounds
                    .into_iter()
                    .map(|bound| TypeParamBoundVar::Trait(bound))
                    .collect();
                Ok(vars)
            }

            TypeParamBound::Lifetime(_) => {
                Err(Error::new(self.span(), "lifetime is not supported"))
            }
        }
    }
}

pub trait ParseTypeParamBoundsVar {
    fn parse_type_param_bounds_var(&self, scope: &Env) -> syn::Result<Vec<Vec<TypeParamBoundVar>>>;
}

impl ParseTypeParamBoundsVar for Type {
    fn parse_type_param_bounds_var(&self, scope: &Env) -> syn::Result<Vec<Vec<TypeParamBoundVar>>> {
        match self {
            Type::Infer(_) => Ok(vec![vec![]; scope.num_branches()]),
            Type::Path(TypePath {
                qself: None, path, ..
            }) => {
                let paths = path.parse_path_var(scope)?;
                let vars: Vec<_> = paths
                    .into_iter()
                    .map(|path| {
                        vec![TypeParamBoundVar::Trait(TraitBoundVar {
                            modifier: TraitBoundModifierVar::None,
                            path,
                        })]
                    })
                    .collect();
                Ok(vars)
            }
            Type::TraitObject(TypeTraitObject {
                bounds, dyn_token, ..
            }) => {
                if let Some(token) = dyn_token {
                    return Err(Error::new(token.span(), "remove the dyn token"));
                }
                let bounds = bounds
                    .iter()
                    .map(|bound| bound.parse_type_param_bound_var(scope))
                    .try_collect::<_, Vec<_>, _>()?
                    .transpose_inplace();
                Ok(bounds)
            }
            _ => Err(Error::new(self.span(), "not trait bounds")),
        }
    }
}

// types

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Var {
    Type(TypeVar),
    Path(PathVar),
}

impl Var {
    pub fn into_type(self) -> Option<TypeVar> {
        match self {
            Self::Type(ty) => Some(ty),
            _ => None,
        }
    }
}

impl Var {
    pub fn into_path(self) -> Option<PathVar> {
        match self {
            Self::Path(path) => Some(path),
            _ => None,
        }
    }
}

impl From<TypeVar> for Var {
    fn from(from: TypeVar) -> Self {
        Self::Type(from)
    }
}

impl From<PathVar> for Var {
    fn from(from: PathVar) -> Self {
        Self::Path(from)
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TypeVar {
    Var(Shared<Variable>),
    Path(TypePathVar),
    Tuple(TypeTupleVar),
}

impl Parse for TypeVar {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Type = input.parse()?;
        let ty = ty.parse_pure_type()?;
        Ok(ty)
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct SegmentVar {
    pub ident: Ident,
    pub arguments: PathArgumentsVar,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum PathArgumentsVar {
    None,
    AngleBracketed(Vec<TypeVar>),
    Parenthesized(Vec<TypeVar>),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct GenericArgumentVar {
    pub ty: TypeVar,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct PathVar {
    pub segments: Vec<SegmentVar>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct QSelfVar {
    pub ty: Box<TypeVar>,
    pub position: usize,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct TypePathVar {
    pub qself: Option<QSelfVar>,
    pub path: PathVar,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct TypeTupleVar {
    pub elems: Vec<TypeVar>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct TraitBoundVar {
    pub modifier: TraitBoundModifierVar,
    pub path: PathVar,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TraitBoundModifierVar {
    None,
    Maybe,
}

impl From<TraitBoundModifier> for TraitBoundModifierVar {
    fn from(from: TraitBoundModifier) -> Self {
        (&from).into()
    }
}

impl From<&TraitBoundModifier> for TraitBoundModifierVar {
    fn from(from: &TraitBoundModifier) -> Self {
        match from {
            TraitBoundModifier::None => Self::None,
            TraitBoundModifier::Maybe(_) => Self::Maybe,
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum WherePredicateVar {
    Type(PredicateTypeVar),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct PredicateTypeVar {
    pub bounded_ty: TypeVar,
    pub bounds: Vec<TypeParamBoundVar>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TypeParamBoundVar {
    Trait(TraitBoundVar),
}
