use crate::{
    common::*,
    scope::{ScopeSet, Variable},
    utils::Shared,
};

pub trait ParseTypeVar {
    fn parse_type_var(&self, scope: &ScopeSet) -> syn::Result<TypeVar>;
}

impl ParseTypeVar for Type {
    fn parse_type_var(&self, scope: &ScopeSet) -> syn::Result<TypeVar> {
        let var = match self {
            Type::Path(TypePath { qself, path }) => match (qself, path.get_ident()) {
                (Some(QSelf { ty, position, .. }), _) => TypeVar::Path(TypePathVar {
                    qself: Some(QSelfVar {
                        ty: Box::new(ty.parse_type_var(scope)?),
                        position: *position,
                    }),
                    path: path.parse_path_var(scope)?,
                }),
                (None, Some(ident)) => match scope.get_quantifier(ident) {
                    Some(var) => TypeVar::Var(var),
                    None => TypeVar::Path(TypePathVar {
                        qself: None,
                        path: path.parse_path_var(scope)?,
                    }),
                },
                (None, None) => TypeVar::Path(TypePathVar {
                    qself: None,
                    path: path.parse_path_var(scope)?,
                }),
            },
            Type::Tuple(TypeTuple { elems, .. }) => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|elem| elem.parse_type_var(scope))
                    .try_collect()?;
                TypeVar::Tuple(TypeTupleVar { elems })
            }
            Type::Paren(TypeParen { elem, .. }) => elem.parse_type_var(scope)?,
            _ => return Err(Error::new(self.span(), "unsupported type variant")),
        };
        Ok(var)
    }
}

pub trait ParsePathVar {
    fn parse_path_var(&self, scope: &ScopeSet) -> syn::Result<PathVar>;
}

impl ParsePathVar for Path {
    fn parse_path_var(&self, scope: &ScopeSet) -> syn::Result<PathVar> {
        let Path { segments, .. } = self;
        let segments: Vec<_> = segments
            .iter()
            .map(|segment| segment.parse_segment_var(scope))
            .try_collect()?;
        Ok(PathVar { segments })
    }
}

pub trait ParseSegmentVar {
    fn parse_segment_var(&self, scope: &ScopeSet) -> syn::Result<SegmentVar>;
}

impl ParseSegmentVar for PathSegment {
    fn parse_segment_var(&self, scope: &ScopeSet) -> syn::Result<SegmentVar> {
        let PathSegment { ident, arguments } = self;
        let arguments = match arguments {
            PathArguments::None => PathArgumentsVar::None,
            PathArguments::AngleBracketed(args) => {
                let args: Vec<_> = args
                    .args
                    .iter()
                    .map(|arg| match arg {
                        GenericArgument::Type(ty) => ty.parse_type_var(scope),
                        _ => Err(Error::new(arg.span(), "unsupported generic variant")),
                    })
                    .try_collect()?;
                PathArgumentsVar::AngleBracketed(args)
            }
            PathArguments::Parenthesized(args) => {
                let inputs: Vec<_> = args
                    .inputs
                    .iter()
                    .map(|ty| ty.parse_type_var(scope))
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

pub trait ParseTraitBoundVar {
    fn parse_trait_bound_var(&self, scope: &ScopeSet) -> syn::Result<TraitBoundVar>;
}

impl ParseTraitBoundVar for TraitBound {
    fn parse_trait_bound_var(&self, scope: &ScopeSet) -> syn::Result<TraitBoundVar> {
        let TraitBound { modifier, path, .. } = self;
        Ok(TraitBoundVar {
            modifier: modifier.to_owned(),
            path: path.parse_path_var(scope)?,
        })
    }
}

pub trait ParsePredicateTypeVar {
    fn parse_predicate_type_var(&self, scope: &ScopeSet) -> syn::Result<PredicateTypeVar>;
}

impl ParsePredicateTypeVar for PredicateType {
    fn parse_predicate_type_var(&self, scope: &ScopeSet) -> syn::Result<PredicateTypeVar> {
        let PredicateType {
            lifetimes,
            bounded_ty,
            bounds,
            ..
        } = self;

        if let Some(life) = lifetimes {
            return Err(Error::new(life.span(), "bounded lifetime is not supported"));
        }

        let bounded_ty = bounded_ty.parse_type_var(scope)?;
        let bounds: Vec<_> = bounds
            .iter()
            .map(|bound| match bound {
                TypeParamBound::Trait(trait_) => Ok(TypeParamBoundVar {
                    trait_: trait_.parse_trait_bound_var(scope)?,
                }),
                TypeParamBound::Lifetime(lifetime) => {
                    Err(Error::new(lifetime.span(), "lifetime is not supported"))
                }
            })
            .try_collect()?;

        Ok(PredicateTypeVar { bounded_ty, bounds })
    }
}

pub trait ParseWherePredicateVar {
    fn parse_where_predicate_var(&self, scope: &ScopeSet) -> syn::Result<WherePredicateVar>;
}

impl ParseWherePredicateVar for WherePredicate {
    fn parse_where_predicate_var(&self, scope: &ScopeSet) -> syn::Result<WherePredicateVar> {
        match self {
            WherePredicate::Type(ty) => Ok(WherePredicateVar {
                ty: ty.parse_predicate_type_var(scope)?,
            }),
            WherePredicate::Lifetime(lifetime) => {
                Err(Error::new(lifetime.span(), "lifetime is not supported"))
            }
            WherePredicate::Eq(eq) => {
                Err(Error::new(eq.span(), "equality predidate is not supported"))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeVar {
    Var(Shared<Variable>),
    Path(TypePathVar),
    Tuple(TypeTupleVar),
}

#[derive(Debug, Clone)]
pub struct SegmentVar {
    pub ident: Ident,
    pub arguments: PathArgumentsVar,
}

#[derive(Debug, Clone)]
pub enum PathArgumentsVar {
    None,
    AngleBracketed(Vec<TypeVar>),
    Parenthesized(Vec<TypeVar>),
}

#[derive(Debug, Clone)]
pub struct GenericArgumentVar {
    pub ty: TypeVar,
}

#[derive(Debug, Clone)]
pub struct PathVar {
    pub segments: Vec<SegmentVar>,
}

#[derive(Debug, Clone)]
pub struct QSelfVar {
    pub ty: Box<TypeVar>,
    pub position: usize,
}

#[derive(Debug, Clone)]
pub struct TypePathVar {
    pub qself: Option<QSelfVar>,
    pub path: PathVar,
}

#[derive(Debug, Clone)]
pub struct TypeTupleVar {
    pub elems: Vec<TypeVar>,
}

#[derive(Debug, Clone)]
pub struct TraitBoundVar {
    pub modifier: TraitBoundModifier,
    pub path: PathVar,
}

#[derive(Debug, Clone)]
pub struct WherePredicateVar {
    ty: PredicateTypeVar,
}

#[derive(Debug, Clone)]
pub struct PredicateTypeVar {
    pub bounded_ty: TypeVar,
    pub bounds: Vec<TypeParamBoundVar>,
}

#[derive(Debug, Clone)]
pub struct TypeParamBoundVar {
    pub trait_: TraitBoundVar,
}
