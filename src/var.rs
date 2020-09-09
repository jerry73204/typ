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
    fn parse_type_var(&self, scope: &Env) -> syn::Result<TypeVar>;
}

impl ParseTypeVar for Type {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<TypeVar> {
        let ty = match self {
            Type::Path(TypePath { qself, path }) => match (qself, path.get_ident()) {
                (Some(QSelf { ty, position, .. }), _) => {
                    let ty = ty.parse_type_var(scope)?;
                    let path = path.parse_path_var(scope)?;
                    TypeVar::Path(TypePathVar {
                        qself: Some(QSelfVar {
                            ty: Box::new(ty),
                            position: *position,
                        }),
                        path,
                    })
                }
                (None, Some(ident)) => match scope.get_variable(ident) {
                    Some(var) => TypeVar::Var(var),
                    None => {
                        let path = path.parse_path_var(scope)?;
                        TypeVar::Path(TypePathVar { qself: None, path })
                    }
                },
                (None, None) => {
                    let path = path.parse_path_var(scope)?;
                    TypeVar::Path(TypePathVar { qself: None, path })
                }
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
        Ok(ty)
    }
}

impl ParseTypeVar for Pat {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<TypeVar> {
        match self {
            Pat::Path(PatPath { qself, path, .. }) => {
                let qself = match qself {
                    Some(QSelf { ty, position, .. }) => {
                        let ty = ty.parse_type_var(scope)?;
                        Some(QSelfVar {
                            ty: Box::new(ty),
                            position: *position,
                        })
                    }
                    None => None,
                };
                let path = path.parse_path_var(scope)?;
                Ok(TypeVar::Path(TypePathVar { qself, path }))
            }
            Pat::Tuple(PatTuple { elems, .. }) => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|elem| elem.parse_type_var(scope))
                    .try_collect()?;
                Ok(TypeVar::Tuple(TypeTupleVar { elems }))
            }
            _ => Err(Error::new(self.span(), "not a type")),
        }
    }
}

impl ParseTypeVar for Ident {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<TypeVar> {
        match scope.get_variable(self) {
            Some(var) => Ok(TypeVar::Var(var)),
            None => Ok(TypeVar::Path(TypePathVar {
                qself: None,
                path: PathVar {
                    segments: vec![SegmentVar {
                        ident: self.to_owned(),
                        arguments: PathArgumentsVar::None,
                    }],
                },
            })),
        }
    }
}

impl ParseTypeVar for ExprPath {
    fn parse_type_var(&self, scope: &Env) -> syn::Result<TypeVar> {
        let ExprPath { qself, path, .. } = self;
        let var = match (qself, path.get_ident()) {
            (Some(QSelf { ty, position, .. }), _) => {
                let ty = ty.parse_type_var(scope)?;
                let path = path.parse_path_var(scope)?;
                TypeVar::Path(TypePathVar {
                    qself: Some(QSelfVar {
                        ty: Box::new(ty),
                        position: *position,
                    }),
                    path,
                })
            }
            (None, Some(ident)) => match scope.get_variable(ident) {
                Some(var) => TypeVar::Var(var),
                None => {
                    let path = path.parse_path_var(scope)?;
                    TypeVar::Path(TypePathVar { qself: None, path })
                }
            },
            (None, None) => {
                let path = path.parse_path_var(scope)?;
                TypeVar::Path(TypePathVar { qself: None, path })
            }
        };
        Ok(var)
    }
}

pub trait ParsePathVar {
    fn parse_path_var(&self, scope: &Env) -> syn::Result<PathVar>;
}

impl ParsePathVar for Path {
    fn parse_path_var(&self, scope: &Env) -> syn::Result<PathVar> {
        let Path { segments, .. } = self;
        let segments: Vec<_> = segments
            .iter()
            .map(|segment| segment.parse_segment_var(scope))
            .try_collect()?;
        Ok(PathVar { segments })
    }
}

pub trait ParseSegmentVar {
    fn parse_segment_var(&self, scope: &Env) -> syn::Result<SegmentVar>;
}

impl ParseSegmentVar for PathSegment {
    fn parse_segment_var(&self, scope: &Env) -> syn::Result<SegmentVar> {
        let PathSegment { ident, arguments } = self;
        let arguments = match arguments {
            PathArguments::None => PathArgumentsVar::None,
            PathArguments::AngleBracketed(args) => {
                let args = args
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
                let inputs = args
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
    fn parse_trait_bound_var(&self, scope: &Env) -> syn::Result<TraitBoundVar>;
}

impl ParseTraitBoundVar for TraitBound {
    fn parse_trait_bound_var(&self, scope: &Env) -> syn::Result<TraitBoundVar> {
        let TraitBound { modifier, path, .. } = self;
        let path = path.parse_path_var(scope)?;
        Ok(TraitBoundVar {
            modifier: modifier.into(),
            path,
        })
    }
}

pub trait ParsePredicateTypeVar {
    fn parse_predicate_type_var(&self, scope: &Env) -> syn::Result<PredicateTypeVar>;
}

impl ParsePredicateTypeVar for PredicateType {
    fn parse_predicate_type_var(&self, scope: &Env) -> syn::Result<PredicateTypeVar> {
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
                TypeParamBound::Trait(trait_) => {
                    let bounds = trait_.parse_trait_bound_var(scope)?;
                    Ok(TypeParamBoundVar::Trait(bounds))
                }
                TypeParamBound::Lifetime(lifetime) => {
                    Err(Error::new(lifetime.span(), "lifetime is not supported"))
                }
            })
            .try_collect()?;

        Ok(PredicateTypeVar { bounded_ty, bounds })
    }
}

pub trait ParseWherePredicateVar {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<WherePredicateVar>;
}

impl ParseWherePredicateVar for WherePredicate {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<WherePredicateVar> {
        match self {
            WherePredicate::Type(ty) => {
                let predicate = ty.parse_predicate_type_var(scope)?;
                Ok(WherePredicateVar::Type(predicate))
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
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<WherePredicateVar> {
        match self {
            GenericParam::Type(TypeParam { ident, bounds, .. }) => {
                let bounded_ty = ident.parse_type_var(scope)?;
                let bounds = bounds
                    .iter()
                    .map(|bound| bound.parse_type_param_bound_var(scope))
                    .try_collect()?;

                Ok(WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty,
                    bounds,
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

impl ParseWherePredicateVar for PatType {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<WherePredicateVar> {
        let PatType { pat, ty, .. } = self;
        let bounded_ty = pat.parse_type_var(scope)?;
        let bounds = ty.parse_type_param_bounds_var(scope)?;
        Ok(WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty,
            bounds,
        }))
    }
}

pub trait ParseTypeParamBoundVar {
    fn parse_type_param_bound_var(&self, scope: &Env) -> syn::Result<TypeParamBoundVar>;
}

impl ParseTypeParamBoundVar for TypeParamBound {
    fn parse_type_param_bound_var(&self, scope: &Env) -> syn::Result<TypeParamBoundVar> {
        match self {
            TypeParamBound::Trait(bound) => {
                let bounds = bound.parse_trait_bound_var(scope)?;
                Ok(TypeParamBoundVar::Trait(bounds))
            }

            TypeParamBound::Lifetime(_) => {
                Err(Error::new(self.span(), "lifetime is not supported"))
            }
        }
    }
}

impl ParseTypeParamBoundVar for TraitBound {
    fn parse_type_param_bound_var(&self, scope: &Env) -> syn::Result<TypeParamBoundVar> {
        let bounds = self.parse_trait_bound_var(scope)?;
        Ok(TypeParamBoundVar::Trait(bounds))
    }
}

pub trait ParseTypeParamBoundsVar {
    fn parse_type_param_bounds_var(&self, scope: &Env) -> syn::Result<Vec<TypeParamBoundVar>>;
}

impl ParseTypeParamBoundsVar for Type {
    fn parse_type_param_bounds_var(&self, scope: &Env) -> syn::Result<Vec<TypeParamBoundVar>> {
        match self {
            Type::Infer(_) => Ok(vec![]),
            Type::Path(TypePath {
                qself: None, path, ..
            }) => {
                let path = path.parse_path_var(scope)?;
                Ok(vec![TypeParamBoundVar::Trait(TraitBoundVar {
                    modifier: TraitBoundModifierVar::None,
                    path,
                })])
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
                    .try_collect()?;
                Ok(bounds)
            }
            _ => Err(Error::new(self.span(), "not trait bounds")),
        }
    }
}

// substitution

pub trait Subsitution {
    type Output;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output;
}

impl Subsitution for TypeVar {
    type Output = Type;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        match self {
            TypeVar::Var(var) => var.substitute(env, substitution),
            TypeVar::Path(path) => Type::Path(path.substitute(env, substitution)),
            TypeVar::Tuple(tuple) => Type::Tuple(tuple.substitute(env, substitution)),
        }
    }
}

impl Subsitution for Shared<Variable> {
    type Output = Type;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        match &self.value {
            Some(value) => value.substitute(env, substitution),
            None => {
                let ident = substitution
                    .get(self)
                    .expect("the subsitution list is incomplete");
                syn::parse2(quote! { #ident }).unwrap()
            }
        }
    }
}

impl Subsitution for TypePathVar {
    type Output = TypePath;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        let TypePathVar { qself, path } = self;
        let path = path.substitute(env, substitution);

        match *qself {
            Some(QSelfVar { ref ty, position }) => {
                let ty = ty.substitute(env, substitution);
                let trait_: Vec<_> = path.segments.iter().take(position).collect();
                let associated_type: Vec<_> = path.segments.iter().skip(position).collect();
                syn::parse2(quote! { < #ty as #(#trait_)::* > #(::#associated_type)* }).unwrap()
            }
            None => syn::parse2(quote! { #path }).unwrap(),
        }
    }
}

impl Subsitution for TypeTupleVar {
    type Output = TypeTuple;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        let elems: Vec<_> = self
            .elems
            .iter()
            .map(|elem| elem.substitute(env, substitution))
            .collect();
        syn::parse2(quote! { (#(#elems),*) }).unwrap()
    }
}

impl Subsitution for PathVar {
    type Output = Path;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        let segments: Vec<_> = self
            .segments
            .iter()
            .map(|SegmentVar { ident, arguments }| {
                let arguments = arguments.substitute(env, substitution);
                PathSegment {
                    ident: ident.to_owned(),
                    arguments,
                }
            })
            .collect();

        syn::parse2(quote! { #(#segments)::* }).unwrap()
    }
}

impl Subsitution for PathArgumentsVar {
    type Output = PathArguments;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        match self {
            PathArgumentsVar::None => PathArguments::None,
            PathArgumentsVar::AngleBracketed(args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.substitute(env, substitution))
                    .collect();
                let args = syn::parse2(quote! { < #(#args),* > }).unwrap();
                PathArguments::AngleBracketed(args)
            }
            PathArgumentsVar::Parenthesized(args) => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| arg.substitute(env, substitution))
                    .collect();
                let args = syn::parse2(quote! { ( #(#args),* ) }).unwrap();
                PathArguments::Parenthesized(args)
            }
        }
    }
}

impl Subsitution for WherePredicateVar {
    type Output = WherePredicate;

    fn substitute(
        &self,
        env: &Env,
        substitution: &IndexMap<Shared<Variable>, Ident>,
    ) -> Self::Output {
        todo!();
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

impl Parse for PathVar {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: Path = input.parse()?;
        let path = path.parse_pure_path()?;
        Ok(path)
    }
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

impl Parse for WherePredicateVar {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let predicate: WherePredicate = input.parse()?;
        let predicate = predicate.parse_pure_where_predicate()?;
        Ok(predicate)
    }
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
