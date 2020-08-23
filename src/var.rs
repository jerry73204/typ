use crate::{common::*, scope::SharedScopeState};

#[derive(Clone, Debug)]
pub struct Scoped<T> {
    pub scope: SharedScopeState,
    pub var: T,
}

impl ToTokens for Scoped<&TypeVar> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { scope, var: ty } = self;

        let expanded = match ty {
            TypeVar::Var { id } => scope.borrow().get_quantifier(id).unwrap().value.to_owned(),
            TypeVar::Path { segments } => {
                let segments = Scoped {
                    scope: scope.clone(),
                    var: segments,
                };
                quote! { #segments }
            }
            TypeVar::QSelf {
                ty,
                trait_,
                associated,
            } => {
                let ty = Scoped {
                    scope: scope.clone(),
                    var: &**ty,
                };
                let trait_ = Scoped {
                    scope: scope.clone(),
                    var: trait_,
                };
                let associated_path = Scoped {
                    scope: scope.clone(),
                    var: associated,
                };

                if associated.is_empty() {
                    quote! {
                        < #ty as #trait_ >
                    }
                } else {
                    quote! {
                        < #ty as #trait_ > :: #associated_path
                    }
                }
            }
            TypeVar::Tuple { types } => {
                let types = types.iter().map(|ty| Scoped {
                    scope: scope.clone(),
                    var: ty,
                });
                quote! { ( #(#types),* ) }
            }
        };

        tokens.extend(expanded);
    }
}

impl ToTokens for Scoped<TypeVar> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Scoped {
            scope: self.scope.clone(),
            var: &self.var,
        }
        .to_tokens(tokens)
    }
}

impl ToTokens for Scoped<&TraitVar> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Scoped {
            scope,
            var: TraitVar { segments },
        } = self;

        let segments = Scoped {
            scope: scope.clone(),
            var: segments,
        };
        let expanded = quote! { #segments };
        tokens.extend(expanded);
    }
}

impl ToTokens for Scoped<TraitVar> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Scoped {
            scope: self.scope.clone(),
            var: &self.var,
        }
        .to_tokens(tokens)
    }
}

impl ToTokens for Scoped<&Vec<SegmentVar>> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            scope,
            var: segments,
        } = self;

        let segments: Vec<_> = segments
            .iter()
            .map(
                |SegmentVar {
                     ident,
                     generic_args,
                 }| {
                    let args: Vec<_> = generic_args
                        .iter()
                        .map(|arg| {
                            let arg = Scoped {
                                scope: scope.clone(),
                                var: arg,
                            };
                            quote! { #arg }
                        })
                        .collect();

                    if args.is_empty() {
                        quote! { #ident }
                    } else {
                        quote! { #ident < #(#args),* > }
                    }
                },
            )
            .collect();

        let expanded = quote! { #(#segments)::* };
        tokens.extend(expanded);
    }
}

impl ToTokens for Scoped<Vec<SegmentVar>> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Scoped {
            scope: self.scope.clone(),
            var: &self.var,
        }
        .to_tokens(tokens)
    }
}

impl ToTokens for Scoped<&TraitBoundsVar> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Scoped {
            scope,
            var: TraitBoundsVar { traits },
        } = self;

        let traits: Vec<_> = traits
            .iter()
            .map(|trait_| {
                let trait_ = Scoped {
                    scope: scope.clone(),
                    var: trait_,
                };
                quote! { #trait_ }
            })
            .collect();
        let expanded = quote! { #(#traits)+* };
        tokens.extend(expanded);
    }
}

impl ToTokens for Scoped<TraitBoundsVar> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Scoped {
            scope: self.scope.clone(),
            var: &self.var,
        }
        .to_tokens(tokens)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SegmentVar {
    pub ident: Ident,
    pub generic_args: Vec<TypeVar>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeVar {
    Var {
        id: usize,
    },
    QSelf {
        ty: Box<TypeVar>,
        trait_: TraitVar,
        associated: Vec<SegmentVar>,
    },
    Path {
        segments: Vec<SegmentVar>,
    },
    Tuple {
        types: Vec<TypeVar>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitVar {
    pub segments: Vec<SegmentVar>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBoundsVar {
    pub traits: BTreeSet<TraitVar>,
}

impl From<TraitVar> for TraitBoundsVar {
    fn from(trait_var: TraitVar) -> Self {
        Self {
            traits: vec![trait_var].into_iter().collect(),
        }
    }
}

impl IntoIterator for TraitBoundsVar {
    type Item = TraitVar;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.traits.into_iter()
    }
}

impl Add<Self> for TraitBoundsVar {
    type Output = TraitBoundsVar;

    fn add(self, rhs: Self) -> Self::Output {
        let traits: BTreeSet<_> = self
            .traits
            .into_iter()
            .chain(rhs.traits.into_iter())
            .collect();
        Self { traits }
    }
}

impl Add<Self> for &TraitBoundsVar {
    type Output = TraitBoundsVar;

    fn add(self, rhs: Self) -> Self::Output {
        let traits: BTreeSet<_> = self.traits.iter().chain(rhs.traits.iter()).collect();
        let traits = traits.into_iter().map(ToOwned::to_owned).collect();
        Self::Output { traits }
    }
}

impl AddAssign<Self> for TraitBoundsVar {
    fn add_assign(&mut self, other: TraitBoundsVar) {
        self.traits.extend(other.into_iter());
    }
}

impl Sum<Self> for TraitBoundsVar {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let traits: BTreeSet<_> = iter.map(|var| var.traits.into_iter()).flatten().collect();
        Self { traits }
    }
}

impl Extend<TraitVar> for TraitBoundsVar {
    fn extend<T: IntoIterator<Item = TraitVar>>(&mut self, iter: T) {
        iter.into_iter().for_each(|item| {
            self.traits.insert(item);
        });
    }
}
