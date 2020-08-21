use crate::{common::*, scope::Scope};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeVar {
    Var(usize),
    Path(Vec<(Ident, Vec<TypeVar>)>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBoundsVar {
    pub(crate) paths: BTreeSet<Vec<(Ident, Vec<TypeVar>)>>,
}

impl TraitBoundsVar {
    pub fn empty() -> Self {
        Self {
            paths: BTreeSet::new(),
        }
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

impl Add<Self> for &TraitBoundsVar {
    type Output = TraitBoundsVar;

    fn add(self, rhs: Self) -> Self::Output {
        let paths: BTreeSet<_> = self.paths.iter().chain(rhs.paths.iter()).collect();
        let paths = paths.into_iter().map(ToOwned::to_owned).collect();
        Self::Output { paths }
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
