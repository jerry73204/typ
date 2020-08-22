use crate::common::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SegmentVar {
    pub ident: Ident,
    pub generic_args: Vec<TypeVar>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeVar {
    Var(usize),
    Path(Vec<SegmentVar>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitVar {
    pub path: Vec<SegmentVar>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBoundsVar {
    pub traits: BTreeSet<TraitVar>,
}

impl TraitBoundsVar {
    pub fn empty() -> Self {
        Self {
            traits: BTreeSet::new(),
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
