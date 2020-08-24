use crate::common::*;

pub use env::*;

mod env {
    use super::*;

    #[derive(Debug)]
    pub struct Env {
        trait_name_prefixes: Trie<String, usize>,
        items: Vec<TokenStream>,
        mods: IndexMap<Ident, Env>,
    }

    impl Env {
        pub fn new() -> Self {
            Self {
                trait_name_prefixes: Trie::new(),
                items: vec![],
                mods: IndexMap::new(),
            }
        }

        pub fn register_trait_name(&mut self, prefix: &str) -> Option<Ident> {
            let count = {
                let prefixes = &mut self.trait_name_prefixes;

                // check if the prefix is a proper prefix of existing prefixes
                if let Some(_) = prefixes.subtrie_mut(prefix) {
                    return None;
                }
                prefixes.map_with_default(prefix.to_string(), |count| *count += 1, 0);
                *prefixes.get(prefix).unwrap()
            };

            Some(format_ident!("{}{}", prefix, count))
        }

        pub fn add_item(&mut self, item: TokenStream) {
            self.items.push(item);
        }

        pub fn extend_items<I>(&mut self, iter: I)
        where
            I: IntoIterator<Item = TokenStream>,
        {
            self.items.extend(iter);
        }

        pub fn create_mod(&mut self, ident: Ident) -> Option<&mut Env> {
            use indexmap::map::Entry;

            match self.mods.entry(ident) {
                Entry::Vacant(entry) => {
                    let sub_env = Env::new();
                    Some(entry.insert(sub_env))
                }
                Entry::Occupied(_entry) => None,
            }
        }
    }

    impl ToTokens for Env {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let Self { items, mods, .. } = self;
            let items_tokens = quote! { #(#items)* };
            let mods_tokens = mods.iter().map(|(mod_name, sub_env)| {
                quote! {
                    mod #mod_name {
                        #sub_env
                    }
                }
            });

            let expanded = quote! {
                #items_tokens
                #(#mods_tokens)*
            };

            tokens.extend(expanded);
        }
    }
}

#[derive(Debug)]
pub struct ImplInit {
    pub self_ty: Option<TokenStream>,
    pub trait_: Option<TokenStream>,
    pub generics: IndexSet<Ident>,
    pub type_items: IndexMap<Ident, TokenStream>,
    pub bounds: Vec<(TokenStream, TokenStream)>,
}

impl ImplInit {
    fn new() -> Self {
        Self {
            self_ty: None,
            trait_: None,
            generics: IndexSet::new(),
            type_items: IndexMap::new(),
            bounds: vec![],
        }
    }

    fn build(self) -> ImplDef {
        let Self {
            self_ty,
            trait_,
            generics,
            type_items,
            bounds,
        } = self;

        ImplDef {
            self_ty: self_ty.expect("self_ty is not set"),
            trait_,
            generics,
            type_items,
            bounds,
        }
    }
}

#[derive(Debug)]
pub struct ImplDef {
    self_ty: TokenStream,
    trait_: Option<TokenStream>,
    generics: IndexSet<Ident>,
    type_items: IndexMap<Ident, TokenStream>,
    bounds: Vec<(TokenStream, TokenStream)>,
}

#[derive(Debug)]
pub struct TraitInit {
    ident: Ident,
    pub vis: Visibility,
    pub generics: IndexSet<Ident>,
    pub type_items: IndexSet<Ident>,
}

impl TraitInit {
    fn new(ident: Ident) -> Self {
        Self {
            ident,
            vis: Visibility::Inherited,
            type_items: IndexSet::new(),
            generics: IndexSet::new(),
        }
    }

    pub fn build(self) -> TraitDef {
        let Self {
            vis,
            ident,
            type_items,
            generics,
        } = self;

        TraitDef {
            vis,
            ident,
            generics,
            type_items,
        }
    }
}

#[derive(Debug)]
pub struct TraitDef {
    vis: Visibility,
    ident: Ident,
    generics: IndexSet<Ident>,
    type_items: IndexSet<Ident>,
}
