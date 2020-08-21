use crate::common::*;

#[derive(Debug)]
pub struct Env {
    state: Rc<RefCell<EnvState>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            state: EnvState::new(None),
        }
    }

    pub fn create_trait_by_name<F, T>(&self, name: Ident, f: F) -> T
    where
        F: FnOnce(&mut TraitBuilder) -> T,
    {
        let mut builder = TraitBuilder::new(name.clone());
        let ret = f(&mut builder);
        let trait_def = builder.build();
        self.state.borrow_mut().traits.insert(name, trait_def);
        ret
    }

    pub fn create_trait_by_prefix<F, T>(&self, prefix: String, f: F) -> T
    where
        F: FnOnce(&Ident, &mut TraitBuilder) -> T,
    {
        let count = {
            let prefixes = &mut self.state.borrow_mut().trait_name_prefixes;
            if let Some(_) = prefixes.subtrie_mut(&prefix) {
                panic!("the trait name cannot proper prefix of existing trait names");
            }
            prefixes.map_with_default(prefix.clone(), |count| *count += 1, 0);
            *prefixes.get(&prefix).unwrap()
        };

        let ident = format_ident!("{}{}", prefix, count);
        let mut builder = TraitBuilder::new(ident.clone());
        let ret = f(&ident, &mut builder);
        let trait_def = builder.build();
        self.state.borrow_mut().traits.insert(ident, trait_def);
        ret
    }

    pub fn create_impl<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut ImplBuilder) -> T,
    {
        let mut builder = ImplBuilder::new();
        let ret = f(&mut builder);
        let def = builder.build();
        self.state.borrow_mut().impls.push(def);
        ret
    }

    pub fn create_mod(&self, ident: Ident) -> Option<Env> {
        use std::collections::hash_map::Entry;

        match self.state.borrow_mut().mods.entry(ident) {
            Entry::Vacant(entry) => {
                let sub_state = EnvState::new(Some(self.state.clone()));
                entry.insert(sub_state.clone());

                let sub_env = Env { state: sub_state };
                Some(sub_env)
            }
            Entry::Occupied(_entry) => None,
        }
    }
}

#[derive(Debug)]
struct EnvState {
    parent: Option<Rc<RefCell<EnvState>>>,
    trait_name_prefixes: Trie<String, usize>,
    mods: HashMap<Ident, Rc<RefCell<EnvState>>>,
    impls: Vec<ImplDef>,
    traits: HashMap<Ident, TraitDef>,
}

impl EnvState {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent,
            trait_name_prefixes: Trie::new(),
            mods: HashMap::new(),
            impls: vec![],
            traits: HashMap::new(),
        }))
    }
}

#[derive(Debug)]
pub struct ImplBuilder {
    generics_opt: Option<LinkedHashSet<Ident>>,
    associated_types: HashMap<Ident, TokenStream>,
}

impl ImplBuilder {
    pub fn set_generics(&mut self, generics: LinkedHashSet<Ident>) {
        self.generics_opt = Some(generics);
    }

    pub fn add_assocoated_type(&mut self, ident: Ident, tokens: TokenStream) {
        if let Some(_) = self.associated_types.insert(ident, tokens) {
            panic!("the associated type is already defined")
        }
    }

    fn new() -> Self {
        Self {
            generics_opt: None,
            associated_types: HashMap::new(),
        }
    }

    fn build(self) -> ImplDef {
        let Self {
            generics_opt,
            associated_types,
        } = self;

        match generics_opt {
            Some(generics) => ImplDef {
                generics: generics.into_iter().collect(),
                associated_types,
            },
            None => panic!("the builder is incomplete"),
        }
    }
}

#[derive(Debug)]
pub struct ImplDef {
    generics: Vec<Ident>,
    associated_types: HashMap<Ident, TokenStream>,
}

#[derive(Debug)]
pub struct TraitBuilder {
    ident: Ident,
    generics_opt: Option<LinkedHashSet<Ident>>,
    associated_types: HashMap<Ident, TokenStream>,
}

impl TraitBuilder {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
            associated_types: HashMap::new(),
            generics_opt: None,
        }
    }

    pub fn set_generics(&mut self, generics: LinkedHashSet<Ident>) {
        self.generics_opt = Some(generics);
    }

    pub fn insert_associated_type(
        &mut self,
        ident: Ident,
        value: TokenStream,
    ) -> Option<TokenStream> {
        self.associated_types.insert(ident, value)
    }

    pub fn build(self) -> TraitDef {
        let Self {
            ident,
            associated_types,
            generics_opt,
        } = self;

        let generics = generics_opt
            .map(|generics| generics.into_iter().collect())
            .unwrap_or_else(|| vec![]);

        TraitDef {
            ident,
            generics,
            associated_types,
        }
    }
}

#[derive(Debug)]
pub struct TraitDef {
    ident: Ident,
    generics: Vec<Ident>,
    associated_types: HashMap<Ident, TokenStream>,
}
