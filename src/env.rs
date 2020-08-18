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
    mods: HashMap<Ident, Rc<RefCell<EnvState>>>,
    impls: Vec<ImplDef>,
}

impl EnvState {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent,
            mods: HashMap::new(),
            impls: vec![],
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
