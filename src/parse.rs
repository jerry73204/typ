use crate::{
    common::*,
    env::Env,
    var::{
        ParseTypeParamBoundVar, ParseTypeVar, ParseWherePredicateVar, PredicateTypeVar,
        WherePredicateVar,
    },
};

#[derive(Debug, Clone)]
pub struct ItemVec(pub Vec<Item>);

impl Parse for ItemVec {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();
        while !input.cursor().eof() {
            items.push(input.parse::<Item>()?);
        }
        Ok(ItemVec(items))
    }
}

#[derive(Debug, Clone)]
pub struct SimpleTypeParam {
    pub ident: Ident,
    pub bounds: Punctuated<TraitBound, syn::token::Add>,
}

impl Parse for SimpleTypeParam {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        let param = if input.peek(syn::token::Colon) {
            let _: syn::token::Colon = input.parse()?;
            let bounds = Punctuated::parse_separated_nonempty(input)?;
            Self { ident, bounds }
        } else {
            Self {
                ident,
                bounds: Punctuated::new(),
            }
        };

        Ok(param)
    }
}

impl ParseWherePredicateVar for SimpleTypeParam {
    fn parse_where_predicate_var(&self, scope: &Env) -> syn::Result<WherePredicateVar> {
        let SimpleTypeParam { ident, bounds } = self;

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
}

#[derive(Debug, Clone)]
pub struct GenericsAttr {
    pub params: Punctuated<SimpleTypeParam, syn::token::Comma>,
}

impl Parse for GenericsAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::parenthesized!(content in input);
        let params = Punctuated::parse_terminated(&content)?;
        Ok(Self { params })
    }
}

#[derive(Debug, Clone)]
pub struct CaptureAttr {
    pub params: Punctuated<Ident, syn::token::Comma>,
}

impl Parse for CaptureAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::parenthesized!(content in input);
        let params = Punctuated::parse_terminated(&content)?;
        Ok(Self { params })
    }
}
