use crate::common::*;

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
