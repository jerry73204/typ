#![feature(hash_set_entry)]

mod common;
mod env;
mod scope;
mod trans;
mod var;

use crate::common::*;

#[derive(Debug, Clone)]
struct ItemVec(pub Vec<Item>);

impl Parse for ItemVec {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();
        while !input.cursor().eof() {
            items.push(input.parse::<Item>()?);
        }
        Ok(ItemVec(items))
    }
}

#[proc_macro]
pub fn typ(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ItemVec(items) = syn::parse_macro_input!(tokens as ItemVec);
    crate::trans::translate_items(&items)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
