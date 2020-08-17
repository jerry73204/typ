mod builder;
mod common;
mod path;
mod scope;
mod trans;

use crate::common::*;

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
pub fn tyrade(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ItemVec(items) = syn::parse_macro_input!(tokens as ItemVec);
    crate::trans::translate_items(&items)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
