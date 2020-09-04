#![feature(hash_set_entry)]

mod common;
mod env;
mod parse;
mod scope;
mod trans;
mod utils;
mod var;

use crate::{common::*, parse::ItemVec};

#[proc_macro]
pub fn typ(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ItemVec(items) = syn::parse_macro_input!(tokens as ItemVec);
    crate::trans::translate_items(&items)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
