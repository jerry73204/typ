//! TYP is a type-level programming langauge that computes types.
//! It enables you to write _type operators_, the functions that translates types, in Rust syntax.
//! Please read the [TYP book](https://github.com/jerry73204/typ-book/) understand the usage.

#![feature(hash_set_entry)]

mod common;
mod env;
mod parse;
mod trans;
mod tyint;
mod utils;
mod var;

use crate::{common::*, parse::ItemVec};

/// The main macro that translates the TYP langauge to actual Rust implementations.
#[proc_macro]
pub fn typ(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ItemVec(items) = parse_macro_input!(tokens as ItemVec);
    crate::trans::translate_items(&items)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

/// Constructs a signed integer type from an integer literal.
#[proc_macro]
pub fn tyint(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    tyint::tyint(input)
}

/// Constructs an unsigned integer type from an integer literal.
#[proc_macro]
pub fn tyuint(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    tyint::tyuint(input)
}
