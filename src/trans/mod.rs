mod assign;
mod binop;
mod block;
mod enum_;
mod fn_;
mod if_;
mod impl_;
mod lit;
mod match_;
mod misc;
mod struct_;
mod unary;

pub use crate::{
    common::*,
    env::Env,
    parse::{GenericsAttr, SimpleTypeParam},
    var::{
        ParsePathVar, ParsePureType, ParsePureWherePredicate, ParseTypeParamBoundsVar,
        ParseTypeVar, ParseWherePredicateVar, PathArgumentsVar, PathVar, PredicateTypeVar,
        QSelfVar, SegmentVar, Subsitution, TraitBoundModifierVar, TraitBoundVar, TypeParamBoundVar,
        TypePathVar, TypeTupleVar, TypeVar, WherePredicateVar,
    },
};
pub use assign::*;
pub use binop::*;
pub use block::*;
pub use enum_::*;
pub use fn_::*;
pub use if_::*;
pub use impl_::*;
pub use lit::*;
pub use match_::*;
pub use misc::*;
pub use struct_::*;
pub use unary::*;

pub fn translate_items(items: &[Item]) -> syn::Result<TokenStream> {
    let tokens_vec: Vec<_> = items
        .into_iter()
        .map(|item| {
            let tokens = match item {
                Item::Enum(enum_) => translate_enum(&enum_)?,
                Item::Fn(fn_) => {
                    let ItemFn {
                        sig, block, vis, ..
                    } = fn_;
                    translate_fn(vis, sig, block, None, None)?
                }
                Item::Struct(struct_) => translate_struct(&struct_)?,
                Item::Impl(impl_) => translate_impl(&impl_)?,
                Item::Use(use_) => translate_use(&use_),
                _ => {
                    return Err(Error::new(item.span(), "unsupported item kind"));
                }
            };
            Ok(tokens)
        })
        .try_collect()?;

    let expanded = quote! {
        #(#tokens_vec)*
    };

    Ok(expanded)
}

fn translate_use(use_: &ItemUse) -> TokenStream {
    // return as it is
    quote! { #use_ }
}
