use super::*;
use crate::parse::{GenericsAttr, SimpleTypeParam};

// trait List {}

// struct Cons<Head, Tail>
// where
//     Tail: List,
// {
//     head: Head,
//     tail: Tail,
// }
// impl<Head, Tail> List for Cons<Head, Tail> where Tail: List {}

// struct Nil;
// impl List for Nil {}

// trait Append<T> {
//     type Output<X>;
// }

// impl<T> Append<T> for () {
//     type Output<X> = X;
// }

pub fn translate_match_expr(match_: &ExprMatch, scope: &mut Env) -> syn::Result<usize>
where
{
    let ExprMatch { expr, arms, .. } = match_;

    // parse matched expression
    let cond_id = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, scope)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };
    let conds: Vec<_> = scope
        .pop(cond_id)
        .into_iter()
        .map(|var| var.into_type().unwrap())
        .collect();

    let mut brancher = scope.into_brancher(conds);

    // generate impl items
    let tokens_per_branch: Vec<_> = arms
        .iter()
        .map(
            |Arm {
                 pat, body, attrs, ..
             }|
             -> syn::Result<_> {
                // parse attributes
                let generics_attr = {
                    let mut generics_attr = None;
                    for attr in attrs.iter() {
                        let Attribute { style, path, .. } = attr;

                        // sanity check
                        match (style, path.get_ident()) {
                            (AttrStyle::Outer, Some(ident)) => match ident.to_string().as_str() {
                                "generics" => match generics_attr {
                                    Some(_) => {
                                        return Err(Error::new(
                                            path.span(),
                                            "the generics attribute is defined more than once",
                                        ));
                                    }
                                    None => generics_attr = Some(attr),
                                },
                                _ => return Err(Error::new(path.span(), "unsupported attribute")),
                            },
                            (AttrStyle::Outer, None) => {
                                return Err(Error::new(path.span(), "unsupported attribute"));
                            }
                            (AttrStyle::Inner(_), _) => {
                                return Err(Error::new(
                                    attr.span(),
                                    "inner attribute is not supported",
                                ))
                            }
                        }
                    }
                    generics_attr
                };

                // insert new free quantifiers
                if let Some(attr) = generics_attr {
                    let GenericsAttr { params } = syn::parse2(attr.tokens.to_owned())?;
                }

                // let cond_target = pat.into_pure_type()?;

                // let body_tokens = brancher.branch(cond_target, |sub_env| -> syn::Result<_> {
                //     let body_id = translate_expr(body, sub_env)?;
                //     let body_tokens = sub_enc.pop(body_id);
                //     Ok(body_tokens)
                // })?;

                // Ok(body_tokens)
                todo!();
                Ok(())
            },
        )
        .try_collect()?;
    // let tokens: Vec<_> = tokens_per_branch.into_iter().flatten().collect();

    // let scope = brancher.merge();
    // let output_id = scope.push(tokens);

    // Ok(output_id)
    todo!();
}
