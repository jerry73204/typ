use super::*;
use crate::parse::{GenericsAttr, SimpleTypeParam};

pub fn translate_match_expr(
    match_: &ExprMatch,
    scope: &mut ScopeSet,
    env: &mut Env,
) -> syn::Result<usize>
where
{
    let ExprMatch { expr, arms, .. } = match_;

    // parse matched expression
    let cond_id = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, scope, env)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };
    let cond_tokens = scope.pop(cond_id);

    let mut brancher = scope.into_brancher(cond_tokens);

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

                match generics_attr {
                    Some(attr) => {
                        let generics: GenericsAttr = syn::parse2(attr.tokens.to_owned())?;
                        dbg!(generics);
                    }
                    None => {}
                }

                let cond_target = pat.into_pure_type()?;

                let body_tokens = brancher.branch(cond_target, |sub_scope| -> syn::Result<_> {
                    let body_id = translate_expr(body, sub_scope, env)?;
                    let body_tokens = sub_scope.pop(body_id);
                    Ok(body_tokens)
                })?;

                Ok(body_tokens)
            },
        )
        .try_collect()?;
    let tokens: Vec<_> = tokens_per_branch.into_iter().flatten().collect();

    let scope = brancher.merge();
    let output_id = scope.push(tokens);

    Ok(output_id)
}
