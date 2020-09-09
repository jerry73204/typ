use super::*;

pub fn translate_block(
    block: &Block,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    let mut output_ty = None;

    // creates a subscope
    scope.sub_scope(|scope| {
        for stmt in block.stmts.iter() {
            match stmt {
                Stmt::Local(local) => {
                    // parse let statement as (let indent: trait_bounds = expr)
                    let (ident, ty_opt, is_mut, expr) = {
                        // check if the initial type is present
                        let (pat, expr) = match local {
                            Local {
                                pat,
                                init: Some((_eq, expr)),
                                ..
                            } => (pat, expr),
                            _ => {
                                return Err(Error::new(local.span(), "initial type must be given"))
                            }
                        };

                        let (ident, ty_opt, is_mut) = match pat {
                            // only indent (let x = ...)
                            Pat::Ident(PatIdent {
                                ident, mutability, ..
                            }) => (ident, None, matches!(mutability, Some(_))),
                            // indent with type (let x: T = ...)
                            Pat::Type(PatType { pat, ty, .. }) => {
                                let (ident, is_mut) = match &**pat {
                                    Pat::Ident(PatIdent {
                                        ident, mutability, ..
                                    }) => (ident, matches!(mutability, Some(_))),
                                    _ => return Err(Error::new(local.span(), "not an identifier")),
                                };
                                (ident, Some(&**ty), is_mut)
                            }
                            _ => return Err(Error::new(pat.span(), "not a identifier")),
                        };

                        (ident, ty_opt, is_mut, expr)
                    };

                    // compute output type from expression
                    let value = translate_expr(expr, scope, items)?;

                    // add new bounded quantifier
                    scope.insert_bounded_quantifier(ident.to_owned(), is_mut, value.clone());

                    // insert trait bounds
                    if let Some(ty) = ty_opt {
                        let bounded_ty = ty.parse_type_var(scope)?;
                        let bounds = ty.parse_type_param_bounds_var(scope)?;
                        let predicates =
                            WherePredicateVar::Type(PredicateTypeVar { bounded_ty, bounds });
                        scope.insert_predicate(predicates);
                    }

                    output_ty = None;
                }
                Stmt::Item(item) => {
                    return Err(Error::new(item.span(), "in-block item is not allowed"))
                }
                Stmt::Expr(expr) => {
                    output_ty = Some(translate_expr(expr, scope, items)?);
                }
                Stmt::Semi(expr, _semi) => {
                    translate_expr(expr, scope, items)?;
                    output_ty = None;
                }
            }
        }

        Ok(())
    })?;

    Ok(output_ty.unwrap_or_else(|| syn::parse2(quote! { () }).unwrap()))
}
