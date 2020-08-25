use super::*;

pub fn translate_block(
    block: &Block,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let mut output_ty = None;

    // creates a subscope
    scope.sub_scope(|scope| {
        for stmt in block.stmts.iter() {
            match stmt {
                Stmt::Local(local) => {
                    // parse let statement
                    let (ident, trait_bounds_opt, is_mut, expr) = {
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

                        let (ident, trait_bounds_opt, is_mut) = match pat {
                            Pat::Ident(PatIdent {
                                ident, mutability, ..
                            }) => (ident, None, matches!(mutability, Some(_))),
                            Pat::Type(PatType { pat, ty, .. }) => {
                                let (ident, is_mut) = match &**pat {
                                    Pat::Ident(PatIdent {
                                        ident, mutability, ..
                                    }) => (ident, matches!(mutability, Some(_))),
                                    _ => return Err(Error::new(local.span(), "not an identifier")),
                                };
                                let trait_bounds =
                                    scope.trait_bounds_var_builder().from_type(ty)?;

                                (ident, Some(trait_bounds), is_mut)
                            }
                            _ => return Err(Error::new(pat.span(), "not a identifier")),
                        };

                        (ident, trait_bounds_opt, is_mut, expr)
                    };

                    // compute output type from expression
                    let value = translate_expr(expr, scope, env)?;

                    // register the local quantifier
                    scope.insert_bounded_quantifier(ident.to_owned(), value, is_mut);

                    // insert trait bounds
                    if let Some(trait_bounds) = trait_bounds_opt {
                        let predicate = scope
                            .type_var_builder()
                            .from_quantifier(ident)
                            .expect("please report bug: not a valid quantifier");
                        scope.insert_trait_bounds(quote! { #predicate }, quote! { #trait_bounds });
                    }
                }
                Stmt::Item(item) => {
                    return Err(Error::new(item.span(), "in-block item is not allowed"))
                }
                Stmt::Expr(expr) => {
                    output_ty = Some(translate_expr(expr, scope, env)?);
                }
                Stmt::Semi(expr, _semi) => {
                    translate_expr(expr, scope, env)?;
                    output_ty = Some(quote! { () });
                }
            }
        }

        Ok(())
    })?;

    Ok(output_ty.unwrap_or_else(|| quote! { () }))
}
