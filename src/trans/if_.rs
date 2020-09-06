use super::*;

pub fn translate_if_expr(if_: &ExprIf, scope: &mut Env) -> syn::Result<usize>
where
{
    let ExprIf {
        cond,
        then_branch,
        else_branch,
        ..
    } = if_;

    // generate predicate tokens
    let cond_tokens = {
        let cond_ty_id = translate_expr(&*cond, scope)?;
        let cond_ty = scope.pop(cond_ty_id);

        let (cond_tokens, predicates): (Vec<_>, Vec<_>) = cond_ty
            .into_iter()
            .map(|cond_ty| {
                let cond_ty = cond_ty.into_type().unwrap();
                let eq_trait: PathVar =
                    syn::parse2(quote! { typenum::type_operators::IsEqual<typenum::B1> }).unwrap();
                let path = {
                    let mut path = eq_trait.clone();
                    path.segments.push(SegmentVar {
                        ident: format_ident!("Output"),
                        arguments: PathArgumentsVar::None,
                    });
                    path
                };
                let output = TypeVar::Path(TypePathVar {
                    qself: Some(QSelfVar {
                        ty: Box::new(cond_ty.clone()),
                        position: eq_trait.segments.len(),
                    }),
                    path,
                });
                let apply_predicate = WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty: cond_ty.clone(),
                    bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                        modifier: TraitBoundModifierVar::None,
                        path: eq_trait,
                    })],
                });
                let output_predicate = {
                    WherePredicateVar::Type(PredicateTypeVar {
                        bounded_ty: output.clone(),
                        bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                            modifier: TraitBoundModifierVar::None,
                            path: syn::parse2(quote! { typenum::marker_traits::Bit }).unwrap(),
                        })],
                    })
                };
                (output, (apply_predicate, output_predicate))
            })
            .unzip();
        let (apply_predicates, output_predicates): (Vec<_>, Vec<_>) =
            predicates.into_iter().unzip();

        scope.insert_predicate(apply_predicates);
        scope.insert_predicate(output_predicates);
        cond_tokens
    };

    // evaluate each branch
    let mut brancher = scope.into_brancher(cond_tokens);

    // generate impl items
    let b1: TypeVar = syn::parse2(quote! { typenum::B1 }).unwrap();
    let b0: TypeVar = syn::parse2(quote! { typenum::B0 }).unwrap();

    let output = match else_branch {
        Some((_else, else_expr)) => {
            let then_tokens = brancher.branch(b1, |sub_scope| -> syn::Result<_> {
                let id = translate_block(then_branch, sub_scope)?;
                let tokens = sub_scope.pop(id);
                Ok(tokens)
            })?;
            let else_tokens = brancher.branch(b0, |sub_scope| -> syn::Result<_> {
                let id = translate_expr(&**else_expr, sub_scope)?;
                let tokens = sub_scope.pop(id);
                Ok(tokens)
            })?;
            let values: Vec<_> = then_tokens
                .into_iter()
                .chain(else_tokens)
                .map(|var| var.into_type().unwrap())
                .collect();
            values
        }
        None => {
            let then_tokens = brancher.branch(b1, |sub_scope| -> syn::Result<_> {
                translate_block(then_branch, sub_scope)?;
                let num_branches = sub_scope.num_branches();
                let tokens: Vec<TypeVar> = (0..num_branches)
                    .map(|_| syn::parse2(quote! { () }).unwrap())
                    .collect();
                Ok(tokens)
            })?;
            let else_tokens = brancher.branch(b0, |sub_scope| {
                let num_branches = sub_scope.num_branches();
                let tokens: Vec<TypeVar> = (0..num_branches)
                    .map(|_| syn::parse2(quote! { () }).unwrap())
                    .collect();
                tokens
            });
            let values: Vec<_> = then_tokens.into_iter().chain(else_tokens).collect();
            values
        }
    };

    let scope = brancher.merge();
    let output_id = scope.push(output);

    Ok(output_id)
}
