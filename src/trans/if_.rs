use super::*;

pub fn translate_if_expr(if_: &ExprIf, scope: &mut ScopeSet, env: &mut Env) -> syn::Result<usize>
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
        let cond_ty_id = translate_expr(&*cond, scope, env)?;
        let cond_ty = scope.pop(cond_ty_id);

        let (cond_tokens, predicates): (Vec<_>, Vec<_>) = cond_ty
            .into_iter()
            .map(|ty| {
                let trait_ = quote! { typenum::type_operators::IsEqual<typenum::B1> };
                let value = Rc::new(quote! { <#ty as #trait_>::Output });
                let op_predicate = (ty, trait_);
                let output_predicate = (value.clone(), quote! { typenum::marker_traits::Bit });
                (value, (op_predicate, output_predicate))
            })
            .unzip();
        let (op_predicates, output_predicates): (Vec<_>, Vec<_>) = predicates.into_iter().unzip();

        scope.insert_trait_bounds(op_predicates);
        scope.insert_trait_bounds(output_predicates);
        cond_tokens
    };

    // evaluate each branch
    let mut brancher = scope.into_brancher(cond_tokens);

    // generate impl items
    let values = match else_branch {
        Some((_else, else_expr)) => {
            let then_tokens =
                brancher.branch(quote! { typenum::B1 }, |sub_scope| -> syn::Result<_> {
                    let id = translate_block(then_branch, sub_scope, env)?;
                    let tokens = sub_scope.pop(id);
                    Ok(tokens)
                })?;
            let else_tokens =
                brancher.branch(quote! { typenum::B0 }, |sub_scope| -> syn::Result<_> {
                    let id = translate_expr(&**else_expr, sub_scope, env)?;
                    let tokens = sub_scope.pop(id);
                    Ok(tokens)
                })?;
            let values: Vec<_> = then_tokens.into_iter().chain(else_tokens).collect();
            values
        }
        None => {
            let then_tokens =
                brancher.branch(quote! { typenum::B1 }, |sub_scope| -> syn::Result<_> {
                    translate_block(then_branch, sub_scope, env)?;
                    let num_branches = sub_scope.num_branches();
                    let tokens: Vec<_> =
                        (0..num_branches).map(|_| Rc::new(quote! { () })).collect();
                    Ok(tokens)
                })?;
            let else_tokens = brancher.branch(quote! { typenum::B0 }, |sub_scope| {
                let num_branches = sub_scope.num_branches();
                let tokens: Vec<_> = (0..num_branches).map(|_| Rc::new(quote! { () })).collect();
                tokens
            });
            let values: Vec<_> = then_tokens.into_iter().chain(else_tokens).collect();
            values
        }
    };

    let scope = brancher.merge();
    let values_id = scope.push(values);

    Ok(values_id)
}
