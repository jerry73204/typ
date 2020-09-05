use super::*;

pub fn translate_unary_expr(
    ExprUnary { op, expr, .. }: &ExprUnary,
    scope: &mut Env,
) -> syn::Result<usize> {
    let ty_id = translate_expr(expr, scope)?;

    let trait_ = match op {
        UnOp::Neg(_) => {
            quote! { core::ops::Neg }
        }
        UnOp::Not(_) => {
            quote! { core::ops::Not }
        }
        _ => return Err(Error::new(op.span(), "unsupported unary operator")),
    };

    let ty_tokens = scope.pop(ty_id);
    let (expanded, predicates): (Vec<_>, Vec<_>) = ty_tokens
        .into_iter()
        .map(|ty| {
            let expanded = quote! { <#ty as #trait_>::Output };
            let bounds = trait_.clone();
            (expanded, (ty, bounds))
        })
        .unzip();

    scope.insert_trait_bounds(predicates);
    let id = scope.push(expanded);

    Ok(id)
}
