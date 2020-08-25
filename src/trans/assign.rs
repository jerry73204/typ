use super::*;

pub fn translate_assign_expr(
    assign: &ExprAssign,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprAssign { left, right, .. } = assign;
    let quantifier_ident = match &**left {
        Expr::Path(path) => match path.path.get_ident() {
            Some(ident) => ident,
            None => return Err(Error::new(path.span(), "not an identifier")),
        },
        _ => return Err(Error::new(left.span(), "not an identifier")),
    };
    let value = translate_expr(right, scope, env)?;

    scope.assign_quantifier(quantifier_ident, value)?;
    Ok(quote! { () })
}
