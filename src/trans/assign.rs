use super::*;

pub fn translate_assign_expr(assign: &ExprAssign, scope: &mut Env) -> syn::Result<TypeVar>
where
{
    let ExprAssign { left, right, .. } = assign;

    // parse lhs
    let ident = match &**left {
        Expr::Path(path) => match path.path.get_ident() {
            Some(ident) => ident,
            None => return Err(Error::new(path.span(), "not an identifier")),
        },
        _ => return Err(Error::new(left.span(), "not an identifier")),
    };

    // parse rhs
    let value = translate_expr(right, scope)?;

    // update state
    scope.assign_quantifier(ident, value)?;

    // expand return value
    let output: TypeVar = syn::parse2(quote! { () }).unwrap();

    Ok(output)
}
