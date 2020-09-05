use super::*;

pub fn translate_assign_expr(assign: &ExprAssign, scope: &mut Env) -> syn::Result<usize>
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
    let value_id = translate_expr(right, scope)?;
    let values: Vec<_> = scope.pop(value_id);

    // update state
    scope.assign_quantifier(ident, values)?;

    // expand return value
    let num_branches = scope.num_branches();
    let expanded: Vec<_> = (0..num_branches).map(|_| quote! { () }).collect();
    let id = scope.push(expanded);

    Ok(id)
}
