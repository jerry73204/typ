use super::*;

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
        .map(|Arm { pat, body, .. }| -> syn::Result<_> {
            let cond_target = pat.into_pure_type()?;

            let body_tokens = brancher.branch(cond_target, |sub_scope| -> syn::Result<_> {
                let body_id = translate_expr(body, sub_scope, env)?;
                let body_tokens = sub_scope.pop(body_id);
                Ok(body_tokens)
            })?;

            Ok(body_tokens)
        })
        .try_collect()?;
    let tokens: Vec<_> = tokens_per_branch.into_iter().flatten().collect();

    let scope = brancher.merge();
    let output_id = scope.push(tokens);

    Ok(output_id)
}
