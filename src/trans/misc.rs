use super::*;

pub fn translate_expr(expr: &Expr, scope: &mut Env) -> syn::Result<usize>
where
{
    dbg!();
    let ret = match expr {
        Expr::Match(match_) => translate_match_expr(match_, scope),
        Expr::Path(path) => translate_path_expr(path, scope),
        Expr::Tuple(tuple) => translate_tuple_expr(tuple, scope),
        Expr::Binary(binop) => translate_binary_expr(binop, scope),
        Expr::If(if_) => translate_if_expr(if_, scope),
        Expr::Block(block) => translate_block_expr(block, scope),
        Expr::Call(call) => translate_call_expr(call, scope),
        Expr::Paren(paren) => translate_expr(&paren.expr, scope),
        Expr::Assign(assign) => translate_assign_expr(&assign, scope),
        Expr::Lit(lit) => translate_lit_expr(&lit, scope),
        Expr::Unary(unary) => translate_unary_expr(&unary, scope),
        _ => Err(Error::new(expr.span(), "unsupported expression")),
    };
    dbg!();
    ret
}

pub fn translate_path_expr(expr: &ExprPath, scope: &mut Env) -> syn::Result<usize>
where
{
    dbg!();
    let values = scope.substitute_type(expr)?;
    let id = scope.push(values);
    dbg!();
    Ok(id)
}

pub fn translate_tuple_expr(tuple: &ExprTuple, scope: &mut Env) -> syn::Result<usize>
where
{
    dbg!();
    // translate each element
    let ids: Vec<_> = tuple
        .elems
        .iter()
        .map(|expr| translate_expr(expr, scope))
        .try_collect()?;
    let branches_per_value: Vec<_> = ids.into_iter().map(|id| scope.pop(id)).collect();

    // merge tokens from each element
    let values_per_branch = crate::utils::transpose(&branches_per_value);
    let expanded: Vec<_> = values_per_branch
        .into_iter()
        .map(|values| {
            quote! { ( #(#values),* ) }
        })
        .collect();
    let id = scope.push(expanded);

    dbg!();
    Ok(id)
}

pub fn translate_block_expr(block: &ExprBlock, scope: &mut Env) -> syn::Result<usize> {
    translate_block(&block.block, scope)
}

pub fn translate_call_expr(call: &ExprCall, scope: &mut Env) -> syn::Result<usize>
where
{
    let ExprCall { func, args, .. } = call;

    // parse func name as trait
    dbg!();
    let func = match &**func {
        Expr::Path(ExprPath {
            qself: None, path, ..
        }) => scope.substitute_trait(path)?,
        Expr::Path(ExprPath { qself: Some(_), .. }) => {
            return Err(Error::new(func.span(), "not a trait"))
        }
        _ => return Err(Error::new(func.span(), "not a trait")),
    };
    let func_id = scope.push(func);

    // parse arguments
    dbg!();
    let arg_ids: Vec<_> = args
        .iter()
        .map(|arg| translate_expr(arg, scope))
        .try_collect()?;
    dbg!();

    dbg!();
    let func_tokens = scope.pop(func_id);
    dbg!();
    let branches_per_arg: Vec<_> = arg_ids.into_iter().map(|id| scope.pop(id)).collect();
    dbg!();
    let args_per_branch = crate::utils::transpose(&branches_per_arg);
    dbg!();

    dbg!();
    let (expanded, predicates): (Vec<_>, Vec<_>) = func_tokens
        .into_iter()
        .zip_eq(args_per_branch)
        .map(|(func, args)| {
            // expand trait "call"
            let func_trait = if args.is_empty() {
                quote! { #func }
            } else {
                quote! { #func < #(#args),* >  }
            };
            let expanded = quote! { < () as #func_trait >::Output };

            // expand trait bounds
            let ty = quote! { () };
            let bounds = func_trait;

            (expanded, (ty, bounds))
        })
        .unzip();

    dbg!();
    scope.insert_trait_bounds(predicates);
    let id = scope.push(expanded);

    Ok(id)
}
