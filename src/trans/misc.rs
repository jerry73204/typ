use super::*;

pub fn translate_expr(expr: &Expr, scope: &mut Scope, env: &mut Env) -> syn::Result<TokenStream>
where
{
    match expr {
        Expr::Match(match_) => translate_match_expr(match_, scope, env),
        Expr::Path(path) => translate_path_expr(path, scope, env),
        Expr::Tuple(tuple) => translate_tuple_expr(tuple, scope, env),
        Expr::Binary(binop) => translate_binary_expr(binop, scope, env),
        Expr::If(if_) => translate_if_expr(if_, scope, env),
        Expr::Block(block) => translate_block_expr(block, scope, env),
        Expr::Call(call) => translate_call_expr(call, scope, env),
        Expr::Paren(paren) => translate_expr(&paren.expr, scope, env),
        Expr::Assign(assign) => translate_assign_expr(&assign, scope, env),
        Expr::Lit(lit) => translate_lit_expr(&lit, scope, env),
        Expr::Unary(unary) => translate_unary_expr(&unary, scope, env),
        _ => Err(Error::new(expr.span(), "unsupported expression")),
    }
}

pub fn translate_path_expr(
    ExprPath { qself, path, .. }: &ExprPath,
    scope: &mut Scope,
    _env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let value = scope.type_var_builder().from_path(qself.as_ref(), path)?;
    Ok(quote! { #value })
}

pub fn translate_tuple_expr(
    tuple: &ExprTuple,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let types: Vec<_> = tuple
        .elems
        .iter()
        .map(|expr| translate_expr(expr, scope, env))
        .try_collect()?;
    Ok(quote! { ( #(#types),*  )})
}

pub fn translate_block_expr(
    block: &ExprBlock,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream> {
    translate_block(&block.block, scope, env)
}

pub fn translate_call_expr(
    call: &ExprCall,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprCall { func, args, .. } = call;

    let func = match &**func {
        Expr::Path(ExprPath {
            qself: None, path, ..
        }) => scope.trait_var_builder().from_path(path)?,
        Expr::Path(ExprPath { qself: Some(_), .. }) => {
            return Err(Error::new(func.span(), "not a trait"))
        }
        _ => return Err(Error::new(func.span(), "not a trait")),
    };
    let args: Vec<_> = args
        .iter()
        .map(|arg| translate_expr(arg, scope, env))
        .try_collect()?;

    let func_trait = if args.is_empty() {
        quote! { #func }
    } else {
        quote! { #func < #(#args),* >  }
    };
    let output = quote! { < () as #func_trait >::Output };

    scope.insert_trait_bounds(quote! { () }, func_trait);

    Ok(output)
}
