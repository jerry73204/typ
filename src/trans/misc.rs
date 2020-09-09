use super::*;

pub fn translate_expr(expr: &Expr, scope: &mut Env, items: &mut Vec<Item>) -> syn::Result<TypeVar>
where
{
    let ret = match expr {
        Expr::Match(match_) => translate_match_expr(match_, scope, items),
        Expr::Path(path) => translate_path_expr(path, scope, items),
        Expr::Tuple(tuple) => translate_tuple_expr(tuple, scope, items),
        Expr::Binary(binop) => translate_binary_expr(binop, scope, items),
        Expr::If(if_) => translate_if_expr(if_, scope, items),
        Expr::Block(block) => translate_block_expr(block, scope, items),
        Expr::Call(call) => translate_call_expr(call, scope, items),
        Expr::Paren(paren) => translate_expr(&paren.expr, scope, items),
        Expr::Assign(assign) => translate_assign_expr(&assign, scope, items),
        Expr::Lit(lit) => translate_lit_expr(&lit, scope, items),
        Expr::Unary(unary) => translate_unary_expr(&unary, scope, items),
        _ => Err(Error::new(expr.span(), "unsupported expression")),
    };
    ret
}

pub fn translate_path_expr(
    expr: &ExprPath,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    expr.parse_type_var(scope)
}

pub fn translate_tuple_expr(
    tuple: &ExprTuple,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    // translate each element
    let elems: Vec<_> = tuple
        .elems
        .iter()
        .map(|expr| translate_expr(expr, scope, items))
        .try_collect()?;

    // merge tokens from each element
    Ok(TypeVar::Tuple(TypeTupleVar { elems }))
}

pub fn translate_block_expr(
    block: &ExprBlock,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar> {
    translate_block(&block.block, scope, items)
}

pub fn translate_call_expr(
    call: &ExprCall,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    let ExprCall { func, args, .. } = call;

    // parse arguments
    let args: Vec<_> = args
        .iter()
        .map(|arg| translate_expr(arg, scope, items))
        .try_collect()?;

    // parse the function path to a trait path
    let trait_path = {
        let mut trait_path = match &**func {
            Expr::Path(ExprPath {
                qself: None, path, ..
            }) => path.parse_path_var(scope)?,
            _ => return Err(Error::new(func.span(), "not a trait")),
        };

        // set the type arguments on trait
        trait_path.segments.last_mut().as_mut().unwrap().arguments =
            if let PathArgumentsVar::None = trait_path.segments.last().unwrap().arguments {
                PathArgumentsVar::AngleBracketed(args)
            } else {
                return Err(Error::new(
                    func.span(),
                    "type parameters are not allowed in trait call",
                ));
            };

        trait_path
    };

    // insert predicates
    let (output, predicate) = {
        // construct the output type
        let bounded_ty: TypeVar = syn::parse2(quote! { () }).unwrap();
        let output = {
            let position = trait_path.segments.len();
            let path = {
                let mut path = trait_path.clone();
                path.segments.push(SegmentVar {
                    ident: format_ident!("Output"),
                    arguments: PathArgumentsVar::None,
                });
                path
            };
            TypeVar::Path(TypePathVar {
                qself: Some(QSelfVar {
                    ty: Box::new(bounded_ty.clone()),
                    position,
                }),
                path,
            })
        };

        // construct the predicate
        let predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty,
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: trait_path,
            })],
        });

        (output, predicate)
    };

    scope.insert_predicate(predicate);

    Ok(output)
}
