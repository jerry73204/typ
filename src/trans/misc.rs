use super::*;

pub fn translate_expr(expr: &Expr, scope: &mut Env) -> syn::Result<usize>
where
{
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
    ret
}

pub fn translate_path_expr(expr: &ExprPath, scope: &mut Env) -> syn::Result<usize>
where
{
    let values = expr.parse_type_var(scope)?;
    let id = scope.push(values);
    Ok(id)
}

pub fn translate_tuple_expr(tuple: &ExprTuple, scope: &mut Env) -> syn::Result<usize>
where
{
    // translate each element
    let elems_ids: Vec<_> = tuple
        .elems
        .iter()
        .map(|expr| translate_expr(expr, scope))
        .try_collect()?;
    let elems_vec = elems_ids
        .into_iter()
        .map(|id| scope.pop(id))
        .collect::<Vec<_>>()
        .transpose_inplace();

    // merge tokens from each element
    let types: Vec<_> = elems_vec
        .into_iter()
        .map(|elems| TypeVar::Tuple(TypeTupleVar { elems }))
        .collect();
    let id = scope.push(types);

    Ok(id)
}

pub fn translate_block_expr(block: &ExprBlock, scope: &mut Env) -> syn::Result<usize> {
    translate_block(&block.block, scope)
}

pub fn translate_call_expr(call: &ExprCall, scope: &mut Env) -> syn::Result<usize>
where
{
    let ExprCall { func, args, .. } = call;

    // parse the function path to a trait path
    let trait_paths = match &**func {
        Expr::Path(ExprPath {
            qself: None, path, ..
        }) => path.parse_path_var(scope)?,
        _ => return Err(Error::new(func.span(), "not a trait")),
    };
    for path in trait_paths.iter() {
        match path.segments.last().unwrap().arguments {
            _ => {
                return Err(Error::new(
                    func.span(),
                    "type parameters are not allowed in trait call",
                ));
            }
            PathArgumentsVar::None => (),
        }
    }
    let trait_paths_id = scope.push(trait_paths);

    // parse arguments
    let arg_ids: Vec<_> = args
        .iter()
        .map(|arg| translate_expr(arg, scope))
        .try_collect()?;
    let args_vec: Vec<_> = arg_ids
        .into_iter()
        .map(|id| scope.pop(id))
        .collect::<Vec<_>>()
        .transpose_inplace();
    let trait_paths = scope.pop(trait_paths_id);

    // insert predicates
    let (outputs, predicates): (Vec<_>, Vec<_>) = trait_paths
        .into_iter()
        .zip_eq(args_vec)
        .map(|(trait_, args)| {
            let mut trait_ = trait_.into_path().unwrap();
            let args: Vec<_> = args
                .into_iter()
                .map(|arg| arg.into_type().unwrap())
                .collect();

            // set the type arguments on trait
            trait_.segments.last().as_mut().unwrap().arguments =
                PathArgumentsVar::AngleBracketed(args);

            // construct the output type
            let bounded_ty: TypeVar = syn::parse2(quote! { () }).unwrap();
            let output = {
                let position = trait_.segments.len();
                let path = {
                    let mut path = trait_.clone();
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
                    path: trait_,
                })],
            });

            (output, predicate)
        })
        .unzip();

    scope.insert_predicate(predicates);
    let output_id = scope.push(outputs);

    Ok(output_id)
}
