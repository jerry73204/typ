use super::*;

pub fn translate_binary_expr(
    ExprBinary {
        left, right, op, ..
    }: &ExprBinary,
    scope: &mut Env,
) -> syn::Result<usize>
where
{
    // parse lhs and rhs
    let lhs_id = translate_expr(left, scope)?;
    let rhs_id = translate_expr(right, scope)?;

    // compute output
    match op {
        BinOp::Add(_) => std_bin_op(scope, quote! { core::ops::Add }, lhs_id, rhs_id),
        BinOp::Sub(_) => std_bin_op(scope, quote! { core::ops::Sub }, lhs_id, rhs_id),
        BinOp::Div(_) => std_bin_op(scope, quote! { core::ops::Div }, lhs_id, rhs_id),
        BinOp::Mul(_) => std_bin_op(scope, quote! { core::ops::Mul }, lhs_id, rhs_id),
        BinOp::And(_) => std_bin_op(scope, quote! { core::ops::BitAnd }, lhs_id, rhs_id),
        BinOp::Or(_) => std_bin_op(scope, quote! { core::ops::BitOr }, lhs_id, rhs_id),
        BinOp::BitAnd(_) => std_bin_op(scope, quote! { core::ops::BitAnd }, lhs_id, rhs_id),
        BinOp::BitOr(_) => std_bin_op(scope, quote! { core::ops::BitOr }, lhs_id, rhs_id),
        BinOp::BitXor(_) => std_bin_op(scope, quote! { core::ops::BitXor }, lhs_id, rhs_id),
        BinOp::Rem(_) => std_bin_op(scope, quote! { core::ops::Rem }, lhs_id, rhs_id),
        BinOp::Lt(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsLess },
            lhs_id,
            rhs_id,
        ),
        BinOp::Gt(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsGreater },
            lhs_id,
            rhs_id,
        ),
        BinOp::Le(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsLessOrEqual },
            lhs_id,
            rhs_id,
        ),
        BinOp::Ge(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsGreaterOrEqual },
            lhs_id,
            rhs_id,
        ),
        BinOp::Eq(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsEqual },
            lhs_id,
            rhs_id,
        ),
        BinOp::Ne(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsNotEqual },
            lhs_id,
            rhs_id,
        ),
        _ => {
            return Err(Error::new(
                op.span(),
                "the binary operator is not supported",
            ))
        }
    }
}

fn std_bin_op(
    scope: &mut Env,
    trait_tokens: TokenStream,
    lhs_id: usize,
    rhs_id: usize,
) -> syn::Result<usize> {
    let lhs_vec: Vec<_> = scope
        .pop(lhs_id)
        .into_iter()
        .map(|var| var.into_type().unwrap())
        .collect();
    let rhs_vec: Vec<_> = scope
        .pop(rhs_id)
        .into_iter()
        .map(|var| var.into_type().unwrap())
        .collect();
    let trait_path: PathVar = syn::parse2(trait_tokens)?;

    let (outputs, predicates): (Vec<_>, Vec<_>) = lhs_vec
        .into_iter()
        .zip_eq(rhs_vec)
        .map(|(lhs, rhs)| {
            let trait_ = {
                let mut path = trait_path.clone();
                path.segments.last_mut().as_mut().unwrap().arguments =
                    PathArgumentsVar::AngleBracketed(vec![rhs]);
                path
            };
            let path = {
                let mut path = trait_.clone();
                path.segments.push(SegmentVar {
                    ident: format_ident!("Output"),
                    arguments: PathArgumentsVar::None,
                });
                path
            };
            let predicate = WherePredicateVar::Type(PredicateTypeVar {
                bounded_ty: lhs.clone(),
                bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                    modifier: TraitBoundModifierVar::None,
                    path: trait_.clone(),
                })],
            });
            let output = TypeVar::Path(TypePathVar {
                qself: Some(QSelfVar {
                    ty: Box::new(lhs),
                    position: trait_.segments.len(),
                }),
                path,
            });
            (output, predicate)
        })
        .unzip();

    scope.insert_predicate(predicates);
    let output_id = scope.push(outputs);

    Ok(output_id)
}

fn typenum_bin_op(
    scope: &mut Env,
    trait_tokens: TokenStream,
    lhs_id: usize,
    rhs_id: usize,
) -> syn::Result<usize> {
    let lhs_vec: Vec<_> = scope
        .pop(lhs_id)
        .into_iter()
        .map(|var| var.into_type().unwrap())
        .collect();
    let rhs_vec: Vec<_> = scope
        .pop(rhs_id)
        .into_iter()
        .map(|var| var.into_type().unwrap())
        .collect();
    let trait_path: PathVar = syn::parse2(trait_tokens)?;

    let (outputs, predicates): (Vec<_>, Vec<_>) = lhs_vec
        .into_iter()
        .zip_eq(rhs_vec)
        .map(|(lhs, rhs)| {
            let trait_ = {
                let mut path = trait_path.clone();
                path.segments.last_mut().as_mut().unwrap().arguments =
                    PathArgumentsVar::AngleBracketed(vec![rhs]);
                path
            };
            let path = {
                let mut path = trait_.clone();
                path.segments.push(SegmentVar {
                    ident: format_ident!("Output"),
                    arguments: PathArgumentsVar::None,
                });
                path
            };
            let output = TypeVar::Path(TypePathVar {
                qself: Some(QSelfVar {
                    ty: Box::new(lhs.clone()),
                    position: trait_.segments.len(),
                }),
                path,
            });
            let apply_predicate = WherePredicateVar::Type(PredicateTypeVar {
                bounded_ty: lhs,
                bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                    modifier: TraitBoundModifierVar::None,
                    path: trait_,
                })],
            });
            let output_predicate = WherePredicateVar::Type(PredicateTypeVar {
                bounded_ty: output.clone(),
                bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                    modifier: TraitBoundModifierVar::None,
                    path: syn::parse2(quote! { typenum::marker_traits::Bit }).unwrap(),
                })],
            });
            (output, (apply_predicate, output_predicate))
        })
        .unzip();
    let (apply_predicates, output_predicates): (Vec<_>, Vec<_>) = predicates.into_iter().unzip();

    scope.insert_predicate(apply_predicates);
    scope.insert_predicate(output_predicates);
    let output_id = scope.push(outputs);

    Ok(output_id)
}
