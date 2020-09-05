use super::*;

pub fn translate_binary_expr(
    ExprBinary {
        left, right, op, ..
    }: &ExprBinary,
    scope: &mut Env,
) -> syn::Result<usize>
where
{
    let lhs_id = translate_expr(left, scope)?;
    let rhs_id = translate_expr(right, scope)?;

    let std_bin_op = |scope: &mut Env, op_trait: TokenStream, lhs_id: usize, rhs_id: usize| {
        let lhs_ty = scope.pop(lhs_id);
        let rhs_ty = scope.pop(rhs_id);

        let (expanded, predicates): (Vec<_>, Vec<_>) = lhs_ty
            .into_iter()
            .zip_eq(rhs_ty)
            .map(|(lhs, rhs)| {
                let trait_pattern = quote! { #op_trait <#rhs> };
                let expanded = quote! {
                    < #lhs as #trait_pattern > :: Output
                };
                (expanded, (lhs, trait_pattern))
            })
            .unzip();

        scope.insert_trait_bounds(predicates);
        scope.push(expanded)
    };

    let typenum_bin_op = |scope: &mut Env, op_trait: TokenStream, lhs_id: usize, rhs_id: usize| {
        let lhs_ty = scope.pop(lhs_id);
        let rhs_ty = scope.pop(rhs_id);

        let (expanded, predicates): (Vec<_>, Vec<_>) = lhs_ty
            .into_iter()
            .zip_eq(rhs_ty)
            .map(|(lhs, rhs)| {
                let trait_pattern = quote! { #op_trait <#rhs> };
                let expanded = quote! {
                    < #lhs as #trait_pattern > :: Output
                };
                let binop_predicate = (lhs, trait_pattern);

                (expanded, binop_predicate)
            })
            .unzip();

        scope.insert_trait_bounds(predicates);
        scope.push(expanded)
    };

    let output_id = match op {
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
    };

    Ok(output_id)
}
