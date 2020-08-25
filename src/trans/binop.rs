use super::*;

pub fn translate_binary_expr(
    ExprBinary {
        left, right, op, ..
    }: &ExprBinary,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let left_ty = translate_expr(left, scope, env)?;
    let right_ty = translate_expr(right, scope, env)?;

    let std_bin_op =
        |scope: &mut Scope, op_trait: TokenStream, left_ty: TokenStream, right_ty: TokenStream| {
            let trait_pattern = quote! { #op_trait < #right_ty > };
            let output = quote! {
                < #left_ty as #trait_pattern > :: Output
            };
            scope.insert_trait_bounds(left_ty, trait_pattern);
            output
        };

    let typenum_bin_op =
        |scope: &mut Scope, op_trait: TokenStream, left_ty: TokenStream, right_ty: TokenStream| {
            let trait_pattern = quote! { #op_trait < #right_ty > };
            let output = quote! {
                < #left_ty as #trait_pattern > :: Output
            };
            scope.insert_trait_bounds(left_ty, trait_pattern);
            scope.insert_trait_bounds(output.clone(), quote! { typenum::marker_traits::Bit });
            output
        };

    let output_ty = match op {
        BinOp::Add(_) => std_bin_op(scope, quote! { core::ops::Add }, left_ty, right_ty),
        BinOp::Sub(_) => std_bin_op(scope, quote! { core::ops::Sub }, left_ty, right_ty),
        BinOp::Div(_) => std_bin_op(scope, quote! { core::ops::Div }, left_ty, right_ty),
        BinOp::Mul(_) => std_bin_op(scope, quote! { core::ops::Mul }, left_ty, right_ty),
        BinOp::And(_) => std_bin_op(scope, quote! { core::ops::BitAnd }, left_ty, right_ty),
        BinOp::Or(_) => std_bin_op(scope, quote! { core::ops::BitOr }, left_ty, right_ty),
        BinOp::BitAnd(_) => std_bin_op(scope, quote! { core::ops::BitAnd }, left_ty, right_ty),
        BinOp::BitOr(_) => std_bin_op(scope, quote! { core::ops::BitOr }, left_ty, right_ty),
        BinOp::BitXor(_) => std_bin_op(scope, quote! { core::ops::BitXor }, left_ty, right_ty),
        BinOp::Lt(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsLess },
            left_ty,
            right_ty,
        ),
        BinOp::Gt(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsGreater },
            left_ty,
            right_ty,
        ),
        BinOp::Le(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsLessOrEqual },
            left_ty,
            right_ty,
        ),
        BinOp::Ge(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsGreaterOrEqual },
            left_ty,
            right_ty,
        ),
        BinOp::Eq(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsEqual },
            left_ty,
            right_ty,
        ),
        BinOp::Ne(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsNotEqual },
            left_ty,
            right_ty,
        ),
        _ => {
            return Err(Error::new(
                op.span(),
                "the binary operator is not supported",
            ))
        }
    };

    Ok(output_ty)
}
