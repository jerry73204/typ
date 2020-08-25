use super::*;

pub fn translate_unary_expr(
    ExprUnary { op, expr, .. }: &ExprUnary,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream> {
    let operand_ty = translate_expr(expr, scope, env)?;
    let output_ty = match op {
        UnOp::Neg(_) => {
            let trait_ = quote! { core::ops::Neg };
            let output = quote! { < #operand_ty as #trait_ > :: Output };
            scope.insert_trait_bounds(operand_ty.clone(), trait_);
            output
        }
        UnOp::Not(_) => {
            let trait_ = quote! { core::ops::Not };
            let output = quote! { < #operand_ty as #trait_ > :: Output };
            scope.insert_trait_bounds(operand_ty, trait_);
            output
        }
        _ => return Err(Error::new(op.span(), "unsupported unary operator")),
    };

    Ok(output_ty)
}
