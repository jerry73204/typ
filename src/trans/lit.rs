use super::*;

pub fn translate_lit_expr(
    expr: &ExprLit,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar> {
    let ExprLit { lit, .. } = expr;

    // parse literal
    let lit_tokens = match lit {
        Lit::Bool(LitBool { value, .. }) => {
            if *value {
                quote! { typenum::B1 }
            } else {
                quote! { typenum::B0 }
            }
        }
        Lit::Int(int_) => match int_.suffix() {
            "" | "i" => {
                let value: u128 = int_.base10_parse()?;
                if value == 0 {
                    quote! { typenum::Z0 }
                } else {
                    let ty = int_to_typenum(value);
                    quote! { typenum::int::PInt<#ty> }
                }
            }
            "u" => {
                let value: u128 = int_.base10_parse()?;
                let ty = int_to_typenum(value);
                ty
            }
            _ => return Err(Error::new(int_.span(), "unsupported literal suffix")),
        },
        _ => return Err(Error::new(lit.span(), "unsupported literal")),
    };
    let lit_ty: TypeVar = syn::parse2(lit_tokens)?;

    Ok(lit_ty)
}

fn int_to_typenum(value: u128) -> TokenStream {
    if value == 0 {
        quote! {
            typenum::uint::UTerm
        }
    } else if value & 1 == 1 {
        let sub_tokens = int_to_typenum(value >> 1);
        quote! {
            typenum::uint::UInt<#sub_tokens, typenum::B1>
        }
    } else {
        let sub_tokens = int_to_typenum(value >> 1);
        quote! {
            typenum::uint::UInt<#sub_tokens, typenum::B0>
        }
    }
}
