use super::*;

pub fn translate_impl(impl_: &ItemImpl) -> syn::Result<TokenStream> {
    let ItemImpl {
        trait_,
        self_ty,
        items,
        ..
    } = impl_;

    if trait_.is_some() {
        return Err(Error::new(
            impl_.span(),
            r#""for Trait" clause is not supported"#,
        ));
    }

    let trait_path = match &**self_ty {
        Type::Path(type_path) => &type_path.path,
        _ => return Err(Error::new(self_ty.span(), "unsupported type kind")),
    };

    let items_tokens: Vec<_> = items
        .iter()
        .map(|item| -> syn::Result<_> {
            let tokens = match item {
                ImplItem::Method(method) => {
                    let ImplItemMethod {
                        sig, block, vis, ..
                    } = method;
                    translate_fn(vis, sig, block, Some(trait_path))?
                }
                _ => {
                    return Err(Error::new(item.span(), "unsupported item"));
                }
            };

            Ok(tokens)
        })
        .try_collect()?;

    let expanded = quote! { #(#items_tokens)* };
    Ok(expanded)
}
