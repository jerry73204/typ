use super::*;

pub fn translate_impl(impl_: &ItemImpl) -> syn::Result<TokenStream> {
    let ItemImpl {
        defaultness,
        unsafety,
        generics,
        trait_,
        self_ty,
        items,
        ..
    } = impl_;

    // sanity check
    if let Some(defaultness) = defaultness {
        return Err(Error::new(
            defaultness.span(),
            "default keyword is not supported",
        ));
    }
    if let Some(unsafety) = unsafety {
        return Err(Error::new(
            unsafety.span(),
            "unsafe keyword is not supported",
        ));
    }
    if trait_.is_some() {
        return Err(Error::new(
            impl_.span(),
            r#""for Trait" clause is not supported"#,
        ));
    }

    let items_tokens: Vec<_> = items
        .iter()
        .map(|item| -> syn::Result<_> {
            let tokens = match item {
                ImplItem::Method(method) => {
                    let ImplItemMethod {
                        sig, block, vis, ..
                    } = method;
                    translate_fn(vis, sig, block, Some(&**self_ty), Some(generics))?
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
