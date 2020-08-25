use super::*;

pub fn translate_enum(_enum_: &ItemEnum) -> syn::Result<TokenStream> {
    todo!();
    // let ItemEnum {
    //     vis,
    //     ident: trait_name,
    //     variants,
    //     ..
    // } = enum_;

    // let types: Vec<_> = variants
    //     .iter()
    //     .map(|variant| -> syn::Result<_> {
    //         let Variant {
    //             ident: type_name,
    //             fields,
    //             ..
    //         } = variant;

    //         let generics = match fields {
    //             Fields::Unit => vec![],
    //             Fields::Unnamed(unnamed) => {
    //                 let generics: Vec<_> = unnamed
    //                     .unnamed
    //                     .iter()
    //                     .enumerate()
    //                     .map(|(index, field)| -> syn::Result<_> {
    //                         let Field { ty, .. } = field;
    //                         let generic_name = format_ident!("T{}", index);
    //                         let trait_bounds = ty_to_trait_bounds(ty)?;

    //                         Ok((generic_name, trait_bounds))
    //                     })
    //                     .try_collect()?;

    //                 generics
    //             }
    //             Fields::Named(named) => {
    //                 let generics: Vec<_> = named
    //                     .named
    //                     .iter()
    //                     .map(|field| -> syn::Result<_> {
    //                         let Field { ident, ty, .. } = field;
    //                         let generic_name = ident.to_owned().unwrap();
    //                         let trait_bounds = ty_to_trait_bounds(ty)?;

    //                         Ok((generic_name.to_owned(), trait_bounds))
    //                     })
    //                     .try_collect()?;

    //                 generics
    //             }
    //         };

    //         Ok((type_name, generics))
    //     })
    //     .try_collect()?;

    // let impls = {
    //     let items: Vec<_> = types
    //         .into_iter()
    //         .map(|(type_name, generics)| {
    //             let generic_args = {
    //                 let names: Vec<_> = generics.iter().map(|(name, _)| name).collect();
    //                 quote! {
    //                     #(#names),*
    //                 }
    //             };

    //             let where_clause = {
    //                 let bounds: Vec<_> = generics
    //                     .iter()
    //                     .filter_map(|(name, bounds)| {
    //                         if bounds.is_empty() {
    //                             None
    //                         } else {
    //                             Some(quote! {
    //                                 #name: #(#bounds)+*
    //                             })
    //                         }
    //                     })
    //                     .collect();

    //                 quote! {
    //                     #(#bounds),*
    //                 }
    //             };

    //             quote! {
    //                 #vis struct #type_name<#generic_args>
    //                 where
    //                     #where_clause
    //                 {
    //                     _phantom: ::core::marker::PhantomData<#generic_args>
    //                 }

    //                 impl<#generic_args> #trait_name for #type_name<#generic_args>
    //                 where
    //                     #where_clause
    //                 {}
    //             }
    //         })
    //         .collect();

    //     quote! {
    //         #(#items)*
    //     }
    // };

    // let expanded = quote! {
    //     #vis trait #trait_name {}

    //     #impls
    // };

    // Ok(expanded)
}
