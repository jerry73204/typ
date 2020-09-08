use super::*;

pub fn translate_fn(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
    self_ty: Option<&Path>,
) -> syn::Result<TokenStream> {
    let Signature {
        ident: fn_name,
        generics,
        inputs,
        output,
        constness,
        asyncness,
        unsafety,
        variadic,
        ..
    } = sig;

    // sanity checks
    if let Some(const_) = constness {
        return Err(Error::new(const_.span(), "the keyword is not supported"));
    }

    if let Some(async_) = asyncness {
        return Err(Error::new(async_.span(), "the keyword is not supported"));
    }

    if let Some(unsafe_) = unsafety {
        return Err(Error::new(unsafe_.span(), "the keyword is not supported"));
    }

    if let Some(var) = variadic {
        return Err(Error::new(var.span(), "variadic argument is not supported"));
    }

    // create root scope
    let mut scope = Env::new(fn_name.clone());

    // check if "self" is present in the input arguments and is consistent with impl block
    let self_ty_tokens = match (self_ty, inputs.first()) {
        (
            Some(self_ty),
            Some(FnArg::Receiver(Receiver {
                reference: None, ..
            })),
        ) => quote! { #self_ty },
        (
            Some(_self_ty),
            Some(
                receiver @ FnArg::Receiver(Receiver {
                    reference: Some(_), ..
                }),
            ),
        ) => {
            return Err(Error::new(
                receiver.span(),
                r#"referenced receiver "&self" is not supported"#,
            ))
        }
        (None, Some(FnArg::Receiver(receiver @ Receiver { .. }))) => {
            return Err(Error::new(
                receiver.span(),
                "self receiver is not accepted in the impl block without self trait",
            ))
        }
        _ => quote! { () },
    };

    // translate function generics to initial quantifiers
    let (input_type_params, input_lifetimes, input_const_params) = {
        // create initial quantifiers
        for param in generics.params.iter() {
            if let GenericParam::Type(TypeParam { ident, bounds, .. }) = param {
                scope.insert_free_quantifier(ident.to_owned());
            }
        }

        // insert trait bounds
        for param in generics.params.iter() {
            let predicate = param.parse_where_predicate_var(&scope)?;
            scope.insert_predicate(predicate);
        }

        if let Some(where_clause) = &generics.where_clause {
            for predicate in where_clause.predicates.iter() {
                let predicate = predicate.parse_where_predicate_var(&scope)?;
                scope.insert_predicate(predicate);
            }
        }

        let input_type_params: Vec<_> = generics
            .params
            .iter()
            .filter_map(|param| match param {
                GenericParam::Type(param) => Some(param),
                _ => None,
            })
            .map(|TypeParam { ident, default, .. }| (ident, default))
            .collect();

        let input_lifetimes: Vec<_> = generics
            .params
            .iter()
            .filter_map(|param| match param {
                GenericParam::Lifetime(param) => Some(param),
                _ => None,
            })
            .map(
                |LifetimeDef {
                     lifetime, bounds, ..
                 }| (lifetime, bounds),
            )
            .collect();

        let input_const_params: Vec<_> = generics
            .params
            .iter()
            .filter_map(|param| match param {
                GenericParam::Const(param) => Some(param),
                _ => None,
            })
            .map(
                |ConstParam {
                     ident, ty, default, ..
                 }| (ident, ty, default),
            )
            .collect();

        (input_type_params, input_lifetimes, input_const_params)
    };

    // translate function arguments into types and trait bounds
    let fn_args = {
        // insert trait bounds
        for arg in inputs.iter() {
            if let FnArg::Typed(pat_type) = arg {
                let predicate = pat_type.parse_where_predicate_var(&scope)?;
                scope.insert_predicate(predicate);
            }
        }

        let fn_args: Vec<_> = inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(pat_type),
                FnArg::Receiver(_) => None,
            })
            .map(|PatType { pat, .. }| pat.parse_type_var(&scope))
            .try_collect()?;

        fn_args
    };

    // translate output type to trait bound
    let output_bounds = match output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => {
            let bounds = ty.parse_type_param_bounds_var(&scope)?;
            Some(Rc::new(bounds))
        }
    };

    // translate block
    let output = translate_block(&block, &mut scope)?;

    // insert trait bound for output type
    // dbg!();
    // if let Some(bounds) = &output_bounds {
    //     let predicates: Vec<_> = outputs_per_branch
    //         .iter()
    //         .cloned()
    //         .map(|output| (output, bounds.clone()))
    //         .collect();
    //     scope.insert_trait_bounds(predicates);
    // }

    // generate trait names
    let pub_type_name = format_ident!("{}Op", fn_name);
    let pub_trait_name = format_ident!("{}", fn_name);
    let priv_trait_name = format_ident!("{}{}", IDENT_PREFIX, fn_name);

    // generate trait
    // let pub_trait_item = {
    //     let num_placeholders = fn_args.len();

    //     let placeholders: Vec<_> = (0..num_placeholders)
    //         .map(|idx| format_ident!("{}{}", IDENT_PREFIX, idx))
    //         .collect();
    //     let output_predicate = output_bounds
    //         .as_ref()
    //         .map(|bounds| quote! { Self::Output : #bounds });

    //     quote! {
    //         #vis trait #pub_trait_name < #(#placeholders),* >
    //         where
    //             #output_predicate
    //         {
    //             type Output;
    //         }
    //     }
    // };

    // let priv_trait_item = {
    //     let num_placeholders = fn_args.len() + scope.num_conditions();

    //     let placeholders: Vec<_> = (0..num_placeholders)
    //         .map(|idx| format_ident!("{}{}", IDENT_PREFIX, idx))
    //         .collect();
    //     let output_predicate = output_bounds
    //         .as_ref()
    //         .map(|bounds| quote! { Self::Output : #bounds });

    //     quote! {
    //         #[allow(non_camel_case_types)]
    //         trait #priv_trait_name < #(#placeholders),* >
    //         where
    //             #output_predicate
    //         {
    //             type Output;
    //         }
    //     }
    // };

    // generate impl items
    // dbg!();
    // let (conditions, cond_targets_per_branch) = scope.conditions(); // conditions for each branch
    // let trait_bounds_per_branch = scope.trait_bounds(); // trait bounds for each branch

    // let pub_impl_item = {
    //     let input_types: Vec<_> = fn_args.iter().map(|(ty, _bounds)| ty).collect();
    //     let trait_pattern = quote! { #priv_trait_name<#(#input_types),* #(,#conditions)*> };
    //     let trait_bounds: Vec<_> = trait_bounds_per_branch
    //         .iter()
    //         .flatten()
    //         .map(|(ty, bounds)| quote! { #ty: #bounds })
    //         .chain(iter::once(quote! { #self_ty_tokens: #trait_pattern }))
    //         .collect();

    //     quote! {
    //         impl<#(#input_generics),*>  #pub_trait_name< #(#input_types),* > for #self_ty_tokens
    //         where
    //             #(#trait_bounds),*
    //         {
    //             type Output = <#self_ty_tokens as #trait_pattern>::Output;
    //         }
    //     }
    // };

    // let priv_impl_items: Vec<_> = outputs_per_branch
    //     .into_iter()
    //     .zip_eq(cond_targets_per_branch)
    //     .zip_eq(trait_bounds_per_branch)
    //     .map(|((output, cond_targets), trait_bounds)| {
    //         let generics = {
    //             let cond_generics =
    //                 cond_targets
    //                     .iter()
    //                     .enumerate()
    //                     .flat_map(|(idx, target)| match target {
    //                         Some(_) => None,
    //                         None => Some(format_ident!("{}{}", IDENT_PREFIX, idx)),
    //                     });
    //             let generics: Vec<_> = input_generics
    //                 .iter()
    //                 .cloned()
    //                 .map(ToOwned::to_owned)
    //                 .chain(cond_generics)
    //                 .collect();
    //             generics
    //         };
    //         let input_types = {
    //             let cond_input_types =
    //                 cond_targets
    //                     .iter()
    //                     .enumerate()
    //                     .map(|(idx, target)| match target {
    //                         Some(tar) => quote! { #tar },
    //                         None => {
    //                             let generic = format_ident!("{}{}", IDENT_PREFIX, idx);
    //                             quote! { #generic }
    //                         }
    //                     });
    //             let input_types: Vec<_> = fn_args
    //                 .iter()
    //                 .map(|(ty, _bounds)| ty.to_owned())
    //                 .chain(cond_input_types)
    //                 .collect();
    //             input_types
    //         };
    //         let trait_bounds_predicates: Vec<_> = trait_bounds
    //             .iter()
    //             .map(|(ty, bounds)| quote! { #ty: #bounds })
    //             .collect();

    //         quote! {
    //             impl<#(#generics),*>  #priv_trait_name< #(#input_types),* > for #self_ty_tokens
    //             where
    //                 #(#trait_bounds_predicates),*
    //             {
    //                 type Output = #output;
    //             }
    //         }
    //     })
    //     .collect();

    // generate type alias
    // let pub_type_item = {
    //     let num_placeholders = fn_args.len();
    //     let placeholders: Vec<_> = (0..num_placeholders)
    //         .map(|idx| format_ident!("{}{}", IDENT_PREFIX, idx))
    //         .collect();

    //     quote! {
    //         #vis type #pub_type_name < #(#placeholders),* > = <() as #pub_trait_name < #(#placeholders),* > >::Output;
    //     }
    // };

    // combine all items
    // let expanded = quote! {
    //     #pub_type_item

    //     #pub_trait_item

    //     #pub_impl_item

    //     #priv_trait_item

    //     #(#priv_impl_items)*
    // };

    // Ok(expanded)
    todo!();
}
