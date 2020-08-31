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

    // create root scope and env
    let mut env = Env::new();
    let mod_name = format_ident!("{}mod_{}", IDENT_PREFIX, fn_name);
    let mut sub_env = env
        .create_mod(mod_name.clone())
        .expect("please report bug: the mod name is taken");
    let mut scope = ScopeSet::new();

    dbg!();
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
    dbg!();
    let input_generics = {
        // function generics (ident, [path])
        let generic_params: Vec<_> = generics
            .params
            .iter()
            .map(|param| match param {
                GenericParam::Type(TypeParam { ident, bounds, .. }) => Ok((ident, bounds)),
                GenericParam::Lifetime(lifetime) => {
                    Err(Error::new(lifetime.span(), "lifetime is not supported"))
                }
                GenericParam::Const(const_) => {
                    Err(Error::new(const_.span(), "const generics is not supported"))
                }
            })
            .try_collect()?;

        // create initial quantifiers
        dbg!();
        for (ident, _bounds) in generic_params.iter().cloned() {
            scope.insert_free_quantifier(ident.to_owned())?;
        }

        // add trait bounds for initial quantifiers
        dbg!();
        for (ident, bounds) in generic_params.iter().cloned() {
            let ty = ident.into_pure_type()?;
            let bounds = bounds.into_pure_trait_bounds()?;
            scope.insert_trait_bounds(vec![(ty, bounds)]);
        }

        dbg!();
        let generaic_idents: Vec<_> = generic_params
            .iter()
            .map(|(ident, _)| ident.to_owned())
            .collect();
        generaic_idents
    };

    // translate function arguments into types and trait bounds
    dbg!();
    let fn_args = {
        // get function arguments (type, trait bounds)
        let fn_args: Vec<_> = inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(pat_type),
                FnArg::Receiver(_) => None,
            })
            .map(|PatType { pat, ty, .. }| -> syn::Result<_> {
                let type_ = pat.into_pure_type()?;
                let bounds = ty.into_pure_trait_bounds()?;
                Ok((type_, bounds))
            })
            .try_collect()?;

        // insert trait bounds
        for predicate in fn_args.iter().cloned() {
            scope.insert_trait_bounds(vec![predicate]);
        }

        fn_args
    };

    // translate output type to trait bound
    let output_bounds = match output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => {
            let bounds = ty.into_pure_trait_bounds()?;
            Some(Rc::new(bounds))
        }
    };

    // translate block
    dbg!();
    let outputs_id = translate_block(&block, &mut scope, &mut sub_env)?;
    dbg!();
    let outputs_per_branch = scope.pop(outputs_id);

    // insert trait bound for output type
    dbg!();
    if let Some(bounds) = &output_bounds {
        let predicates: Vec<_> = outputs_per_branch
            .iter()
            .cloned()
            .map(|output| (output, bounds.clone()))
            .collect();
        scope.insert_trait_bounds(predicates);
    }

    // generate trait names
    dbg!();
    let pub_type_name = format_ident!("{}Op", fn_name);
    let pub_trait_name = format_ident!("{}", fn_name);
    let priv_trait_name = format_ident!("{}{}", IDENT_PREFIX, fn_name);

    // generate trait
    let pub_trait_item = {
        let num_placeholders = fn_args.len();

        let placeholders: Vec<_> = (0..num_placeholders)
            .map(|idx| format_ident!("{}{}", IDENT_PREFIX, idx))
            .collect();
        let output_predicate = output_bounds
            .as_ref()
            .map(|bounds| quote! { Self::Output : #bounds });

        quote! {
            #vis trait #pub_trait_name < #(#placeholders),* >
            where
                #output_predicate
            {
                type Output;
            }
        }
    };

    let priv_trait_item = {
        let num_placeholders = fn_args.len() + scope.num_conditions();

        let placeholders: Vec<_> = (0..num_placeholders)
            .map(|idx| format_ident!("{}{}", IDENT_PREFIX, idx))
            .collect();
        let output_predicate = output_bounds
            .as_ref()
            .map(|bounds| quote! { Self::Output : #bounds });

        quote! {
            #[allow(non_camel_case_types)]
            trait #priv_trait_name < #(#placeholders),* >
            where
                #output_predicate
            {
                type Output;
            }
        }
    };

    // generate impl items
    dbg!();
    let (conditions, cond_targets_per_branch) = scope.conditions(); // conditions for each branch
    let trait_bounds_per_branch = scope.trait_bounds(); // trait bounds for each branch

    let pub_impl_item = {
        let input_types: Vec<_> = fn_args.iter().map(|(ty, _bounds)| ty).collect();
        let trait_pattern = quote! { #priv_trait_name<#(#input_types),* #(,#conditions)*> };
        let trait_bounds: Vec<_> = trait_bounds_per_branch
            .iter()
            .flatten()
            .map(|(ty, bounds)| quote! { #ty: #bounds })
            .chain(iter::once(quote! { #self_ty_tokens: #trait_pattern }))
            .collect();

        quote! {
            impl<#(#input_generics),*>  #pub_trait_name< #(#input_types),* > for #self_ty_tokens
            where
                #(#trait_bounds),*
            {
                type Output = <#self_ty_tokens as #trait_pattern>::Output;
            }
        }
    };

    let priv_impl_items: Vec<_> = outputs_per_branch
        .into_iter()
        .zip_eq(cond_targets_per_branch)
        .zip_eq(trait_bounds_per_branch)
        .map(|((output, cond_targets), trait_bounds)| {
            let generics = {
                let cond_generics =
                    cond_targets
                        .iter()
                        .enumerate()
                        .flat_map(|(idx, target)| match target {
                            Some(_) => None,
                            None => Some(format_ident!("{}{}", IDENT_PREFIX, idx)),
                        });
                let generics: Vec<_> = input_generics
                    .iter()
                    .cloned()
                    .map(ToOwned::to_owned)
                    .chain(cond_generics)
                    .collect();
                generics
            };
            let input_types = {
                let cond_input_types =
                    cond_targets
                        .iter()
                        .enumerate()
                        .map(|(idx, target)| match target {
                            Some(tar) => quote! { #tar },
                            None => {
                                let generic = format_ident!("{}{}", IDENT_PREFIX, idx);
                                quote! { #generic }
                            }
                        });
                let input_types: Vec<_> = fn_args
                    .iter()
                    .map(|(ty, _bounds)| ty.to_owned())
                    .chain(cond_input_types)
                    .collect();
                input_types
            };
            let trait_bounds_predicates: Vec<_> = trait_bounds
                .iter()
                .map(|(ty, bounds)| quote! { #ty: #bounds })
                .collect();

            quote! {
                impl<#(#generics),*>  #priv_trait_name< #(#input_types),* > for #self_ty_tokens
                where
                    #(#trait_bounds_predicates),*
                {
                    type Output = #output;
                }
            }
        })
        .collect();

    // generate type alias
    let pub_type_item = {
        let num_placeholders = fn_args.len();
        let placeholders: Vec<_> = (0..num_placeholders)
            .map(|idx| format_ident!("{}{}", IDENT_PREFIX, idx))
            .collect();

        quote! {
            #vis type #pub_type_name < #(#placeholders),* > = <() as #pub_trait_name < #(#placeholders),* > >::Output;
        }
    };

    dbg!();
    // combine all items
    let expanded = quote! {
        #pub_type_item

        #pub_trait_item

        #pub_impl_item

        #priv_trait_item

        #(#priv_impl_items)*
    };

    Ok(expanded)
}
