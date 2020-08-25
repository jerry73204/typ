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
    let mut scope = Scope::new();

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
        ) => return Err(Error::new(receiver.span(), r#""&self" is not supported"#)),
        (None, Some(FnArg::Receiver(receiver @ Receiver { .. }))) => {
            return Err(Error::new(
                receiver.span(),
                "self receiver is not accepted in the impl block without self trait",
            ))
        }
        _ => quote! { () },
    };

    // translate function generics to initial quantifiers
    let generic_idents = {
        // function generics (ident, [path])
        let generic_params: Vec<(&Ident, _)> = generics
            .params
            .iter()
            .map(|param| match param {
                GenericParam::Type(TypeParam { ident, bounds, .. }) => Ok((ident, bounds)),
                GenericParam::Lifetime(lifetime) => {
                    Err(Error::new(lifetime.span(), "lifetime is not allowed"))
                }
                GenericParam::Const(const_) => {
                    Err(Error::new(const_.span(), "const generics is not allowed"))
                }
            })
            .try_collect()?;

        // create initial quantifiers
        for (ident, _paths) in generic_params.iter().cloned() {
            scope.insert_free_quantifier(ident.to_owned())?;
        }

        // add trait bounds for initial quantifiers
        for (ident, bounds) in generic_params.iter().cloned() {
            let predicate = scope.type_var_builder().from_ident(ident);
            let bounds = scope
                .trait_bounds_var_builder()
                .from_type_param_bounds(&bounds)?;
            scope.insert_trait_bounds(quote! { #predicate }, quote! { #bounds });
        }

        let generaic_idents: Vec<_> = generic_params
            .iter()
            .map(|(ident, _)| ident.to_owned())
            .collect();
        generaic_idents
    };

    // turn function arguments into trait bounds
    let fn_args = {
        // get function arguments (type, trait bounds)
        let fn_args: Vec<_> = inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(pat_type),
                FnArg::Receiver(_) => None,
            })
            .map(|PatType { pat, ty, .. }| -> syn::Result<_> {
                let type_ = scope.type_var_builder().from_pat(&**pat)?;
                let bounds = scope.trait_bounds_var_builder().from_type(&**ty)?;
                Ok((quote! { #type_ }, quote! { #bounds }))
            })
            .try_collect()?;

        // insert trait bounds
        for (predicate, bounds) in fn_args.iter() {
            scope.insert_trait_bounds(predicate.to_owned(), bounds.to_owned());
        }

        fn_args
    };

    // translate output type to trait bound
    let output_bounds = match output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => Some(scope.trait_bounds_var_builder().from_type(ty)?),
    };

    // translate block
    let block_tokens = translate_block(&block, &mut scope, &mut sub_env)?;

    // insert trait bound for output type
    if let Some(bounds) = &output_bounds {
        scope.insert_trait_bounds(quote! { #block_tokens }, quote! { #bounds });
    }

    // generate trait bounds tokens
    let trait_bounds_tokens = scope.generate_trait_bounds_tokens();

    // generate trait
    let trait_tokens = {
        let place_holders: Vec<_> = fn_args
            .iter()
            .enumerate()
            .map(|(idx, _)| format_ident!("{}{}", IDENT_PREFIX, idx))
            .collect();
        let output_bounds_tokens = output_bounds
            .as_ref()
            .map(|bounds| quote! { Self::Output : #bounds });

        quote! {
            pub trait #fn_name < #(#place_holders),* >
            where
                #output_bounds_tokens
            {
                type Output;
            }
        }
    };

    // generate impl
    let impl_tokens = {
        let input_types: Vec<_> = fn_args.iter().map(|(ty, _bounds)| ty).collect();

        quote! {
            impl< #(#generic_idents),* >  #fn_name< #(#input_types),* > for #self_ty_tokens
            where
                #trait_bounds_tokens
            {
                type Output = #block_tokens;
            }
        }
    };

    // generate output tokens
    sub_env.add_item(quote! { use super::*; });
    sub_env.add_item(trait_tokens);
    sub_env.add_item(impl_tokens);
    env.add_item(quote! { #vis use #mod_name :: #fn_name ; });

    Ok(quote! { #env })
}
