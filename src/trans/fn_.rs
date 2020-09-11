use super::*;

pub fn translate_fn(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
    self_ty: Option<&Path>,
    impl_generics: Option<&Generics>,
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
    let mut env = Env::new(fn_name.clone());

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
    let (_input_type_params, _input_lifetimes, _input_const_params) = {
        // create initial quantifiers
        for param in generics.params.iter() {
            if let GenericParam::Type(TypeParam { ident, .. }) = param {
                env.insert_free_quantifier(ident.to_owned());
            }
        }

        // insert trait bounds
        for param in generics.params.iter() {
            let predicate = param.parse_where_predicate_var(&env)?;
            env.insert_predicate(predicate);
        }

        if let Some(where_clause) = &generics.where_clause {
            for predicate in where_clause.predicates.iter() {
                let predicate = predicate.parse_where_predicate_var(&env)?;
                env.insert_predicate(predicate);
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
                let predicate = pat_type.parse_where_predicate_var(&env)?;
                env.insert_predicate(predicate);
            }
        }

        let fn_args: Vec<_> = inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(pat_type),
                FnArg::Receiver(_) => None,
            })
            .map(|PatType { pat, .. }| pat.parse_type_var(&env))
            .try_collect()?;

        fn_args
    };

    // translate output type to trait bound
    let output_bounds = match output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => {
            let bounds = ty.parse_type_param_bounds_var(&env)?;
            Some(bounds)
        }
    };

    // translate block
    let mut items = vec![];
    let output = translate_block(&block, &mut env, &mut items)?;

    // insert trait bound for output type
    if let Some(bounds) = &output_bounds {
        let predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty: output.clone(),
            bounds: bounds.to_owned(),
        });

        env.insert_predicate(predicate);
    }

    // generate generics
    let free_quantifiers = env.free_quantifiers();
    let subsitution: IndexMap<_, _> = free_quantifiers
        .iter()
        .cloned()
        .enumerate()
        .map(|(index, var)| (var, format_ident!("{}GENERIC_{}", IDENT_PREFIX, index)))
        .collect();
    let input_generics: Vec<_> = subsitution.values().collect();

    // generate trait names
    let trait_name = format_ident!("{}", fn_name);

    // generate trait
    let trait_item: ItemTrait = {
        let num_args = fn_args.len();
        let args: Vec<_> = (0..num_args)
            .map(|idx| format_ident!("{}ARG_{}", IDENT_PREFIX, idx))
            .collect();
        let output_predicate = output_bounds.as_ref().map(|bounds| {
            WherePredicateVar::Type(PredicateTypeVar {
                bounded_ty: syn::parse2(quote! { Self::Output }).unwrap(),
                bounds: bounds.to_owned(),
            })
            .substitute(&env, &subsitution)
        });

        syn::parse2(quote! {
            #[allow(non_snake_case)]
            pub trait #trait_name < #(#args),* >
            where
                #output_predicate
            {
                type Output;
            }
        })?
    };

    // generate impl items
    let impl_item: ItemImpl = {
        let input_types: Vec<_> = fn_args
            .iter()
            .map(|arg| arg.substitute(&env, &subsitution))
            .collect();
        let predicates: Vec<_> = env
            .predicates()
            .into_iter()
            .map(|predicate| predicate.substitute(&env, &subsitution))
            .collect();
        let output = output.substitute(&env, &subsitution);

        syn::parse2(quote! {
            impl<#(#input_generics),*>  #trait_name< #(#input_types),* > for #self_ty_tokens
            where
                #(#predicates),*
            {
                type Output = #output;
            }
        })?
    };

    // push items to child module
    items.push(Item::Trait(trait_item));
    items.push(Item::Impl(impl_item));

    let expanded = {
        let num_args = fn_args.len();
        let args: Vec<_> = (0..num_args)
            .map(|idx| format_ident!("{}ARG_{}", IDENT_PREFIX, idx))
            .collect();
        let mod_name = format_ident!("{}mod_{}", IDENT_PREFIX, fn_name);
        let type_name = format_ident!("{}Op", fn_name);

        quote! {
            #vis use #mod_name :: #trait_name;
            #vis type #type_name<#(#args),*> = < () as #trait_name <#(#args),*> > :: Output;

            #[allow(non_snake_case)]
            mod #mod_name {
                use super::*;

                #(#items)*
            }
        }
    };

    Ok(expanded)
}
