use super::*;

pub fn translate_fn(
    vis: &Visibility,
    sig: &Signature,
    block: &Block,
    self_ty: Option<&Type>,
    impl_generics: Option<&Generics>,
) -> syn::Result<TokenStream> {
    let Signature {
        ident: fn_name,
        generics: fn_generics,
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

    // reject lifetime and const generics
    if let Some(impl_generics) = impl_generics {
        for param in impl_generics.params.iter() {
            match param {
                GenericParam::Type(TypeParam { .. }) => (),
                GenericParam::Lifetime(lifetime) => {
                    return Err(Error::new(lifetime.span(), "lifetime is not supported"))
                }
                GenericParam::Const(const_) => {
                    return Err(Error::new(const_.span(), "const generic is not supported"))
                }
            }
        }
    }

    for param in fn_generics.params.iter() {
        match param {
            GenericParam::Type(TypeParam { .. }) => (),
            GenericParam::Lifetime(lifetime) => {
                return Err(Error::new(lifetime.span(), "lifetime is not supported"))
            }
            GenericParam::Const(const_) => {
                return Err(Error::new(const_.span(), "const generic is not supported"))
            }
        }
    }

    // create root scope
    let mut env = Env::new(fn_name.clone());

    // check if impl and fn generic names coincide with each other
    if let Some(impl_generics) = impl_generics {
        let impl_type_params: HashSet<_> = impl_generics
            .params
            .iter()
            .map(|param| match param {
                GenericParam::Type(TypeParam { ident, .. }) => ident,
                _ => unreachable!("please report bug"),
            })
            .collect();

        for param in fn_generics.params.iter() {
            if let GenericParam::Type(TypeParam {
                ident: fn_ident, ..
            }) = param
            {
                if let Some(impl_ident) = impl_type_params.get(fn_ident) {
                    let mut err = Error::new(impl_ident.span(), "the generic is defined here");
                    err.combine(Error::new(
                        fn_ident.span(),
                        "the generic name must not coincide with generics from impl blocks",
                    ));
                    return Err(err);
                }
            }
        }
    }

    // insert free quantifiers and predicates from impl generics
    if let Some(impl_generics) = impl_generics {
        // create initial quantifiers
        for param in impl_generics.params.iter() {
            if let GenericParam::Type(TypeParam { ident, .. }) = param {
                env.insert_free_quantifier(ident.to_owned());
            }
        }

        // insert trait bounds
        for param in impl_generics.params.iter() {
            let predicate = param.parse_where_predicate_var(&mut env)?;
            env.insert_predicate(predicate);
        }

        if let Some(where_clause) = &impl_generics.where_clause {
            for predicate in where_clause.predicates.iter() {
                let predicate = predicate.parse_where_predicate_var(&mut env)?;
                env.insert_predicate(predicate);
            }
        }
    }

    // parse self_ty from impl block
    let self_ty_var = match (self_ty, inputs.first()) {
        (Some(self_ty), Some(FnArg::Receiver(receiver))) => {
            if receiver.reference.is_some() {
                return Err(Error::new(
                    receiver.span(),
                    r#"referenced receiver "&self" is not supported, use "self" instead"#,
                ));
            }
            Some(self_ty.parse_type_var(&mut env)?)
        }
        (Some(_), _) => {
            return Err(Error::new(
                inputs.span(),
                "functions inside impl block must have a self receiver",
            ))
        }
        (None, Some(FnArg::Receiver(receiver))) => {
            return Err(Error::new(
                receiver.span(),
                "functions with self receiver must be inside an impl block",
            ))
        }
        (None, _) => {
            // syn::parse2::<Type>(quote! { () })
            //     .unwrap()
            //     .parse_pure_type(&mut vec![])
            //     .unwrap();
            None
        }
    };

    // insert free quantifiers and predicates from fn generics
    {
        // insert free quantifiers
        for param in fn_generics.params.iter() {
            if let GenericParam::Type(TypeParam { ident, .. }) = param {
                env.insert_free_quantifier(ident.to_owned());
            }
        }

        // insert trait bounds
        for param in fn_generics.params.iter() {
            let predicate = param.parse_where_predicate_var(&mut env)?;
            env.insert_predicate(predicate);
        }

        if let Some(where_clause) = &fn_generics.where_clause {
            for predicate in where_clause.predicates.iter() {
                let predicate = predicate.parse_where_predicate_var(&mut env)?;
                env.insert_predicate(predicate);
            }
        }
    }

    // translate function arguments into types and trait bounds
    let (fn_args, fn_predicates): (Vec<_>, Vec<_>) = inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Typed(pat_type) => Some(pat_type),
            FnArg::Receiver(_) => None,
        })
        .map(|pat_type| -> syn::Result<_> {
            let predicate = pat_type.parse_where_predicate_var(&mut env)?;
            let arg = pat_type.pat.parse_type_var(&mut env)?;
            env.insert_predicate(predicate.clone());
            Ok((arg, predicate))
        })
        .try_collect::<_, Vec<_>, _>()?
        .into_iter()
        .unzip();

    // translate output type to trait bound
    let output_bounds = match output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => {
            let bounds = ty.parse_type_param_bounds_var(&mut env)?;
            Some(bounds)
        }
    };

    // insert "self" variable if self_ty is present
    if let Some(var) = &self_ty_var {
        env.insert_bounded_quantifier(format_ident!("self"), false, var.clone());
    }

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

    // generate generic names
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

    // generate trait item
    let trait_item: ItemTrait = {
        let num_args = fn_args.len();
        let args: Vec<_> = (0..num_args)
            .map(|idx| format_ident!("{}ARG_{}", IDENT_PREFIX, idx))
            .collect();
        let arg_predicates: Vec<_> = args
            .iter()
            .zip(fn_predicates.iter())
            .map(|(arg, predicate)| match predicate {
                WherePredicateVar::Type(PredicateTypeVar { bounds, .. }) => {
                    let bounds: Vec<_> = bounds
                        .iter()
                        .map(|bound| bound.substitute(&env, &subsitution))
                        .collect();
                    quote! { #arg: #(#bounds)+* }
                }
            })
            .collect();
        let output_predicate = output_bounds.as_ref().map(|bounds| {
            WherePredicateVar::Type(PredicateTypeVar {
                bounded_ty: syn::parse2::<Type>(quote! { Self::Output })
                    .unwrap()
                    .parse_pure_type(&mut vec![])
                    .unwrap(),
                bounds: bounds.to_owned(),
            })
            .substitute(&env, &subsitution)
        });

        syn::parse2(quote! {
            #[allow(non_snake_case)]
            pub trait #trait_name < #(#args),* >
            where
                #(#arg_predicates,)*
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
        let self_ty_tokens = match &self_ty_var {
            Some(var) => {
                let subsituted = var.substitute(&env, &subsitution);
                quote! { #subsituted }
            }
            None => quote! { () },
        };

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
        let type_item = if self_ty_var.is_some() {
            let self_arg = format_ident!("{}SELF", IDENT_PREFIX);
            quote! {
                #vis type #type_name<#self_arg, #(#args),*> = < #self_arg as #trait_name <#(#args),*> > :: Output;
            }
        } else {
            quote! {
                #vis type #type_name<#(#args),*> = < () as #trait_name <#(#args),*> > :: Output;
            }
        };

        quote! {
            #vis use #mod_name :: #trait_name;
            #type_item

            #[allow(non_snake_case)]
            mod #mod_name {
                use super::*;

                #(#items)*
            }
        }
    };

    Ok(expanded)
}
