use super::*;

pub fn translate_if_expr(
    if_: &ExprIf,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprIf {
        cond,
        then_branch,
        else_branch,
        ..
    } = if_;

    let free_quantifiers = scope.free_quantifiers();
    let mutable_quantifiers: HashSet<_> = scope.mutable_quantifiers().keys().cloned().collect();
    let generics: Vec<_> = free_quantifiers.keys().collect();

    // generate trait names
    let if_trait_name = env
        .register_trait_name(&format!("{}If_", IDENT_PREFIX))
        .expect("the trait name cannot proper prefix of existing trait names");

    let side_effect_trait_names: HashMap<_, _> = mutable_quantifiers
        .iter()
        .map(|ident| {
            let trait_name = env
                .register_trait_name(&format!("{}IfSideEffect_", IDENT_PREFIX))
                .expect("please report bug: accidentally using a proper prefix");
            (ident, trait_name)
        })
        .collect();

    // generate traits
    let placeholder_ident = format_ident!("{}PLACEHOLDER", IDENT_PREFIX);

    let if_trait_item = quote! {
        pub trait #if_trait_name < #(#generics,)* #placeholder_ident > {
            type Output;
        }
    };

    let side_effect_trait_items: Vec<_> = side_effect_trait_names
        .values()
        .map(|trait_name| {
            quote! {
                pub trait #trait_name < #(#generics,)* #placeholder_ident> {
                    type Output;
                }
            }
        })
        .collect();

    // generate impl items
    let impls = {
        // clone first to avoid unnecessary trait bounds
        let saved_scope = scope.clone();

        let impls: Vec<_> = match else_branch {
            Some((_else, else_expr)) => {
                let if_impls: Vec<_> = {
                    let mut branched_scope = saved_scope.clone();
                    let then_tokens = translate_block(then_branch, &mut branched_scope, env)?;
                    let trait_bounds_tokens = branched_scope.generate_trait_bounds_tokens();

                    let body_impl = {
                        let trait_pattern = quote! { #if_trait_name<#(#generics,)* typenum::B1> };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_pattern for ()
                            where
                                #trait_bounds_tokens
                            {
                                type Output = #then_tokens;
                            }
                        };
                        scope.insert_trait_bounds(quote! { () }, trait_pattern);
                        impl_
                    };

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|ident| {
                            let trait_name = &side_effect_trait_names[ident];
                            let value = branched_scope
                                .get_quantifier(ident)
                                .expect("please report bug: the variable is missing")
                                .value;
                            let trait_pattern = quote! { #trait_name<#(#generics,)* typenum::B1> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_pattern for ()
                                where
                                    #trait_bounds_tokens
                                {
                                    type Output = #value;
                                }
                            };
                            scope.insert_trait_bounds(quote! { () }, trait_pattern.clone());
                            impl_
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                let else_impls: Vec<_> = {
                    let mut branched_scope = saved_scope.clone();
                    let else_tokens = translate_expr(&**else_expr, &mut branched_scope, env)?;
                    let trait_bounds_tokens = branched_scope.generate_trait_bounds_tokens();

                    let body_impl = {
                        let trait_pattern = quote! { #if_trait_name<#(#generics,)* typenum::B0> };
                        let body_impl = quote! {
                            impl< #(#generics),* > #trait_pattern for ()
                            where
                                #trait_bounds_tokens
                            {
                                type Output = #else_tokens;
                            }
                        };
                        scope.insert_trait_bounds(quote! { () }, trait_pattern);
                        body_impl
                    };

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|ident| {
                            let trait_name = &side_effect_trait_names[ident];
                            let value = branched_scope
                                .get_quantifier(ident)
                                .expect("please report bug: the variable is missing")
                                .value;
                            let trait_pattern = quote! { #trait_name<#(#generics,)* typenum::B0> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_pattern for ()
                                where
                                    #trait_bounds_tokens
                                {
                                    type Output = #value;
                                }
                            };
                            scope.insert_trait_bounds(quote! { () }, trait_pattern);
                            impl_
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                if_impls.into_iter().chain(else_impls).collect()
            }
            None => {
                let if_impls: Vec<_> = {
                    let mut branched_scope = saved_scope.clone();
                    let _then_tokens = translate_block(then_branch, &mut branched_scope, env)?;
                    let trait_bounds_tokens = branched_scope.generate_trait_bounds_tokens();

                    let body_impl = {
                        let trait_pattern = quote! { #if_trait_name<#(#generics,)* typenum::B1> };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_pattern for ()
                            where
                                #trait_bounds_tokens
                            {
                                type Output = ();
                            }
                        };
                        scope.insert_trait_bounds(quote! { () }, trait_pattern);
                        impl_
                    };

                    // insert trait bound
                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|ident| {
                            let trait_name = &side_effect_trait_names[ident];
                            let value = branched_scope
                                .get_quantifier(ident)
                                .expect("please report bug: the variable is missing")
                                .value;
                            let trait_pattern = quote! { #trait_name<#(#generics,)* typenum::B1> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_pattern for ()
                                where
                                    #trait_bounds_tokens
                                {
                                    type Output = #value;
                                }
                            };

                            // add trait bound
                            scope.insert_trait_bounds(quote! { () }, trait_pattern);

                            impl_
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                let else_impls: Vec<_> = {
                    let trait_bounds_tokens = saved_scope.generate_trait_bounds_tokens();

                    let body_impl = {
                        let trait_pattern = quote! { #if_trait_name<#(#generics,)* typenum::B0> };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_pattern for ()
                            where
                                #trait_bounds_tokens
                            {
                                type Output = ();
                            }
                        };
                        scope.insert_trait_bounds(quote! { () }, trait_pattern);
                        impl_
                    };

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|ident| {
                            let trait_name = &side_effect_trait_names[ident];
                            let value = saved_scope
                                .get_quantifier(ident)
                                .expect("please report bug: the variable is missing")
                                .value;
                            let trait_pattern = quote! { #trait_name<#(#generics,)* typenum::B0> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_pattern for ()
                                where
                                    #trait_bounds_tokens
                                {
                                    type Output = #value;
                                }
                            };
                            scope.insert_trait_bounds(quote! { () }, trait_pattern);
                            impl_
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                if_impls.into_iter().chain(else_impls).collect()
            }
        };

        impls
    };

    // generate predicate tokens
    let cond_predicate = {
        let cond_ty = translate_expr(&*cond, scope, env)?;
        let eq_trait = quote! { typenum::type_operators::IsEqual<typenum::B1> };
        let cond_predicate = quote! { < #cond_ty as #eq_trait >::Output };

        scope.insert_trait_bounds(cond_ty, eq_trait);
        scope.insert_trait_bounds(
            cond_predicate.clone(),
            quote! { typenum::marker_traits::Bit },
        );

        cond_predicate
    };

    // assign affected variables
    for ident in mutable_quantifiers.iter() {
        let trait_name = &side_effect_trait_names[ident];
        scope.assign_quantifier(
            ident,
            quote! {
                < () as #trait_name < #(#generics,)* #cond_predicate > >::Output
            },
        )?;
    }

    // construct return type
    let output = {
        let trait_pattern = quote! { #if_trait_name < #(#generics,)* #cond_predicate > };
        let output = quote! { < () as #trait_pattern >::Output };
        scope.insert_trait_bounds(quote! { () }, trait_pattern);
        output
    };

    // add trait bounds for side effect traits
    side_effect_trait_names.values().for_each(|trait_name| {
        let trait_pattern = quote! { #trait_name < #(#generics,)* #cond_predicate > };
        scope.insert_trait_bounds(quote! { () }, trait_pattern);
    });

    // add items to env
    env.add_item(if_trait_item);
    env.extend_items(side_effect_trait_items);
    env.extend_items(impls);

    Ok(output)
}
