use super::*;

pub fn translate_match_expr(
    match_: &ExprMatch,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprMatch { expr, arms, .. } = match_;

    // parse matched expression
    let pattern_tokens = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, scope, env)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };

    // save quantifiers (after matched expression)
    let mutable_quantifiers: HashSet<_> = scope.mutable_quantifiers().keys().cloned().collect();
    let free_quantifiers = scope.free_quantifiers();
    let generics: Vec<_> = free_quantifiers.keys().collect();

    // generate trait names
    let match_trait_name = env
        .register_trait_name(&format!("{}MatchArm_", IDENT_PREFIX))
        .expect("the trait name cannot proper prefix of existing trait names");

    let side_effect_trait_names: HashMap<_, _> = mutable_quantifiers
        .iter()
        .map(|ident| {
            let trait_name = env
                .register_trait_name(&format!("{}MatchSideEffect_", IDENT_PREFIX))
                .expect("please report bug: accidentally using a proper prefix");
            (ident, trait_name)
        })
        .collect();

    // generate trait items
    let placeholder_ident = format_ident!("{}PLACEHOLDER", IDENT_PREFIX);

    let match_trait_item = quote! {
        pub trait #match_trait_name < #(#generics,)* #placeholder_ident> {
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
    let impl_items = {
        let impl_items: Vec<_> = arms
            .iter()
            .map(|Arm { pat, body, .. }| -> syn::Result<_> {
                let mut branched_scope = scope.clone();

                let pat_tokens = {
                    let ty = scope.type_var_builder().from_pat(pat)?;
                    quote! { #ty }
                };
                let body_tokens = translate_expr(body, &mut branched_scope, env)?;
                let trait_bounds_tokens = branched_scope.generate_trait_bounds_tokens();

                // impl item for matched type
                let match_impl = {
                    let trait_pattern = quote!( #match_trait_name<#(#generics,)* #pat_tokens> );
                    let impl_ = quote! {
                        impl< #(#generics),* > #trait_pattern for ()
                        where
                            #trait_bounds_tokens
                        {
                            type Output = #body_tokens;
                        }
                    };
                    impl_
                };

                // impls for side effects
                let side_effect_impls: Vec<_> = mutable_quantifiers
                    .iter()
                    .map(|ident| {
                        let trait_name = &side_effect_trait_names[ident];
                        let value = &branched_scope
                            .get_quantifier(ident)
                            .expect("please report bug: the variable is missing")
                            .value;
                        let trait_pattern = quote! { #trait_name< #(#generics,)* #pat_tokens > };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_pattern for ()
                            where
                                #trait_bounds_tokens
                            {
                                type Output = #value;
                            }
                        };
                        impl_
                    })
                    .collect();

                let impls: Vec<_> = iter::once(match_impl).chain(side_effect_impls).collect();
                Ok(impls)
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect();

        impl_items
    };

    // add items to env
    env.add_item(match_trait_item);
    env.extend_items(side_effect_trait_items);
    env.extend_items(impl_items);

    // assign affected variables
    for ident in mutable_quantifiers.iter() {
        let trait_name = &side_effect_trait_names[ident];
        let trait_pattern = quote! { #trait_name < #(#generics),* , #pattern_tokens > };
        scope.assign_quantifier(
            ident,
            quote! {
                < () as #trait_pattern >::Output
            },
        )?;
        scope.insert_trait_bounds(quote! { () }, trait_pattern);
    }

    // construct returned value
    let output = {
        let trait_pattern = quote! { #match_trait_name < #(#generics),* , #pattern_tokens > };
        let output = quote! {
            < () as #trait_pattern >::Output
        };
        scope.insert_trait_bounds(quote! { () }, trait_pattern);
        output
    };

    Ok(output)
}
