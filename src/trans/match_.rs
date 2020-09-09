use super::*;

pub fn translate_match_expr(
    match_: &ExprMatch,
    env: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    let ExprMatch { expr, arms, .. } = match_;

    // parse matched expression
    let pattern_tokens = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, env, items)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };

    // save quantifiers (after matched expression)
    let mutable_quantifiers = env.mutable_quantifiers();
    let free_quantifiers = env.free_quantifiers();

    // generate trait names
    let match_trait_name = env
        .register_trait_name(&format!("{}MatchArm_", IDENT_PREFIX))
        .expect("the trait name cannot proper prefix of existing trait names");

    let assign_trait_names: HashMap<_, _> = mutable_quantifiers
        .iter()
        .map(|ident| {
            let trait_name = env
                .register_trait_name(&format!("{}MatchAssign", IDENT_PREFIX))
                .expect("please report bug: accidentally using a proper prefix");
            (ident, trait_name)
        })
        .collect();

    // generate trait items
    let cond_generic = format_ident!("{}_CONDITION_GENERIC", IDENT_PREFIX);

    // generate traits
    let free_quantifier_substitution: IndexMap<_, _> = free_quantifiers
        .iter()
        .cloned()
        .enumerate()
        .map(|(index, var)| (var, format_ident!("{}GENERIC_{}", IDENT_PREFIX, index)))
        .collect();
    let generics: Vec<_> = free_quantifier_substitution.values().collect();

    let match_trait_item: ItemTrait = syn::parse2(quote! {
        pub trait #match_trait_name < #(#generics,)* #cond_generic> {
            type Output;
        }
    })?;

    let assign_trait_items: Vec<ItemTrait> = assign_trait_names
        .values()
        .map(|trait_name| {
            syn::parse2(quote! {
                pub trait #trait_name < #(#generics,)* #cond_generic> {
                    type Output;
                }
            })
        })
        .try_collect()?;

    // generate impl items
    let impl_items = {
        let impl_items: Vec<_> = arms
            .iter()
            .map(
                |Arm {
                     attrs, pat, body, ..
                 }|
                 -> syn::Result<_> {
                    let mut branched_env = env.clone();

                    let pat_tokens = {
                        let ty = scope.type_var_builder().from_pat(pat)?;
                        quote! { #ty }
                    };
                    let body_tokens = translate_expr(body, &mut branched_env, items)?;
                    let predicates = branched_env.predicates();

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
                    let assign_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|ident| {
                            let trait_name = &assign_trait_names[ident];
                            let value = &branched_scope
                                .get_quantifier(ident)
                                .expect("please report bug: the variable is missing")
                                .value;
                            let trait_pattern =
                                quote! { #trait_name< #(#generics,)* #pat_tokens > };
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

                    let impls: Vec<_> = iter::once(match_impl).chain(assign_impls).collect();
                    Ok(impls)
                },
            )
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect();

        impl_items
    };

    // add items to env
    // env.add_item(match_trait_item);
    // env.extend_items(assign_trait_items);
    // env.extend_items(impl_items);

    // assign affected variables
    // for ident in mutable_quantifiers.iter() {
    //     let trait_name = &assign_trait_names[ident];
    //     let trait_pattern = quote! { #trait_name < #(#generics),* , #pattern_tokens > };
    //     scope.assign_quantifier(
    //         ident,
    //         quote! {
    //             < () as #trait_pattern >::Output
    //         },
    //     )?;
    //     scope.insert_trait_bounds(quote! { () }, trait_pattern);
    // }

    // construct returned value
    // let output = {
    //     let trait_pattern = quote! { #match_trait_name < #(#generics),* , #pattern_tokens > };
    //     let output = quote! {
    //         < () as #trait_pattern >::Output
    //     };
    //     scope.insert_trait_bounds(quote! { () }, trait_pattern);
    //     output
    // };

    // Ok(output)
    todo!();
}
