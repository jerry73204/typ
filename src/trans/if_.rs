use super::*;

pub fn translate_if_expr(
    if_: &ExprIf,
    env: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    let ExprIf {
        cond,
        then_branch,
        else_branch,
        ..
    } = if_;

    // generate predicate tokens
    let condition = {
        let cond_ty = translate_expr(&*cond, env, items)?;
        let eq_trait: PathVar =
            syn::parse2(quote! { typenum::type_operators::IsEqual<typenum::B1> })?;
        let path = {
            let mut path = eq_trait.clone();
            path.segments.push(SegmentVar {
                ident: format_ident!("Output"),
                arguments: PathArgumentsVar::None,
            });
            path
        };
        let output = TypeVar::Path(TypePathVar {
            qself: Some(QSelfVar {
                ty: Box::new(cond_ty.clone()),
                position: eq_trait.segments.len(),
            }),
            path,
        });
        let apply_predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty: cond_ty,
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: eq_trait,
            })],
        });
        let output_predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty: output.clone(),
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: syn::parse2(quote! { typenum::marker_traits::Bit }).unwrap(),
            })],
        });

        env.insert_predicate(apply_predicate);
        env.insert_predicate(output_predicate);

        output
    };

    // save quantifiers
    let mutable_quantifiers = env.mutable_quantifiers();
    let free_quantifiers = env.free_quantifiers();

    // generate trait names
    let if_trait_name = env
        .register_trait_name(&format!("{}If", IDENT_PREFIX))
        .expect("the trait name cannot proper prefix of existing trait names");

    let assign_trait_names: HashMap<_, _> = mutable_quantifiers
        .keys()
        .map(|ident| {
            let trait_name = env
                .register_trait_name(&format!("{}IfAssign", IDENT_PREFIX))
                .expect("please report bug: accidentally using a proper prefix");
            (ident, trait_name)
        })
        .collect();

    // generate traits
    let substitution: IndexMap<_, _> = free_quantifiers
        .iter()
        .cloned()
        .enumerate()
        .map(|(index, var)| (var, format_ident!("{}GENERIC_{}", IDENT_PREFIX, index)))
        .collect();
    let generics: Vec<_> = substitution.values().collect();
    let cond_generic = format_ident!("{}CONDITION_GENERIC", IDENT_PREFIX);

    let if_trait_item: ItemTrait = {
        syn::parse2(quote! {
            pub trait #if_trait_name < #(#generics,)* #cond_generic > {
                type Output;
            }
        })?
    };

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
    let impls = {
        let impls: Vec<ItemImpl> = match else_branch {
            Some((_else, else_expr)) => {
                let if_impls: Vec<_> = {
                    let mut branched_env = env.branch();
                    let then_output = translate_block(then_branch, &mut branched_env, items)?
                        .substitute(&branched_env, &substitution);
                    let predicates: Vec<_> = branched_env
                        .predicates()
                        .into_iter()
                        .map(|predicate| predicate.substitute(&branched_env, &substitution))
                        .collect();

                    let body_impl: ItemImpl = {
                        let trait_ = quote! { #if_trait_name<#(#generics,)* typenum::B1> };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_ for ()
                            where
                                #(#predicates),*
                            {
                                type Output = #then_output;
                            }
                        };
                        syn::parse2(impl_)?
                    };

                    let assign_impls: Vec<ItemImpl> = mutable_quantifiers
                        .keys()
                        .map(|ident| {
                            let trait_name = &assign_trait_names[ident];
                            let output = branched_env
                                .get_variable(ident)
                                .expect("please report bug: the variable is missing")
                                .value
                                .as_ref()
                                .unwrap()
                                .substitute(&branched_env, &substitution);
                            let trait_ = quote! { #trait_name<#(#generics,)* typenum::B1> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_ for ()
                                where
                                    #(#predicates),*
                                {
                                    type Output = #output;
                                }
                            };
                            syn::parse2(impl_)
                        })
                        .try_collect()?;

                    iter::once(body_impl).chain(assign_impls).collect()
                };

                let else_impls: Vec<_> = {
                    let mut branched_env = env.branch();
                    let else_output = translate_expr(&**else_expr, &mut branched_env, items)?
                        .substitute(&branched_env, &substitution);
                    let predicates: Vec<_> = branched_env
                        .predicates()
                        .into_iter()
                        .map(|predicate| predicate.substitute(&branched_env, &substitution))
                        .collect();

                    let body_impl: ItemImpl = {
                        let trait_ = quote! { #if_trait_name<#(#generics,)* typenum::B0> };
                        let body_impl = quote! {
                            impl< #(#generics),* > #trait_ for ()
                            where
                                #(#predicates),*
                            {
                                type Output = #else_output;
                            }
                        };
                        syn::parse2(body_impl)?
                    };

                    let assign_impls: Vec<ItemImpl> = mutable_quantifiers
                        .keys()
                        .map(|ident| {
                            let trait_name = &assign_trait_names[ident];
                            let value = branched_env
                                .get_variable(ident)
                                .expect("please report bug: the variable is missing")
                                .value
                                .as_ref()
                                .unwrap()
                                .substitute(&env, &substitution);
                            let trait_ = quote! { #trait_name<#(#generics,)* typenum::B0> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_ for ()
                                where
                                    #(#predicates),*
                                {
                                    type Output = #value;
                                }
                            };
                            syn::parse2(impl_)
                        })
                        .try_collect()?;

                    iter::once(body_impl).chain(assign_impls).collect()
                };

                if_impls.into_iter().chain(else_impls).collect()
            }
            None => {
                let if_impls: Vec<_> = {
                    let mut branched_env = env.branch();
                    translate_block(then_branch, &mut branched_env, items)?;
                    let predicates: Vec<_> = branched_env
                        .predicates()
                        .into_iter()
                        .map(|predicate| predicate.substitute(&branched_env, &substitution))
                        .collect();

                    let body_impl: ItemImpl = {
                        let trait_ = quote! { #if_trait_name<#(#generics,)* typenum::B1> };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_ for ()
                            where
                                #(#predicates),*
                            {
                                type Output = ();
                            }
                        };
                        syn::parse2(impl_)?
                    };

                    // insert trait bound
                    let assign_impls: Vec<ItemImpl> = mutable_quantifiers
                        .keys()
                        .map(|ident| {
                            let trait_name = &assign_trait_names[ident];
                            let value = branched_env
                                .get_variable(ident)
                                .expect("please report bug: the variable is missing")
                                .value
                                .as_ref()
                                .unwrap()
                                .substitute(&branched_env, &substitution);
                            let trait_ = quote! { #trait_name<#(#generics,)* typenum::B1> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_ for ()
                                where
                                    #(#predicates),*
                                {
                                    type Output = #value;
                                }
                            };
                            syn::parse2(impl_)
                        })
                        .try_collect()?;

                    iter::once(body_impl).chain(assign_impls).collect()
                };

                let else_impls: Vec<_> = {
                    let predicates: Vec<_> = env
                        .predicates()
                        .into_iter()
                        .map(|predicate| predicate.substitute(env, &substitution))
                        .collect();

                    let body_impl: ItemImpl = {
                        let trait_ = quote! { #if_trait_name<#(#generics,)* typenum::B0> };
                        let impl_ = quote! {
                            impl< #(#generics),* > #trait_ for ()
                            where
                                #(#predicates),*
                            {
                                type Output = ();
                            }
                        };
                        syn::parse2(impl_)?
                    };

                    let assign_impls: Vec<ItemImpl> = mutable_quantifiers
                        .keys()
                        .map(|ident| {
                            let trait_name = &assign_trait_names[ident];
                            let value = &env
                                .get_variable(ident)
                                .expect("please report bug: the variable is missing")
                                .value
                                .as_ref()
                                .unwrap()
                                .substitute(&env, &substitution);
                            let trait_ = quote! { #trait_name<#(#generics,)* typenum::B0> };
                            let impl_ = quote! {
                                impl< #(#generics),* > #trait_ for ()
                                where
                                    #(#predicates),*
                                {
                                    type Output = #value;
                                }
                            };
                            syn::parse2(impl_)
                        })
                        .try_collect()?;

                    iter::once(body_impl).chain(assign_impls).collect()
                };

                if_impls.into_iter().chain(else_impls).collect()
            }
        };

        impls
    };

    // assign affected variables
    for ident in mutable_quantifiers.keys() {
        let trait_name = &assign_trait_names[ident];
        let args: Vec<_> = free_quantifiers
            .iter()
            .map(|var| TypeVar::Var(var.clone()))
            .chain(iter::once(condition.clone()))
            .collect();
        let trait_ = PathVar {
            segments: vec![SegmentVar {
                ident: trait_name.to_owned(),
                arguments: PathArgumentsVar::AngleBracketed(args),
            }],
        };
        let path = {
            let mut path = trait_.clone();
            path.segments.push(SegmentVar {
                ident: format_ident!("Output"),
                arguments: PathArgumentsVar::None,
            });
            path
        };
        let bounded_ty: TypeVar = syn::parse2(quote! { () }).unwrap();
        let value = TypeVar::Path(TypePathVar {
            qself: Some(QSelfVar {
                ty: Box::new(bounded_ty.clone()),
                position: trait_.segments.len(),
            }),
            path,
        });
        let predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty,
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: trait_,
            })],
        });

        env.assign_quantifier(ident, value)?;
        env.insert_predicate(predicate);
    }

    // construct output
    let output = {
        let args: Vec<_> = free_quantifiers
            .iter()
            .map(|var| TypeVar::Var(var.clone()))
            .chain(iter::once(condition))
            .collect();
        let trait_ = PathVar {
            segments: vec![SegmentVar {
                ident: if_trait_name.to_owned(),
                arguments: PathArgumentsVar::AngleBracketed(args),
            }],
        };
        let path = {
            let mut path = trait_.clone();
            path.segments.push(SegmentVar {
                ident: format_ident!("Output"),
                arguments: PathArgumentsVar::None,
            });
            path
        };
        let bounded_ty: TypeVar = syn::parse2(quote! { () }).unwrap();
        let output = TypeVar::Path(TypePathVar {
            qself: Some(QSelfVar {
                ty: Box::new(bounded_ty.clone()),
                position: trait_.segments.len(),
            }),
            path,
        });
        let predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty,
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: trait_,
            })],
        });

        env.insert_predicate(predicate);
        output
    };

    // add items to env
    items.push(Item::Trait(if_trait_item));
    items.extend(assign_trait_items.into_iter().map(Item::Trait));
    items.extend(impls.into_iter().map(Item::Impl));

    Ok(output)
}
