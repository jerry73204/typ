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
    let pattern = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, env, items)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };

    // save quantifiers (after matched expression)
    let mutable_quantifiers = env.mutable_quantifiers();

    // generate trait names
    let match_trait_name = env
        .register_trait_name(&format!("{}MatchArm_", IDENT_PREFIX))
        .expect("the trait name cannot proper prefix of existing trait names");

    let assign_trait_names: HashMap<_, _> = mutable_quantifiers
        .keys()
        .map(|ident| {
            let trait_name = env
                .register_trait_name(&format!("{}MatchAssign", IDENT_PREFIX))
                .expect("please report bug: accidentally using a proper prefix");
            (ident, trait_name)
        })
        .collect();

    // generate generics
    let parent_substitution: IndexMap<_, _> = env
        .free_quantifiers()
        .iter()
        .cloned()
        .enumerate()
        .map(|(index, var)| (var, format_ident!("{}GENERIC_{}", IDENT_PREFIX, index)))
        .collect();
    let parent_generics: Vec<_> = parent_substitution.values().collect();

    // generate traits
    let (match_trait_item, assign_trait_items) = {
        let cond_generic = format_ident!("{}_CONDITION_GENERIC", IDENT_PREFIX);

        let match_trait_item: ItemTrait = syn::parse2(quote! {
            pub trait #match_trait_name < #(#parent_generics,)* #cond_generic> {
                type Output;
            }
        })?;

        let assign_trait_items: Vec<ItemTrait> = assign_trait_names
            .values()
            .map(|trait_name| {
                syn::parse2(quote! {
                    pub trait #trait_name < #(#parent_generics,)* #cond_generic> {
                        type Output;
                    }
                })
            })
            .try_collect()?;

        (match_trait_item, assign_trait_items)
    };

    // generate impl items
    let impl_items = {
        let impl_items: Vec<_> = arms
            .iter()
            .map(
                |Arm {
                     attrs, pat, body, ..
                 }|
                 -> syn::Result<_> {
                    let mut branched_env = env.branch();

                    // parse attributes
                    let generics_attr = unpack_pat_attr(attrs)?;

                    // insert new free quantifiers and predicates
                    let extra_free_quantifiers = match generics_attr {
                        Some(attr) => {
                            let GenericsAttr { params } = syn::parse2(attr.tokens.to_owned())?;
                            let extra_free_quantifiers: IndexSet<_> = params
                                .iter()
                                .map(|param| {
                                    let SimpleTypeParam { ident, bounds } = param;
                                    let predicate =
                                        param.parse_where_predicate_var(&branched_env)?;
                                    let var = branched_env.insert_free_quantifier(ident.to_owned());
                                    branched_env.insert_predicate(predicate);
                                    Ok(var)
                                })
                                .try_collect()?;
                            extra_free_quantifiers
                        }
                        None => IndexSet::new(),
                    };

                    // generate free quantifier substitutions
                    let substitution: IndexMap<_, _> = env
                        .free_quantifiers()
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(index, var)| {
                            (var, format_ident!("{}GENERIC_{}", IDENT_PREFIX, index))
                        })
                        .collect();
                    let input_generics: Vec<_> = substitution
                        .iter()
                        .filter_map(|(var, generic)| {
                            if extra_free_quantifiers.contains(var) {
                                None
                            } else {
                                Some(generic)
                            }
                        })
                        .collect();
                    let all_generics: Vec<_> = substitution.values().collect();

                    // parse body
                    let target = pat
                        .parse_type_var(&branched_env)?
                        .substitute(&branched_env, &substitution);
                    let body_value = translate_expr(body, &mut branched_env, items)?
                        .substitute(&branched_env, &substitution);
                    let predicates = branched_env
                        .predicates()
                        .into_iter()
                        .map(|predicate| predicate.substitute(&branched_env, &substitution));

                    // impl item for matched type
                    let match_impl: ItemImpl = {
                        let trait_ = quote!( #match_trait_name<#(#input_generics,)* #target> );
                        let impl_ = quote! {
                            impl< #(#all_generics),* > #trait_ for ()
                            where
                                #(#predicates),*
                            {
                                type Output = #body_value;
                            }
                        };
                        syn::parse2(impl_)?
                    };

                    // impls for side effects
                    let assign_impls: Vec<ItemImpl> = mutable_quantifiers
                        .keys()
                        .map(|ident| {
                            let trait_name = &assign_trait_names[ident];
                            let value = &branched_env
                                .get_variable(ident)
                                .expect("please report bug: the variable is missing")
                                .value
                                .as_ref()
                                .unwrap()
                                .substitute(&branched_env, &substitution);
                            let trait_ = quote! { #trait_name< #(#input_generics,)* #target > };
                            let impl_ = quote! {
                                impl< #(#all_generics),* > #trait_ for ()
                                where
                                    #(#predicates),*
                                {
                                    type Output = #value;
                                }
                            };
                            syn::parse2(impl_)
                        })
                        .try_collect()?;

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
    items.push(Item::Trait(match_trait_item));
    items.extend(assign_trait_items.into_iter().map(Item::Trait));
    items.extend(impl_items.into_iter().map(Item::Impl));

    // assign affected variables
    for ident in mutable_quantifiers.keys() {
        let trait_name = &assign_trait_names[ident];
        // let trait_ =
        // let value: TypeVar = syn::parse2(quote! { < () as #trait_ >::Output })?;
        // env.assign_quantifier(ident, value)?;
        // scope.insert_trait_bounds(quote! { () }, trait_pattern);
        todo!();
    }

    // construct returned value
    let output = {
        // let trait_pattern = quote! { #match_trait_name < #(#generics),* , #pattern_tokens > };
        // let output = quote! {
        //     < () as #trait_pattern >::Output
        // };
        // scope.insert_trait_bounds(quote! { () }, trait_pattern);
        // output
        todo!();
    };

    Ok(output)
}

fn unpack_pat_attr(attrs: &[Attribute]) -> syn::Result<Option<&Attribute>> {
    let mut generics_attr = None;
    for attr in attrs.iter() {
        let Attribute { style, path, .. } = attr;

        // sanity check
        match (style, path.get_ident()) {
            (AttrStyle::Outer, Some(ident)) => match ident.to_string().as_str() {
                "generics" => match generics_attr {
                    Some(_) => {
                        return Err(Error::new(
                            path.span(),
                            "the generics attribute is defined more than once",
                        ));
                    }
                    None => generics_attr = Some(attr),
                },
                _ => return Err(Error::new(path.span(), "unsupported attribute")),
            },
            (AttrStyle::Outer, None) => {
                return Err(Error::new(path.span(), "unsupported attribute"));
            }
            (AttrStyle::Inner(_), _) => {
                return Err(Error::new(attr.span(), "inner attribute is not supported"))
            }
        }
    }
    Ok(generics_attr)
}
