use super::*;

struct ArmAttributes<'a> {
    pub generics_attr: Option<&'a Attribute>,
    pub capture_attr: Option<&'a Attribute>,
}

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
                .register_trait_name(&format!("{}MatchAssign_", IDENT_PREFIX))
                .expect("please report bug: accidentally using a proper prefix");
            (ident, trait_name)
        })
        .collect();

    // generate generics
    let parent_free_quantifiers = env.free_quantifiers();
    let parent_substitution: IndexMap<_, _> = parent_free_quantifiers
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
            #[allow(non_snake_case)]
            pub trait #match_trait_name < #(#parent_generics,)* #cond_generic> {
                type Output;
            }
        })?;

        let assign_trait_items: Vec<ItemTrait> = assign_trait_names
            .values()
            .map(|trait_name| {
                syn::parse2(quote! {
                    #[allow(non_snake_case)]
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
                    let ArmAttributes {
                        generics_attr,
                        capture_attr,
                    } = unpack_pat_attr(attrs)?;

                    // TODO: process capture_attr

                    // insert new free quantifiers and predicates
                    let extra_free_quantifiers = match generics_attr {
                        Some(attr) => {
                            let GenericsAttr { params } = syn::parse2(attr.tokens.to_owned())?;
                            let extra_free_quantifiers: IndexSet<_> = params
                                .iter()
                                .map(|param| -> syn::Result<_> {
                                    let SimpleTypeParam { ident, .. } = param;
                                    let var = branched_env.insert_free_quantifier(ident.to_owned());
                                    let predicate =
                                        param.parse_where_predicate_var(&branched_env)?;
                                    branched_env.insert_predicate(predicate);
                                    Ok(var)
                                })
                                .try_collect()?;
                            extra_free_quantifiers
                        }
                        None => IndexSet::new(),
                    };

                    // generate free quantifier substitutions
                    let substitution: IndexMap<_, _> = branched_env
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
                    let predicates: Vec<_> = branched_env
                        .predicates()
                        .into_iter()
                        .map(|predicate| predicate.substitute(&branched_env, &substitution))
                        .collect();

                    // impl item for output type
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

                    // impls for variable assignments
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
        let args: Vec<_> = parent_free_quantifiers
            .iter()
            .map(|var| TypeVar::Var(var.clone()))
            .chain(iter::once(pattern.clone()))
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

    // construct returned value
    let output = {
        let args: Vec<_> = parent_free_quantifiers
            .iter()
            .map(|var| TypeVar::Var(var.clone()))
            .chain(iter::once(pattern))
            .collect();
        let trait_ = PathVar {
            segments: vec![SegmentVar {
                ident: match_trait_name.to_owned(),
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

    Ok(output)
}

fn unpack_pat_attr(attrs: &[Attribute]) -> syn::Result<ArmAttributes<'_>> {
    let mut generics_attr = None;
    let mut capture_attr = None;
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
                "capture" => match capture_attr {
                    Some(_) => {
                        return Err(Error::new(
                            path.span(),
                            "the capture attribute is defined more than once",
                        ));
                    }
                    None => capture_attr = Some(attr),
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
    Ok(ArmAttributes {
        generics_attr,
        capture_attr,
    })
}
