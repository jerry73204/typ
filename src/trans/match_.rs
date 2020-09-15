use super::*;

struct ArmAttributes {
    pub generics_attr: Option<GenericsAttr>,
    pub capture_attr: Option<CaptureAttr>,
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
    let pattern = translate_expr(&**expr, env, items)?;

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

                    // insert new free quantifiers and predicates
                    let extra_free_quantifiers = match &generics_attr {
                        Some(GenericsAttr { params }) => {
                            let free_quantifiers: IndexSet<_> = params
                                .iter()
                                .map(|param| -> syn::Result<_> {
                                    let SimpleTypeParam { ident, .. } = param;
                                    let var = branched_env.insert_free_quantifier(ident.to_owned());
                                    let predicate =
                                        param.parse_where_predicate_var(&mut branched_env)?;
                                    branched_env.insert_predicate(predicate);
                                    Ok(var)
                                })
                                .try_collect()?;
                            free_quantifiers
                        }
                        None => IndexSet::new(),
                    };

                    // insert new predicates for captured variables
                    match &capture_attr {
                        Some(CaptureAttr { params }) => {
                            for param in params.iter() {
                                let predicate =
                                    param.parse_where_predicate_var(&mut branched_env)?;
                                branched_env.insert_predicate(predicate);
                            }
                        }
                        None => (),
                    }

                    // generate substitutions for free variables
                    let substitution: IndexMap<_, _> = branched_env
                        .free_quantifiers()
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(index, var)| {
                            (var, format_ident!("{}GENERIC_{}", IDENT_PREFIX, index))
                        })
                        .collect();

                    // generate generic identifiers
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
                    let target = {
                        // list in-place free and captured variables
                        let mut variables = HashMap::new();

                        if let Some(CaptureAttr { params }) = capture_attr {
                            let vars: Vec<_> = params
                                .iter()
                                .map(|SimpleTypeParam { ident, .. }| {
                                    branched_env
                                        .get_variable(ident)
                                        .map(|var| (ident.to_owned(), var))
                                        .ok_or_else(|| {
                                            Error::new(ident.span(), "the variable is not defined")
                                        })
                                })
                                .try_collect()?;
                            variables.extend(vars);
                        };

                        if let Some(GenericsAttr { params }) = generics_attr {
                            let iter = params.iter().map(|SimpleTypeParam { ident, .. }| {
                                branched_env
                                    .get_variable(ident)
                                    .map(|var| (ident.to_owned(), var))
                                    .unwrap()
                            });
                            variables.extend(iter);
                        };

                        // parse target
                        parse_pattern::parse_type_pattern_from_pat(pat, &variables)?
                            .substitute(&branched_env, &substitution)
                    };
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
                            let value = branched_env
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
        let bounded_ty: TypeVar = syn::parse2::<Type>(quote! { () })
            .unwrap()
            .parse_pure_type(&mut vec![])
            .unwrap();
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
        let bounded_ty: TypeVar = syn::parse2::<Type>(quote! { () })
            .unwrap()
            .parse_pure_type(&mut vec![])
            .unwrap();
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

fn unpack_pat_attr(attrs: &[Attribute]) -> syn::Result<ArmAttributes> {
    let mut generics_attr = None;
    let mut capture_attr = None;

    // check attributes one by one
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

    // parse
    let generics_attr: Option<GenericsAttr> = generics_attr
        .map(|attr| syn::parse2(attr.tokens.to_owned()))
        .transpose()?;
    let capture_attr: Option<CaptureAttr> = capture_attr
        .map(|attr| syn::parse2(attr.tokens.to_owned()))
        .transpose()?;

    // sanity check
    match (&generics_attr, &capture_attr) {
        (Some(generics_attr), Some(capture_attr)) => {
            let generic_idents: HashSet<_> = generics_attr
                .params
                .iter()
                .map(|param| &param.ident)
                .collect();
            for SimpleTypeParam {
                ident: capture_ident,
                ..
            } in capture_attr.params.iter()
            {
                if let Some(generic_ident) = generic_idents.get(capture_ident) {
                    let mut err = Error::new(
                        capture_ident.span(),
                        "cannot capture a variable already in generics list",
                    );
                    err.combine(Error::new(
                        generic_ident.span(),
                        "the name is declared as a generic here",
                    ));
                    return Err(err);
                }
            }
        }
        _ => (),
    }

    Ok(ArmAttributes {
        generics_attr,
        capture_attr,
    })
}

mod parse_pattern {
    use super::*;

    pub fn parse_type_pattern_from_pat(
        pat: &Pat,
        captured: &HashMap<Ident, Shared<Variable>>,
    ) -> syn::Result<TypeVar> {
        match pat {
            Pat::Ident(pat_ident) => {
                // sanity check
                let PatIdent {
                    ident,
                    by_ref,
                    mutability,
                    subpat,
                    ..
                } = pat_ident;

                if let Some(by_ref) = by_ref {
                    return Err(Error::new(by_ref.span(), "ref keyword is not supported"));
                }

                if let Some(mutability) = mutability {
                    return Err(Error::new(
                        mutability.span(),
                        "mut keyword is not supported",
                    ));
                }

                if let Some(_) = subpat {
                    return Err(Error::new(pat_ident.span(), "subpattern is not supported"));
                }

                parse_type_pattern_from_ident(ident, captured)
            }
            Pat::Path(PatPath { qself, path, .. }) => {
                let qself = match qself {
                    Some(QSelf { ty, position, .. }) => {
                        let ty = parse_type_pattern(ty, captured)?;
                        Some(QSelfVar {
                            ty: Box::new(ty),
                            position: *position,
                        })
                    }
                    None => None,
                };
                let path = parse_path_pattern(path, captured)?;
                Ok(TypeVar::Path(TypePathVar { qself, path }))
            }
            Pat::Tuple(PatTuple { elems, .. }) => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|elem| parse_type_pattern_from_pat(elem, captured))
                    .try_collect()?;
                Ok(TypeVar::Tuple(TypeTupleVar { elems }))
            }
            _ => Err(Error::new(pat.span(), "not a type")),
        }
    }

    pub fn parse_type_pattern(
        type_: &Type,
        captured: &HashMap<Ident, Shared<Variable>>,
    ) -> syn::Result<TypeVar> {
        let ty = match type_ {
            Type::Path(TypePath { qself, path }) => match (qself, path.get_ident()) {
                (Some(QSelf { ty, position, .. }), _) => {
                    let ty = parse_type_pattern(ty, captured)?;
                    let path = parse_path_pattern(path, captured)?;
                    TypeVar::Path(TypePathVar {
                        qself: Some(QSelfVar {
                            ty: Box::new(ty),
                            position: *position,
                        }),
                        path,
                    })
                }
                (None, Some(ident)) => match captured.get(ident) {
                    Some(var) => TypeVar::Var(var.to_owned()),
                    None => {
                        let path = parse_path_pattern(path, captured)?;
                        TypeVar::Path(TypePathVar { qself: None, path })
                    }
                },
                (None, None) => {
                    let path = parse_path_pattern(path, captured)?;
                    TypeVar::Path(TypePathVar { qself: None, path })
                }
            },
            Type::Tuple(TypeTuple { elems, .. }) => {
                let elems: Vec<_> = elems
                    .iter()
                    .map(|elem| parse_type_pattern(elem, captured))
                    .try_collect()?;
                TypeVar::Tuple(TypeTupleVar { elems })
            }
            Type::Paren(TypeParen { elem, .. }) => parse_type_pattern(elem, captured)?,
            _ => return Err(Error::new(type_.span(), "unsupported type variant")),
        };
        Ok(ty)
    }

    pub fn parse_type_pattern_from_ident(
        ident: &Ident,
        captured: &HashMap<Ident, Shared<Variable>>,
    ) -> syn::Result<TypeVar> {
        match captured.get(ident) {
            Some(var) => Ok(TypeVar::Var(var.to_owned())),
            None => Ok(TypeVar::Path(TypePathVar {
                qself: None,
                path: PathVar {
                    segments: vec![SegmentVar {
                        ident: ident.to_owned(),
                        arguments: PathArgumentsVar::None,
                    }],
                },
            })),
        }
    }

    pub fn parse_path_pattern(
        path: &Path,
        captured: &HashMap<Ident, Shared<Variable>>,
    ) -> syn::Result<PathVar> {
        let Path { segments, .. } = path;
        let segments: Vec<_> = segments
            .iter()
            .map(|segment| parse_segment_pattern(segment, captured))
            .try_collect()?;
        Ok(PathVar { segments })
    }

    pub fn parse_segment_pattern(
        segment: &PathSegment,
        captured: &HashMap<Ident, Shared<Variable>>,
    ) -> syn::Result<SegmentVar> {
        let PathSegment { ident, arguments } = segment;
        let arguments = match arguments {
            PathArguments::None => PathArgumentsVar::None,
            PathArguments::AngleBracketed(args) => {
                let args = args
                    .args
                    .iter()
                    .map(|arg| match arg {
                        GenericArgument::Type(ty) => parse_type_pattern(ty, captured),
                        _ => Err(Error::new(arg.span(), "unsupported generic variant")),
                    })
                    .try_collect()?;
                PathArgumentsVar::AngleBracketed(args)
            }
            PathArguments::Parenthesized(args) => {
                let inputs = args
                    .inputs
                    .iter()
                    .map(|ty| parse_type_pattern(ty, captured))
                    .try_collect()?;
                PathArgumentsVar::Parenthesized(inputs)
            }
        };

        Ok(SegmentVar {
            ident: ident.to_owned(),
            arguments,
        })
    }
}
