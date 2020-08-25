use crate::{
    common::*,
    env::Env,
    scope::{Scope, Variable},
};

pub fn translate_items(items: &[Item]) -> syn::Result<TokenStream> {
    let tokens_vec: Vec<_> = items
        .into_iter()
        .map(|item| {
            let tokens = match item {
                Item::Enum(enum_) => translate_enum(&enum_)?,
                Item::Fn(fn_) => {
                    let ItemFn {
                        sig, block, vis, ..
                    } = fn_;
                    translate_fn(vis, sig, block, None)?
                }
                Item::Struct(struct_) => translate_struct(&struct_)?,
                Item::Impl(impl_) => translate_impl(&impl_)?,
                _ => {
                    return Err(Error::new(item.span(), "unsupported item kind"));
                }
            };
            Ok(tokens)
        })
        .try_collect()?;

    let expanded = quote! {
        #(#tokens_vec)*
    };

    Ok(expanded)
}

fn translate_struct(_struct_: &ItemStruct) -> syn::Result<TokenStream> {
    todo!();
}

fn translate_enum(_enum_: &ItemEnum) -> syn::Result<TokenStream> {
    todo!();
    // let ItemEnum {
    //     vis,
    //     ident: trait_name,
    //     variants,
    //     ..
    // } = enum_;

    // let types: Vec<_> = variants
    //     .iter()
    //     .map(|variant| -> syn::Result<_> {
    //         let Variant {
    //             ident: type_name,
    //             fields,
    //             ..
    //         } = variant;

    //         let generics = match fields {
    //             Fields::Unit => vec![],
    //             Fields::Unnamed(unnamed) => {
    //                 let generics: Vec<_> = unnamed
    //                     .unnamed
    //                     .iter()
    //                     .enumerate()
    //                     .map(|(index, field)| -> syn::Result<_> {
    //                         let Field { ty, .. } = field;
    //                         let generic_name = format_ident!("T{}", index);
    //                         let trait_bounds = ty_to_trait_bounds(ty)?;

    //                         Ok((generic_name, trait_bounds))
    //                     })
    //                     .try_collect()?;

    //                 generics
    //             }
    //             Fields::Named(named) => {
    //                 let generics: Vec<_> = named
    //                     .named
    //                     .iter()
    //                     .map(|field| -> syn::Result<_> {
    //                         let Field { ident, ty, .. } = field;
    //                         let generic_name = ident.to_owned().unwrap();
    //                         let trait_bounds = ty_to_trait_bounds(ty)?;

    //                         Ok((generic_name.to_owned(), trait_bounds))
    //                     })
    //                     .try_collect()?;

    //                 generics
    //             }
    //         };

    //         Ok((type_name, generics))
    //     })
    //     .try_collect()?;

    // let impls = {
    //     let items: Vec<_> = types
    //         .into_iter()
    //         .map(|(type_name, generics)| {
    //             let generic_args = {
    //                 let names: Vec<_> = generics.iter().map(|(name, _)| name).collect();
    //                 quote! {
    //                     #(#names),*
    //                 }
    //             };

    //             let where_clause = {
    //                 let bounds: Vec<_> = generics
    //                     .iter()
    //                     .filter_map(|(name, bounds)| {
    //                         if bounds.is_empty() {
    //                             None
    //                         } else {
    //                             Some(quote! {
    //                                 #name: #(#bounds)+*
    //                             })
    //                         }
    //                     })
    //                     .collect();

    //                 quote! {
    //                     #(#bounds),*
    //                 }
    //             };

    //             quote! {
    //                 #vis struct #type_name<#generic_args>
    //                 where
    //                     #where_clause
    //                 {
    //                     _phantom: ::core::marker::PhantomData<#generic_args>
    //                 }

    //                 impl<#generic_args> #trait_name for #type_name<#generic_args>
    //                 where
    //                     #where_clause
    //                 {}
    //             }
    //         })
    //         .collect();

    //     quote! {
    //         #(#items)*
    //     }
    // };

    // let expanded = quote! {
    //     #vis trait #trait_name {}

    //     #impls
    // };

    // Ok(expanded)
}

fn translate_impl(impl_: &ItemImpl) -> syn::Result<TokenStream> {
    let ItemImpl {
        trait_,
        self_ty,
        items,
        ..
    } = impl_;

    if trait_.is_some() {
        return Err(Error::new(
            impl_.span(),
            r#""for Trait" clause is not supported"#,
        ));
    }

    let trait_path = match &**self_ty {
        Type::Path(type_path) => &type_path.path,
        _ => return Err(Error::new(self_ty.span(), "unsupported type kind")),
    };

    let items_tokens: Vec<_> = items
        .iter()
        .map(|item| -> syn::Result<_> {
            let tokens = match item {
                ImplItem::Method(method) => {
                    let ImplItemMethod {
                        sig, block, vis, ..
                    } = method;
                    translate_fn(vis, sig, block, Some(trait_path))?
                }
                _ => {
                    return Err(Error::new(item.span(), "unsupported item"));
                }
            };

            Ok(tokens)
        })
        .try_collect()?;

    let expanded = quote! { #(#items_tokens)* };
    Ok(expanded)
}

fn translate_fn(
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
    let mut scope = Scope::new();

    // check if "self" is present in the input arguments and is consistent with impl block
    let self_ty_tokens = match (self_ty, inputs.first()) {
        (
            Some(self_ty),
            Some(FnArg::Receiver(Receiver {
                reference: None, ..
            })),
        ) => quote! { self_ty },
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
    let block_tokens = translate_block(&block, &mut scope, &mut env)?;

    // insert trait bound for output type
    if let Some(bounds) = &output_bounds {
        scope.insert_trait_bounds(quote! { #block_tokens }, quote! { #bounds });
    }

    // generate trait bounds tokens
    let trait_bounds_tokens: Vec<_> = scope
        .trait_bounds()
        .iter()
        .map(|(predicate, bounds)| {
            quote! { #predicate: #bounds }
        })
        .collect();

    // generate trait
    let trait_tokens = {
        let place_holders: Vec<_> = fn_args
            .iter()
            .enumerate()
            .map(|(idx, _)| format_ident!("_{}", idx))
            .collect();
        let output_bounds_tokens = output_bounds
            .as_ref()
            .map(|bounds| quote! { Self::Output : #bounds });

        quote! {
            #vis trait #fn_name < #(#place_holders),* >
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
                #(#trait_bounds_tokens),*
            {
                type Output = #block_tokens;
            }
        }
    };

    // generate output tokens
    env.add_item(trait_tokens);
    env.add_item(impl_tokens);

    Ok(quote! { #env })
}

fn translate_block(block: &Block, scope: &mut Scope, env: &mut Env) -> syn::Result<TokenStream>
where
{
    let mut output_ty = None;

    // creates a subscope
    scope.sub_scope(|scope| {
        for stmt in block.stmts.iter() {
            match stmt {
                Stmt::Local(local) => {
                    // parse let statement
                    let (ident, trait_bounds_opt, is_mut, expr) = {
                        let (pat, expr) = match local {
                            Local {
                                pat,
                                init: Some((_eq, expr)),
                                ..
                            } => (pat, expr),
                            _ => {
                                return Err(Error::new(local.span(), "initial type must be given"))
                            }
                        };

                        let (ident, trait_bounds_opt, is_mut) = match pat {
                            Pat::Ident(PatIdent {
                                ident, mutability, ..
                            }) => (ident, None, matches!(mutability, Some(_))),
                            Pat::Type(PatType { pat, ty, .. }) => {
                                let (ident, is_mut) = match &**pat {
                                    Pat::Ident(PatIdent {
                                        ident, mutability, ..
                                    }) => (ident, matches!(mutability, Some(_))),
                                    _ => return Err(Error::new(local.span(), "not an identifier")),
                                };
                                let trait_bounds =
                                    scope.trait_bounds_var_builder().from_type(ty)?;

                                (ident, Some(trait_bounds), is_mut)
                            }
                            _ => return Err(Error::new(pat.span(), "not a identifier")),
                        };

                        (ident, trait_bounds_opt, is_mut, expr)
                    };

                    // compute output type from expression
                    let value = translate_expr(expr, scope, env)?;

                    // register the local quantifier
                    scope.insert_bounded_quantifier(ident.to_owned(), value, is_mut);

                    // insert trait bounds
                    if let Some(trait_bounds) = trait_bounds_opt {
                        let predicate = scope
                            .type_var_builder()
                            .from_quantifier(ident)
                            .expect("please report bug: not a valid quantifier");
                        scope.insert_trait_bounds(quote! { #predicate }, quote! { #trait_bounds });
                    }
                }
                Stmt::Item(item) => {
                    return Err(Error::new(item.span(), "in-block item is not allowed"))
                }
                Stmt::Expr(expr) => {
                    output_ty = Some(translate_expr(expr, scope, env)?);
                }
                Stmt::Semi(expr, _semi) => {
                    translate_expr(expr, scope, env)?;
                    output_ty = Some(quote! { () });
                }
            }
        }

        Ok(())
    })?;

    Ok(output_ty.unwrap_or_else(|| quote! { () }))
}

fn translate_expr(expr: &Expr, scope: &mut Scope, env: &mut Env) -> syn::Result<TokenStream>
where
{
    match expr {
        Expr::Match(match_) => translate_match_expr(match_, scope, env),
        Expr::Path(path) => translate_path_expr(path, scope, env),
        Expr::Tuple(tuple) => translate_tuple_expr(tuple, scope, env),
        Expr::Binary(binop) => translate_binary_expr(binop, scope, env),
        Expr::If(if_) => translate_if_expr(if_, scope, env),
        Expr::Block(block) => translate_block_expr(block, scope, env),
        Expr::Call(call) => translate_call_expr(call, scope, env),
        Expr::Paren(paren) => translate_expr(&paren.expr, scope, env),
        Expr::Assign(assign) => translate_assign_expr(&assign, scope, env),
        _ => Err(Error::new(expr.span(), "unsupported expression")),
    }
}

fn translate_assign_expr(
    assign: &ExprAssign,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprAssign { left, right, .. } = assign;
    let quantifier_ident = match &**left {
        Expr::Path(path) => match path.path.get_ident() {
            Some(ident) => ident,
            None => return Err(Error::new(path.span(), "not an identifier")),
        },
        _ => return Err(Error::new(left.span(), "not an identifier")),
    };
    let value = translate_expr(right, scope, env)?;

    scope.assign_quantifier(quantifier_ident, value)?;
    Ok(quote! { () })
}

fn translate_match_expr(
    match_: &ExprMatch,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprMatch { expr, arms, .. } = match_;

    let pattern_tokens = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, scope, env)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };

    let free_quantifiers = scope.free_quantifiers();
    let mutable_quantifiers = scope.mutable_quantifiers();
    let generics: Vec<_> = free_quantifiers.keys().collect();

    // generate trait names
    let match_trait_name = env
        .register_trait_name("MatchArm")
        .expect("the trait name cannot proper prefix of existing trait names");

    let side_effect_trait_names: HashMap<_, _> = mutable_quantifiers
        .iter()
        .map(|(_ident, Variable { id: var_id, .. })| {
            let trait_name = env
                .register_trait_name("MatchSideEffect")
                .expect("please report bug: accidentally using a proper prefix");
            (var_id, trait_name)
        })
        .collect();

    // generate trait items
    // TODO: avoid name collision on generics
    let match_trait_item = quote! {
        trait #match_trait_name < #(#generics),* , __> {
            type Output;
        }
    };

    let side_effect_trait_items: Vec<_> = side_effect_trait_names
        .iter()
        .map(|(_var_id, trait_name)| {
            quote! {
                trait #trait_name < #(#generics),* , __> {
                    type Output;
                }
            }
        })
        .collect();

    // generate impl items

    let impl_items = {
        // clone first to avoid unnecessary trait bounds
        let branched_scope = scope.clone();

        let impl_items: Vec<_> = arms
            .iter()
            .map(|arm| -> syn::Result<_> {
                let Arm { pat, body, .. } = arm;
                let mut branched_scope = branched_scope.clone();

                let pat_tokens = {
                    let ty = scope.type_var_builder().from_pat(pat)?;
                    quote! { #ty }
                };
                let body_tokens = translate_expr(body, &mut branched_scope, env)?;
                let trait_bounds_tokens: Vec<_> = branched_scope
                    .trait_bounds()
                    .iter()
                    .map(|(predicate, bounds)| {
                        quote! { #predicate: #bounds }
                    })
                    .collect();

                // impl item for matched type
                let match_impl = quote! {
                    impl< #(#generics),* > #match_trait_name<#(#generics),* , #pat_tokens> for ()
                    where
                        #(#trait_bounds_tokens),*
                    {
                        type Output = #body_tokens;
                    }
                };

                // add trait bound for match impl
                scope.insert_trait_bounds(
                    quote! { () },
                    quote! { #match_trait_name<#(#generics),* , #pat_tokens> },
                );

                // impls for side effects
                let side_effect_impls: Vec<_> = mutable_quantifiers
                .iter()
                .map(|(_ident, Variable { id: var_id, .. })| {
                    let trait_name = &side_effect_trait_names[var_id];
                    let value = branched_scope
                        .get_variable(var_id)
                        .expect("please report bug: the variable is missing")
                        .value;

                    // add trait bound for side effect trait
                    scope.insert_trait_bounds(
                        quote! { () },
                        quote! { #trait_name< #(#generics),* , #pat_tokens > },
                    );

                    quote! {
                        impl< #(#generics),* > #trait_name< #(#generics),* , #pat_tokens > for ()
                        where
                            #(#trait_bounds_tokens),*
                        {
                            type Output = #value;
                        }
                    }
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
    for (ident, Variable { id: var_id, .. }) in mutable_quantifiers.iter() {
        let trait_name = &side_effect_trait_names[var_id];
        scope.assign_quantifier(
            ident,
            quote! {
                < () as #trait_name < #(#generics),* , #pattern_tokens > >::Output
            },
        )?;
    }

    // construct returned value
    let output = quote! {
        < () as #match_trait_name < #(#generics),* , #pattern_tokens > >::Output
    };

    Ok(output)
}

fn translate_path_expr(
    ExprPath { qself, path, .. }: &ExprPath,
    scope: &mut Scope,
    _env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let value = scope.type_var_builder().from_path(qself.as_ref(), path)?;
    Ok(quote! { #value })
}

fn translate_tuple_expr(
    tuple: &ExprTuple,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let types: Vec<_> = tuple
        .elems
        .iter()
        .map(|expr| translate_expr(expr, scope, env))
        .try_collect()?;
    Ok(quote! { ( #(#types),*  )})
}

fn translate_binary_expr(
    ExprBinary {
        left, right, op, ..
    }: &ExprBinary,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let left_ty = translate_expr(left, scope, env)?;
    let right_ty = translate_expr(right, scope, env)?;

    let mut make_bin_op = |op: &'static str, left_ty: TokenStream, right_ty: TokenStream| {
        let op = format_ident!("{}", op);
        let trait_ = quote! {
            core::ops::#op < #right_ty >
        };
        let output = quote! {
            < #left_ty as #trait_ > :: Output
        };

        // add trait bound
        scope.insert_trait_bounds(left_ty, trait_);

        output
    };

    let out_ty = match op {
        BinOp::Add(_) => make_bin_op("Add", left_ty, right_ty),
        BinOp::Sub(_) => make_bin_op("Sub", left_ty, right_ty),
        BinOp::Div(_) => make_bin_op("Div", left_ty, right_ty),
        BinOp::Mul(_) => make_bin_op("Mul", left_ty, right_ty),
        BinOp::And(_) => make_bin_op("BitAnd", left_ty, right_ty),
        BinOp::Or(_) => make_bin_op("BitOr", left_ty, right_ty),
        BinOp::BitAnd(_) => make_bin_op("BitAnd", left_ty, right_ty),
        BinOp::BitOr(_) => make_bin_op("BitOr", left_ty, right_ty),
        BinOp::BitXor(_) => make_bin_op("BitXor", left_ty, right_ty),
        _ => {
            return Err(Error::new(
                op.span(),
                "the binary operator is not supported",
            ))
        }
    };

    Ok(out_ty)
}

fn translate_if_expr(if_: &ExprIf, scope: &mut Scope, env: &mut Env) -> syn::Result<TokenStream>
where
{
    let ExprIf {
        cond,
        then_branch,
        else_branch,
        ..
    } = if_;

    let free_quantifiers = scope.free_quantifiers();
    let mutable_quantifiers = scope.mutable_quantifiers();
    let generics: Vec<_> = free_quantifiers.keys().collect();

    // generate predicate tokens
    let cond_ty = translate_expr(&*cond, scope, env)?;
    let eq_trait = quote! { core::ops::Eq<typenum::B1> };
    let cond_predicate = quote! { < #cond_ty as #eq_trait >::Output };

    // add trait bound
    scope.insert_trait_bounds(cond_ty, eq_trait);

    // generate trait names
    let if_trait_name = env
        .register_trait_name("If")
        .expect("the trait name cannot proper prefix of existing trait names");

    let side_effect_trait_names: HashMap<_, _> = mutable_quantifiers
        .iter()
        .map(|(_ident, Variable { id: var_id, .. })| {
            let trait_name = env
                .register_trait_name("IfSideEffect")
                .expect("please report bug: accidentally using a proper prefix");
            (var_id, trait_name)
        })
        .collect();

    // generate traits
    let if_trait_item = quote! {
        trait #if_trait_name < #(#generics),* , __> {
            type Output;
        }
    };

    let side_effect_trait_items: Vec<_> = side_effect_trait_names
        .iter()
        .map(|(_var_id, trait_name)| {
            quote! {
                trait #trait_name < #(#generics),* , __> {
                    type Output;
                }
            }
        })
        .collect();

    // generate impl items
    // clone first to avoid unnecessary trait bounds
    let impls = {
        let branched_scope = scope.clone();

        let impls: Vec<_> = match else_branch {
            Some((_else, else_expr)) => {
                let if_impls: Vec<_> = {
                    let mut branched_scope = branched_scope.clone();
                    let then_tokens = translate_block(then_branch, &mut branched_scope, env)?;

                    let body_impl = quote! {
                        impl< #(#generics),* > #if_trait_name<#(#generics),* , typenum::B1> for () {
                            type Output = #then_tokens;
                        }
                    };

                    // add trait bound
                    scope.insert_trait_bounds(
                        quote! { () },
                        quote! { #if_trait_name<#(#generics),* , typenum::B1> },
                    );

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|(_ident, Variable { id: var_id, .. })| {
                            let trait_name = &side_effect_trait_names[var_id];
                            let value = branched_scope
                                .get_variable(var_id)
                                .expect("please report bug: the variable is missing")
                                .value;

                            // add trait bound
                            scope.insert_trait_bounds(
                                quote!{ () },
                                quote!{ #trait_name<#(#generics),* , typenum::B1> }
                            );


                            quote! {
                                impl< #(#generics),* > #trait_name<#(#generics),* , typenum::B1> for () {
                                    type Output = #value;
                                }
                            }
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                let else_impls: Vec<_> = {
                    let mut branched_scope = scope.clone();
                    let else_tokens = translate_expr(&**else_expr, &mut branched_scope, env)?;

                    let body_impl = quote! {
                        impl< #(#generics),* > #if_trait_name<#(#generics),* , typenum::B0> for () {
                            type Output = #else_tokens;
                        }
                    };

                    // add trait bound
                    scope.insert_trait_bounds(
                        quote! { () },
                        quote! { #if_trait_name<#(#generics),* , typenum::B0> },
                    );

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|(_ident, Variable { id: var_id, .. })| {
                            let trait_name = &side_effect_trait_names[var_id];
                            let value = branched_scope
                                .get_variable(var_id)
                                .expect("please report bug: the variable is missing")
                                .value;

                            // add trait bound
                            scope.insert_trait_bounds(
                                quote!{ () },
                                quote!{ #trait_name<#(#generics),* , typenum::B0> }
                            );

                            quote! {
                                impl< #(#generics),* > #trait_name<#(#generics),* , typenum::B0> for () {
                                    type Output = #value;
                                }
                            }
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                if_impls.into_iter().chain(else_impls).collect()
            }
            None => {
                let if_impls: Vec<_> = {
                    let mut branched_scope = scope.clone();
                    let then_tokens = translate_block(then_branch, &mut branched_scope, env)?;

                    let body_impl = quote! {
                        impl< #(#generics),* > #if_trait_name<#(#generics),* , typenum::B1> for () {
                            type Output = ();
                        }
                    };

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|(_ident, Variable { id: var_id, .. })| {
                            let trait_name = &side_effect_trait_names[var_id];
                            let value = branched_scope
                                .get_variable(var_id)
                                .expect("please report bug: the variable is missing")
                                .value;

                            quote! {
                                impl< #(#generics),* > #trait_name<#(#generics),* , typenum::B1> for () {
                                    type Output = #value;
                                }
                            }
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                let else_impls: Vec<_> = {
                    let body_impl = quote! {
                        impl< #(#generics),* > #if_trait_name<#(#generics),* , typenum::B0> for () {
                            type Output = ();
                        }
                    };

                    let side_effect_impls: Vec<_> = mutable_quantifiers
                        .iter()
                        .map(|(_ident, Variable { id: var_id, .. })| {
                            let trait_name = &side_effect_trait_names[var_id];
                            let value = scope
                                .get_variable(var_id)
                                .expect("please report bug: the variable is missing")
                                .value;

                            quote! {
                                impl< #(#generics),* > #trait_name<#(#generics),* , typenum::B0> for () {
                                    type Output = #value;
                                }
                            }
                        })
                        .collect();

                    iter::once(body_impl).chain(side_effect_impls).collect()
                };

                if_impls.into_iter().chain(else_impls).collect()
            }
        };

        impls
    };

    // add items to env
    env.add_item(if_trait_item);
    env.extend_items(side_effect_trait_items);
    env.extend_items(impls);

    // assign affected variables
    for (ident, Variable { id: var_id, .. }) in mutable_quantifiers.iter() {
        let trait_name = &side_effect_trait_names[var_id];
        scope.assign_quantifier(
            ident,
            quote! {
                < () as #trait_name < #(#generics),* , #cond_predicate > >::Output
            },
        )?;
    }

    // construct return type
    let output = quote! {
        < () as #if_trait_name < #(#generics),* , #cond_predicate > >::Output
    };

    Ok(output)
}

fn translate_block_expr(
    block: &ExprBlock,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream> {
    translate_block(&block.block, scope, env)
}

fn translate_call_expr(
    call: &ExprCall,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprCall { func, args, .. } = call;

    let func = match &**func {
        Expr::Path(ExprPath {
            qself: None, path, ..
        }) => scope.trait_var_builder().from_path(path)?,
        Expr::Path(ExprPath { qself: Some(_), .. }) => {
            return Err(Error::new(func.span(), "not a trait"))
        }
        _ => return Err(Error::new(func.span(), "not a trait")),
    };
    let args: Vec<_> = args
        .iter()
        .map(|arg| translate_expr(arg, scope, env))
        .try_collect()?;

    let func_trait = if args.is_empty() {
        quote! { #func }
    } else {
        quote! { #func < #(#args),* >  }
    };
    let output = quote! { < () as #func_trait >::Output };

    scope.insert_trait_bounds(quote! { () }, func_trait);

    Ok(output)
}

// fn ty_to_trait_bounds(ty: &Type) -> syn::Result<Vec<&Path>>
// where
//
// {
//     let bounds = match ty {
//         Type::Infer(_) => vec![],
//         Type::Path(path) => vec![&path.path],
//         Type::TraitObject(tobj) => {
//             let paths = tobj
//                 .bounds
//                 .iter()
//                 .map(|param_bound| match param_bound {
//                     TypeParamBound::Trait(bound) => Ok(&bound.path),
//                     TypeParamBound::Lifetime(lifetime) => {
//                         Err(Error::new(lifetime.span(), "lifetime is not allowed"))
//                     }
//                 })
//                 .try_collect()?;
//             paths
//         }
//         _ => {
//             return Err(Error::new(ty.span(), "not a trait bound"));
//         }
//     };

//     Ok(bounds)
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
// }
