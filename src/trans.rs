use crate::{
    common::*,
    env::Env,
    scope::Scope,
    var::{Scoped, SegmentVar, TraitBoundsVar, TraitVar, TypeVar},
};

pub fn translate_items(items: &[Item]) -> syn::Result<TokenStream> {
    let tokens_vec: Vec<_> = items
        .into_iter()
        .map(|item| {
            let tokens = match item {
                Item::Enum(enum_) => translate_enum(&enum_)?,
                Item::Fn(fn_) => {
                    let ItemFn { sig, block, .. } = fn_;
                    translate_fn(sig, block, None)?
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

    eprintln!("{}", expanded);

    Ok(expanded)
}

fn translate_struct(_struct_: &ItemStruct) -> syn::Result<TokenStream> {
    todo!();
}

fn translate_enum(enum_: &ItemEnum) -> syn::Result<TokenStream> {
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

    let _: Vec<_> = items
        .iter()
        .map(|item| -> syn::Result<_> {
            match item {
                ImplItem::Type(type_) => {
                    let ImplItemType {
                        ident: type_name,
                        ty,
                        ..
                    } = type_;
                    todo!();
                }
                ImplItem::Method(method) => {
                    let ImplItemMethod { sig, block, .. } = method;
                    translate_fn(sig, block, Some(trait_path))?
                }
                _ => {
                    return Err(Error::new(item.span(), "unsupported item kind"));
                }
            };

            Ok(())
        })
        .try_collect()?;

    let expanded = quote! {};
    Ok(expanded)
}

fn translate_fn(
    sig: &Signature,
    block: &Block,
    self_trait_opt: Option<&Path>,
) -> syn::Result<TokenStream> {
    let Signature {
        ident: trait_op_name,
        generics,
        inputs,
        output,
        ..
    } = sig;

    // create root scope and env
    let mut env = Env::new();
    let mut scope = Scope::new();

    // TODO
    // match (self_trait_opt, inputs.first()) {
    //     (
    //         Some(self_trait),
    //         Some(FnArg::Receiver(Receiver {
    //             reference: None, ..
    //         })),
    //     ) => {
    //         // TODO
    //     }
    //     _ => {
    //         return Err(Error::new(
    //             sig.span(),
    //             r#"methods in impl block must place "self" at the first argument"#,
    //         ))
    //     }
    // };

    // translate function generics to initial quantifiers
    {
        // function generics (ident, [path])
        let generic_args: Vec<(&Ident, _)> = generics
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
        for (ident, _paths) in generic_args.iter().cloned() {
            scope.insert_initial_quantifier(ident.to_owned());
        }

        // add trait bounds for initial quantifiers
        for (ident, bounds) in generic_args.iter().cloned() {
            let predicate = scope.type_var_builder().from_ident(ident);
            let bounds = scope
                .trait_bounds_var_builder()
                .from_type_param_bounds(&bounds)?;
            scope.insert_trait_bounds(quote! { #predicate }, quote! { #bounds });
        }
    }

    // turn function arguments into trait bounds
    {
        // get function arguments (type, trait bounds)
        let fn_args: Vec<_> = inputs
            .iter()
            .filter_map(|arg| match arg {
                FnArg::Typed(pat_type) => Some(pat_type),
                FnArg::Receiver(_) => None,
            })
            .map(|PatType { pat, ty, .. }| -> syn::Result<_> {
                let type_ = scope.type_var_builder().from_pat(&**pat)?;
                let trait_bounds = scope.trait_bounds_var_builder().from_type(&**ty)?;
                Ok((type_, trait_bounds))
            })
            .try_collect()?;

        // insert trait bounds
        for (predicate, bounds) in fn_args {
            scope.insert_trait_bounds(quote! { #predicate }, quote! { #bounds });
        }
    }

    // translate output type to trait bound
    let output_trait_bounds = match output {
        ReturnType::Default => scope.trait_bounds_var_builder().empty(),
        ReturnType::Type(_, ty) => scope.trait_bounds_var_builder().from_type(ty)?,
    };

    // translate block
    let value = {
        let mut sub_env = env
            .create_mod(trait_op_name.clone())
            .expect("please report bug: a mod is created twice");

        translate_block(&block, &mut scope, &mut sub_env)?
    };

    // build trait and impls
    env.create_trait_by_name(trait_op_name.clone(), |env, builder| {
        todo!();
    });

    todo!();
}

fn translate_block(block: &Block, scope: &mut Scope, env: &mut Env) -> syn::Result<TokenStream>
where
{
    let mut output_value = None;

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
                    scope.insert_quantifier(ident.to_owned(), value, is_mut);
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
                    assert!(
                        matches!(output_value, None),
                        "please report bug: output type is set more than once"
                    );
                    output_value = Some(translate_expr(expr, scope, env)?);
                }
                Stmt::Semi(expr, _semi) => {
                    translate_expr(expr, scope, env)?;
                }
            }
        }

        Ok(())
    })?;

    todo!();
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
        _ => Err(Error::new(expr.span(), "invalid expression")),
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

    if !scope.assign_quantifier(quantifier_ident, value) {
        return Err(Error::new(
            quantifier_ident.span(),
            "the variable is not declared or not mutable",
        ));
    }

    todo!();
}

fn translate_match_expr(
    match_: &ExprMatch,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream>
where
{
    let ExprMatch { expr, arms, .. } = match_;

    let matched_type = match &**expr {
        Expr::Path(path) => translate_path_expr(&path, scope, env)?,
        _ => return Err(Error::new(expr.span(), "not a type")),
    };

    let _: Vec<_> = match_
        .arms
        .iter()
        .map(|arm| -> syn::Result<_> {
            let Arm { pat, body, .. } = arm;

            let branched_scope = scope.clone();

            Ok(())
        })
        .try_collect()?;

    todo!();
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
        let trait_ = quote! {
            core::op::#op < #right_ty >
        };
        let output = quote! {
            < #left_ty as #trait_ >::Output
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
        BinOp::And(_) => make_bin_op("And", left_ty, right_ty),
        BinOp::Or(_) => make_bin_op("Or", left_ty, right_ty),
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
    env.create_trait_by_prefix("If", |env, name, builder| -> syn::Result<()> {
        let ExprIf {
            cond,
            then_branch,
            else_branch,
            ..
        } = if_;

        let cond = translate_expr(&*cond, scope, env)?;
        let eq_trait = quote! { core::op::Eq< typenum::B1 };
        let cond_ty = quote! { < #cond as #eq_trait >::Output };

        // add trait bound
        scope.insert_trait_bounds(cond, eq_trait);

        Ok(())
    })?;

    todo!();
}

fn translate_block_expr(
    block: &ExprBlock,
    scope: &mut Scope,
    env: &mut Env,
) -> syn::Result<TokenStream> {
    // solve unlimited recursion
    // translate_block(&block.block, scope, env)
    todo!();
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

#[cfg(test)]
mod tests {
    use super::*;
}
