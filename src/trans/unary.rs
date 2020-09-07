use super::*;

pub fn translate_unary_expr(
    ExprUnary { op, expr, .. }: &ExprUnary,
    scope: &mut Env,
) -> syn::Result<usize> {
    let operand_id = translate_expr(expr, scope)?;
    let operands = scope.pop(operand_id);

    let output_id = match op {
        UnOp::Neg(_) => {
            let (outputs, predicates): (Vec<_>, Vec<_>) = operands
                .into_iter()
                .map(|bounded_ty| {
                    let bounded_ty = bounded_ty.into_type().unwrap();
                    let trait_: PathVar = syn::parse2(quote! { core::ops::Neg }).unwrap();
                    let path = {
                        let mut path = trait_.clone();
                        path.segments.push(SegmentVar {
                            ident: format_ident!("Output"),
                            arguments: PathArgumentsVar::None,
                        });
                        path
                    };
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

                    (output, predicate)
                })
                .unzip();

            scope.insert_predicate(predicates);
            let output_id = scope.push(outputs);
            output_id
        }
        UnOp::Not(_) => {
            let (outputs, predicates): (Vec<_>, Vec<_>) = operands
                .into_iter()
                .map(|bounded_ty| {
                    let bounded_ty = bounded_ty.into_type().unwrap();
                    let trait_: PathVar = syn::parse2(quote! { core::ops::Not }).unwrap();
                    let path = {
                        let mut path = trait_.clone();
                        path.segments.push(SegmentVar {
                            ident: format_ident!("Output"),
                            arguments: PathArgumentsVar::None,
                        });
                        path
                    };
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

                    (output, predicate)
                })
                .unzip();

            scope.insert_predicate(predicates);
            let output_id = scope.push(outputs);
            output_id
        }
        UnOp::Deref(_) => {
            let (outputs, predicates): (Vec<_>, Vec<_>) = operands
                .into_iter()
                .map(|bounded_ty| {
                    let bounded_ty = bounded_ty.into_type().unwrap();
                    let trait_: PathVar = syn::parse2(quote! { core::ops::Deref }).unwrap();
                    let path = {
                        let mut path = trait_.clone();
                        path.segments.push(SegmentVar {
                            ident: format_ident!("Target"),
                            arguments: PathArgumentsVar::None,
                        });
                        path
                    };
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

                    (output, predicate)
                })
                .unzip();

            scope.insert_predicate(predicates);
            let output_id = scope.push(outputs);
            output_id
        }
    };

    Ok(output_id)
}
