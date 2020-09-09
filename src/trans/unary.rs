use super::*;

pub fn translate_unary_expr(
    ExprUnary { op, expr, .. }: &ExprUnary,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar> {
    let operand = translate_expr(expr, scope, items)?;

    let output = match op {
        UnOp::Neg(_) => {
            let (output, predicate) = {
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
                        ty: Box::new(operand.clone()),
                        position: trait_.segments.len(),
                    }),
                    path,
                });
                let predicate = WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty: operand,
                    bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                        modifier: TraitBoundModifierVar::None,
                        path: trait_,
                    })],
                });

                (output, predicate)
            };

            scope.insert_predicate(predicate);
            output
        }
        UnOp::Not(_) => {
            let (output, predicate) = {
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
                        ty: Box::new(operand.clone()),
                        position: trait_.segments.len(),
                    }),
                    path,
                });
                let predicate = WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty: operand,
                    bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                        modifier: TraitBoundModifierVar::None,
                        path: trait_,
                    })],
                });

                (output, predicate)
            };

            scope.insert_predicate(predicate);
            output
        }
        UnOp::Deref(_) => {
            let (output, predicate) = {
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
                        ty: Box::new(operand.clone()),
                        position: trait_.segments.len(),
                    }),
                    path,
                });
                let predicate = WherePredicateVar::Type(PredicateTypeVar {
                    bounded_ty: operand,
                    bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                        modifier: TraitBoundModifierVar::None,
                        path: trait_,
                    })],
                });

                (output, predicate)
            };

            scope.insert_predicate(predicate);
            output
        }
    };

    Ok(output)
}
