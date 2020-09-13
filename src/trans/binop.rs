use super::*;

pub fn translate_binary_expr(
    ExprBinary {
        left, right, op, ..
    }: &ExprBinary,
    scope: &mut Env,
    items: &mut Vec<Item>,
) -> syn::Result<TypeVar>
where
{
    // parse lhs and rhs
    let lhs = translate_expr(left, scope, items)?;
    let rhs = translate_expr(right, scope, items)?;

    // compute output
    match op {
        BinOp::Add(_) => std_bin_op(scope, quote! { core::ops::Add }, lhs, rhs),
        BinOp::Sub(_) => std_bin_op(scope, quote! { core::ops::Sub }, lhs, rhs),
        BinOp::Div(_) => std_bin_op(scope, quote! { core::ops::Div }, lhs, rhs),
        BinOp::Mul(_) => std_bin_op(scope, quote! { core::ops::Mul }, lhs, rhs),
        BinOp::And(_) => std_bin_op(scope, quote! { core::ops::BitAnd }, lhs, rhs),
        BinOp::Or(_) => std_bin_op(scope, quote! { core::ops::BitOr }, lhs, rhs),
        BinOp::BitAnd(_) => std_bin_op(scope, quote! { core::ops::BitAnd }, lhs, rhs),
        BinOp::BitOr(_) => std_bin_op(scope, quote! { core::ops::BitOr }, lhs, rhs),
        BinOp::BitXor(_) => std_bin_op(scope, quote! { core::ops::BitXor }, lhs, rhs),
        BinOp::Rem(_) => std_bin_op(scope, quote! { core::ops::Rem }, lhs, rhs),
        BinOp::Lt(_) => typenum_bin_op(scope, quote! { typenum::type_operators::IsLess }, lhs, rhs),
        BinOp::Gt(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsGreater },
            lhs,
            rhs,
        ),
        BinOp::Le(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsLessOrEqual },
            lhs,
            rhs,
        ),
        BinOp::Ge(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsGreaterOrEqual },
            lhs,
            rhs,
        ),
        BinOp::Eq(_) => {
            typenum_bin_op(scope, quote! { typenum::type_operators::IsEqual }, lhs, rhs)
        }
        BinOp::Ne(_) => typenum_bin_op(
            scope,
            quote! { typenum::type_operators::IsNotEqual },
            lhs,
            rhs,
        ),
        _ => {
            return Err(Error::new(
                op.span(),
                "the binary operator is not supported",
            ))
        }
    }
}

fn std_bin_op(
    scope: &mut Env,
    trait_tokens: TokenStream,
    lhs: TypeVar,
    rhs: TypeVar,
) -> syn::Result<TypeVar> {
    let trait_path = syn::parse2::<Path>(trait_tokens)?.parse_pure_path(&mut vec![])?;

    let (output, predicate) = {
        let trait_ = {
            let mut path = trait_path.clone();
            path.segments.last_mut().as_mut().unwrap().arguments =
                PathArgumentsVar::AngleBracketed(vec![rhs]);
            path
        };
        let path = {
            let mut path = trait_.clone();
            path.segments.push(SegmentVar {
                ident: format_ident!("Output"),
                arguments: PathArgumentsVar::None,
            });
            path
        };
        let predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty: lhs.clone(),
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: trait_.clone(),
            })],
        });
        let output = TypeVar::Path(TypePathVar {
            qself: Some(QSelfVar {
                ty: Box::new(lhs),
                position: trait_.segments.len(),
            }),
            path,
        });
        (output, predicate)
    };

    scope.insert_predicate(predicate);

    Ok(output)
}

fn typenum_bin_op(
    scope: &mut Env,
    trait_tokens: TokenStream,
    lhs: TypeVar,
    rhs: TypeVar,
) -> syn::Result<TypeVar> {
    let trait_path = syn::parse2::<Path>(trait_tokens)?.parse_pure_path(&mut vec![])?;

    let (output, apply_predicates, output_predicates) = {
        let trait_ = {
            let mut path = trait_path.clone();
            path.segments.last_mut().as_mut().unwrap().arguments =
                PathArgumentsVar::AngleBracketed(vec![rhs]);
            path
        };
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
                ty: Box::new(lhs.clone()),
                position: trait_.segments.len(),
            }),
            path,
        });
        let apply_predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty: lhs,
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: trait_,
            })],
        });
        let output_predicate = WherePredicateVar::Type(PredicateTypeVar {
            bounded_ty: output.clone(),
            bounds: vec![TypeParamBoundVar::Trait(TraitBoundVar {
                modifier: TraitBoundModifierVar::None,
                path: syn::parse2::<Path>(quote! { typenum::marker_traits::Bit })
                    .unwrap()
                    .parse_pure_path(&mut vec![])
                    .unwrap(),
            })],
        });
        (output, apply_predicate, output_predicate)
    };

    scope.insert_predicate(apply_predicates);
    scope.insert_predicate(output_predicates);

    Ok(output)
}
