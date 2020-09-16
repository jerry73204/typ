use crate::common::*;
use typenum::consts::*;

typ! {
    fn Empty() {}

    fn MethodCall<lhs, rhs>(lhs: Unsigned, rhs: Unsigned) -> Unsigned {
        lhs.Max(rhs)
    }
}

typ! {
    impl<V: Unsigned> V {
        fn GetOneIfIsSquareOf<rhs>(self, rhs: Unsigned) -> Unsigned {
            if self == rhs * rhs { 1u } else { 0u }
        }
    }
}

#[test]
fn test() {
    let _: AssertSameOp<EmptyOp, ()> = ();
    let _: AssertSameOp<MethodCallOp<U3, U7>, U7> = ();
    let _: AssertSameOp<GetOneIfIsSquareOfOp<U49, U7>, U1> = ();
    let _: AssertSameOp<GetOneIfIsSquareOfOp<U5, U2>, U0> = ();
}
