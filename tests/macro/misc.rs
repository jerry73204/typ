use crate::common::*;
use typenum::consts::*;

typ! {
    fn Empty() {}

    fn MethodCall<lhs, rhs>(lhs: Unsigned, rhs: Unsigned) -> Unsigned {
        lhs.Max(rhs)
    }
}

#[test]
fn test() {
    let _: AssertSameOp<EmptyOp, ()> = ();
    let _: AssertSameOp<MethodCallOp<U3, U7>, U7> = ();
}
