use crate::common::*;

typ! {
    fn Empty() {}
}

#[test]
fn test() {
    let _: AssertSameOp<EmptyOp, ()> = ();
}
