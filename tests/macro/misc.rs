use crate::common::*;

typ::typ! {
    fn Empty() {}
}

#[test]
fn test() {
    let _: AssertSameOp<EmptyOp, ()> = ();
}
