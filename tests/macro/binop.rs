use super::common::*;

typ! {
    fn True() -> Bit {
        true
    }

    fn False() -> Bit {
        false
    }

    fn And<Lhs, Rhs>(Lhs: Bit, Rhs: Bit) -> Bit {
        Lhs && Rhs
    }

    fn Or<Lhs, Rhs>(Lhs: Bit, Rhs: Bit) -> Bit {
        Lhs || Rhs
    }

    fn Not<Value>(Value: Bit) -> Bit {
        !Value
    }

    fn XorV1<Lhs, Rhs>(Lhs: Bit, Rhs: Bit) -> Bit {
        (Lhs && !Rhs) || (!Lhs && Rhs)
    }

    fn XorV2<Lhs, Rhs>(Lhs: Bit, Rhs: Bit) -> Bit {
        Or(And(Lhs, Not(Rhs)), And(Not(Lhs), Rhs))
    }
}

#[test]
fn test() {
    use typenum::{B0, B1};
    let _: AssertSameOp<FalseOp, B0> = ();
    let _: AssertSameOp<TrueOp, B1> = ();

    let _: AssertSameOp<NotOp<B0>, B1> = ();
    let _: AssertSameOp<NotOp<B1>, B0> = ();

    let _: AssertSameOp<OrOp<B0, B0>, B0> = ();
    let _: AssertSameOp<OrOp<B1, B0>, B1> = ();
    let _: AssertSameOp<OrOp<B1, B0>, B1> = ();
    let _: AssertSameOp<OrOp<B1, B1>, B1> = ();

    let _: AssertSameOp<AndOp<B0, B0>, B0> = ();
    let _: AssertSameOp<AndOp<B1, B0>, B0> = ();
    let _: AssertSameOp<AndOp<B1, B0>, B0> = ();
    let _: AssertSameOp<AndOp<B1, B1>, B1> = ();

    let _: AssertSameOp<XorV1Op<B0, B0>, B0> = ();
    let _: AssertSameOp<XorV1Op<B1, B0>, B1> = ();
    let _: AssertSameOp<XorV1Op<B1, B0>, B1> = ();
    let _: AssertSameOp<XorV1Op<B1, B1>, B0> = ();

    let _: AssertSameOp<XorV2Op<B0, B0>, B0> = ();
    let _: AssertSameOp<XorV2Op<B1, B0>, B1> = ();
    let _: AssertSameOp<XorV2Op<B1, B0>, B1> = ();
    let _: AssertSameOp<XorV2Op<B1, B1>, B0> = ();
}
