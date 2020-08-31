use super::common::*;

typ::typ! {
    fn IfTest1<Cond>(Cond: _) {
        if Cond {
            1
        }
    }

    fn IfTest2<Cond>(Cond: _) {
        if Cond {
            1
        } else {
            -1
        }
    }

    fn IfTest3<Cond>(Cond: typenum::Bit) {
        let mut value = 0;

        if Cond {
            value = 1;
        }
        value
    }

    fn IfTest4<Cond>(Cond: typenum::Bit) {
        let mut value = 0;

        if Cond {
            value = 1;
        } else {
            value = -1;
        }

        value
    }

    fn IfTest5<Input>(Input: typenum::Integer) {
        let mut value: typenum::Integer = 0;

        if Input < 5 {
            value = 1;
        } else {
            value = -1;
        }

        let output = if value >= 0 {
            7
        } else {
            -7
        };

        output
    }

    fn IfTest6<L, R>(L: typenum::Integer, R: typenum::Integer) {
        if L % 2 == 1 {
            if R % 2 == 1 {
                3
            } else {
                2
            }
        } else if R % 2 == 1 {
            1
        } else {
            0
        }
    }
}

#[test]
fn test() {
    use typenum::consts::*;

    let _: AssertSameOp<IfTest1Op<B0>, ()> = ();
    let _: AssertSameOp<IfTest1Op<B1>, ()> = ();

    let _: AssertSameOp<IfTest2Op<B0>, N1> = ();
    let _: AssertSameOp<IfTest2Op<B1>, P1> = ();

    let _: AssertSameOp<IfTest3Op<B0>, Z0> = ();
    let _: AssertSameOp<IfTest3Op<B1>, P1> = ();

    let _: AssertSameOp<IfTest4Op<B0>, N1> = ();
    let _: AssertSameOp<IfTest4Op<B1>, P1> = ();

    let _: AssertSameOp<IfTest5Op<P4>, P7> = ();
    let _: AssertSameOp<IfTest5Op<P5>, N7> = ();

    let _: AssertSameOp<IfTest6Op<P4, P2>, Z0> = ();
    let _: AssertSameOp<IfTest6Op<P4, P3>, P1> = ();
    let _: AssertSameOp<IfTest6Op<P5, P2>, P2> = ();
    let _: AssertSameOp<IfTest6Op<P7, P9>, P3> = ();
}
