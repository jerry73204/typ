trait List {}

typ::typ! {
    fn Empty() {}
}

typ::typ! {
    fn True() -> typenum::Bit {
        true
    }

    fn False() -> typenum::Bit {
        false
    }

    fn And<Lhs, Rhs>(Lhs: typenum::Bit, Rhs: typenum::Bit) -> typenum::Bit {
        Lhs && Rhs
    }

    fn Or<Lhs, Rhs>(Lhs: typenum::Bit, Rhs: typenum::Bit) -> typenum::Bit {
        Lhs || Rhs
    }

    fn Not<Value>(Value: typenum::Bit) -> typenum::Bit {
        !Value
    }

    fn XorV1<Lhs, Rhs>(Lhs: typenum::Bit, Rhs: typenum::Bit) -> typenum::Bit {
        (Lhs && !Rhs) || (!Lhs && Rhs)
    }

    fn XorV2<Lhs, Rhs>(Lhs: typenum::Bit, Rhs: typenum::Bit) -> typenum::Bit {
        Or(And(Lhs, Not(Rhs)), And(Not(Lhs), Rhs))
    }
}

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
}
