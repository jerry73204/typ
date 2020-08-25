trait List {}

mod empty {
    typ::typ! {
        fn Empty() {}
    }
}

mod literals {
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
    }
}
