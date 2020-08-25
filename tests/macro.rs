use std::marker::PhantomData;

pub trait List {}

pub struct Cons<Head, Tail>
where
    Tail: List,
{
    _phantom: PhantomData<(Head, Tail)>,
}

impl<Head, Tail> List for Cons<Head, Tail> where Tail: List {}

pub struct Nil;

impl List for Nil {}

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

mod match_test {
    pub trait Animal {}
    pub struct Dog;
    pub struct Cat;
    pub struct Mouse;

    typ::typ! {
        fn MatchTest1<animal>(animal: Animal) -> typenum::Unsigned {
            match animal {
                Dog => 1u,
                Cat => 2u,
                Mouse => 3u,
            }
        }

        fn MatchTest2<animal>(animal: Animal) -> typenum::Unsigned {
            let value: typenum::Unsigned = match animal {
                Dog => 1u,
                Cat => 2u,
                Mouse => 3u,
            };
            value
        }

        fn MatchTest3<animal>(animal: Animal) -> typenum::Unsigned {
            let mut value: typenum::Unsigned = 0u;
            match animal {
                Dog => value = 1u,
                Cat => value = 2u,
                Mouse => value = 3u,
            }
            value
        }
    }
}

// typ::typ! {
//     fn Recursive<N>(N: typenum::Unsigned) {
//         if N == 0 {
//             ()
//         } else {
//             Recursive(N - 1)
//         }
//     }
// }
