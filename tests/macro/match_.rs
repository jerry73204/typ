use crate::common::*;

mod animal_test {
    use super::*;

    pub trait Animal {}

    pub struct Dog;
    impl Animal for Dog {}

    pub struct Cat;
    impl Animal for Cat {}

    pub struct Mouse;
    impl Animal for Mouse {}

    typ::typ! {
        fn MatchTest1<A>(A: Animal) -> typenum::Unsigned
        {
            match A {
                Dog => 1u,
                Cat => 2u,
                Mouse => 3u,
            }
        }
    }

    #[test]
    fn test() {
        use typenum::consts::*;

        let _: AssertSameOp<MatchTest1Op<Dog>, U1> = ();
        let _: AssertSameOp<MatchTest1Op<Cat>, U2> = ();
        let _: AssertSameOp<MatchTest1Op<Mouse>, U3> = ();
    }
}

mod list_test {
    use super::*;

    pub trait List {}

    pub struct Cons<Head, Tail>
    where
        Tail: List,
    {
        head: Head,
        tail: Tail,
    }

    impl<Head, Tail> List for Cons<Head, Tail> where Tail: List {}

    pub struct Nil;

    impl List for Nil {}

    typ::typ! {
        fn Append<L>(L: List) -> List {
            match L {
                #[generics(Head, Tail: List)]
                Cons::<Head, Tail> => {}
                Nil => {}
            }
        }
    }
}
