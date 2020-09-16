use crate::common::*;
use typenum::consts::*;

mod animal_test {
    use super::*;

    pub trait Animal {}

    pub struct Dog;
    impl Animal for Dog {}

    pub struct Cat;
    impl Animal for Cat {}

    pub struct Mouse;
    impl Animal for Mouse {}

    typ! {
        fn MatchTest1<A>(A: Animal) -> Unsigned
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
        let _: AssertSameOp<MatchTest1Op<Dog>, U1> = ();
        let _: AssertSameOp<MatchTest1Op<Cat>, U2> = ();
        let _: AssertSameOp<MatchTest1Op<Mouse>, U3> = ();
    }
}

mod recursive_append_list_test {
    use super::*;

    // traits and types

    pub trait List {}

    pub struct Cons<Head, Tail>
    where
        Tail: List,
    {
        _head: Head,
        _tail: Tail,
    }

    impl<Head, Tail> List for Cons<Head, Tail> where Tail: List {}

    pub struct Nil;

    impl List for Nil {}

    // append type operator

    typ! {
        fn Append<input, value>(input: List, value: _) -> List {
            match input {
                #[generics(head, tail: List)]
                Cons::<head, tail> => {
                    let new_tail = Append(tail, value);
                    Cons::<head, new_tail>
                }
                Nil => {
                    Cons::<value, Nil>
                }
            }
        }
    }

    #[test]
    fn test() {
        type List0 = Nil;
        type List1 = Cons<U0, Nil>;
        type List2 = Cons<U0, Cons<U1, Nil>>;
        type List3 = Cons<U0, Cons<U1, Cons<U2, Nil>>>;
        type List4 = Cons<U0, Cons<U1, Cons<U2, Cons<U3, Nil>>>>;

        let _: AssertSameOp<AppendOp<List0, U0>, List1> = ();
        let _: AssertSameOp<AppendOp<List1, U1>, List2> = ();
        let _: AssertSameOp<AppendOp<List2, U2>, List3> = ();
        let _: AssertSameOp<AppendOp<List3, U3>, List4> = ();
    }

}

mod attributes_test {
    use super::*;

    struct Alice<X>(X);
    struct Bob<X>(X);

    typ! {
        fn Compare<lhs, rhs>(lhs: _, rhs: _) {
            let lval = match lhs {
                #[generics(val)]
                Alice::<val> => val,
                #[generics(val)]
                Bob::<val> => val,
            };

            match rhs {
                #[capture(lval)]
                Alice::<lval> => (),
                #[capture(lval)]
                Bob::<lval> => (),
            }
        }
    }

    #[test]
    fn test() {
        let _: AssertSameOp<CompareOp<Alice<B0>, Alice<B0>>, ()> = ();
        let _: AssertSameOp<CompareOp<Alice<B0>, Bob<B0>>, ()> = ();
        let _: AssertSameOp<CompareOp<Bob<B0>, Alice<B0>>, ()> = ();
        let _: AssertSameOp<CompareOp<Bob<B0>, Bob<B0>>, ()> = ();
        let _: AssertSameOp<CompareOp<Alice<B1>, Alice<B1>>, ()> = ();
        let _: AssertSameOp<CompareOp<Alice<B1>, Bob<B1>>, ()> = ();
        let _: AssertSameOp<CompareOp<Bob<B1>, Alice<B1>>, ()> = ();
        let _: AssertSameOp<CompareOp<Bob<B1>, Bob<B1>>, ()> = ();
    }
}

#[test]
fn compile_fail_test() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/macro/fail_match_attribute.rs");
}
