use crate::common::*;
use typenum::Unsigned;

mod binary_gcd {
    use super::*;

    typ! {
        fn BinaryGcd<lhs, rhs>(lhs: Unsigned, rhs: Unsigned) -> Unsigned {
            if lhs == rhs {
                lhs
            } else if lhs == 0u {
                rhs
            } else if rhs == 0u {
                lhs
            } else {
                if lhs % 2u == 1u {
                    if rhs % 2u == 1u {
                        if lhs > rhs {
                            let sub: Unsigned = lhs - rhs;
                            BinaryGcd(sub, rhs)
                        } else {
                            let sub: Unsigned = rhs - lhs;
                            BinaryGcd(sub, lhs)
                        }
                    } else {
                        let div: Unsigned = rhs / 2u;
                        BinaryGcd(lhs, div)

                    }
                } else {
                    if rhs % 2u == 1u {
                        let div: Unsigned = lhs / 2u;
                        BinaryGcd(div, rhs)
                    } else {
                        let ldiv: Unsigned = lhs / 2u;
                        let rdiv: Unsigned = rhs / 2u;
                        BinaryGcd(ldiv, rdiv) * 2u
                    }
                }
            }
        }
    }

    #[test]
    fn binary_gcd() {
        use typenum::consts::*;
        let _: AssertSameOp<BinaryGcdOp<U3, U0>, U3> = ();
        let _: AssertSameOp<BinaryGcdOp<U0, U1>, U1> = ();
        let _: AssertSameOp<BinaryGcdOp<U2, U4>, U2> = ();
        let _: AssertSameOp<BinaryGcdOp<U6, U3>, U3> = ();
        let _: AssertSameOp<BinaryGcdOp<U4, U4>, U4> = ();
        let _: AssertSameOp<BinaryGcdOp<U7, U17>, U1> = ();
        let _: AssertSameOp<BinaryGcdOp<U58, U11>, U1> = ();
        let _: AssertSameOp<BinaryGcdOp<tyuint!(624129), tyuint!(2061517)>, tyuint!(18913)> = ();
    }
}
