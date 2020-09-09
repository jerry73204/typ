use crate::common::*;

typ! {
    fn BinaryGcd<lhs, rhs>(lhs: typenum::Unsigned, rhs: typenum::Unsigned) -> typenum::Unsigned {
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
                        BinaryGcd(lhs - rhs, rhs)
                    } else {
                        BinaryGcd(rhs - lhs, lhs)
                    }
                } else {
                    BinaryGcd(lhs, rhs / 2u)

                }
            } else {
                if rhs % 2u == 1u {
                    BinaryGcd(lhs / 2u, rhs)
                } else {
                    BinaryGcd(lhs / 2u, rhs / 2u) * 2u
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
    let _: AssertSameOp<BinaryGcdOp<tyuint!(624129), tyuint!(2061517)>, tyuint!(18913)> = ();
}
