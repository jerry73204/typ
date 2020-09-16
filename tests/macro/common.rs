pub use typ::{typ, tyuint};
pub use typenum::{Bit, Integer, Max, Unsigned};

pub trait AssertSame<Lhs, Rhs> {
    type Output;
}

impl<T> AssertSame<T, T> for () {
    type Output = ();
}

pub type AssertSameOp<Lhs, Rhs> = <() as AssertSame<Lhs, Rhs>>::Output;
