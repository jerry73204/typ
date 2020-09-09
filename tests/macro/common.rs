pub use typ::{typ, tyuint};

pub trait AssertSame<Lhs, Rhs> {
    type Output;
}

impl<T> AssertSame<T, T> for () {
    type Output = ();
}

pub type AssertSameOp<Lhs, Rhs> = <() as AssertSame<Lhs, Rhs>>::Output;
