use typ::typ;

pub trait AssertSame<Lhs, Rhs> {
    type Output;
}

impl<T> AssertSame<T, T> for () {
    type Output = ();
}

pub type AssertSameOp<Lhs, Rhs> = <() as AssertSame<Lhs, Rhs>>::Output;

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

fn main() {
    use typenum::consts::*;

    let _: AssertSameOp<CompareOp<Alice<B0>, Alice<B1>>, ()> = ();
    let _: AssertSameOp<CompareOp<Alice<B0>, Bob<B1>>, ()> = ();
    let _: AssertSameOp<CompareOp<Bob<B0>, Alice<B1>>, ()> = ();
    let _: AssertSameOp<CompareOp<Bob<B0>, Bob<B1>>, ()> = ();
    let _: AssertSameOp<CompareOp<Alice<B1>, Alice<B0>>, ()> = ();
    let _: AssertSameOp<CompareOp<Alice<B1>, Bob<B0>>, ()> = ();
    let _: AssertSameOp<CompareOp<Bob<B1>, Alice<B0>>, ()> = ();
    let _: AssertSameOp<CompareOp<Bob<B1>, Bob<B0>>, ()> = ();
}
