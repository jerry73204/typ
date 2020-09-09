# TYP: type-level programming in Rust

TYP enables you to write _type operators_, the functions that translates types, in Rust syntax.

It is redesign of [willcrichton/Tyrade](https://github.com/willcrichton/tyrade) and was inspired by [jerry73204/type-freak](https://github.com/jerry73204/rust-type-freak).

## Features

### Rusty syntax

TYP adopts Rust-like syntax, where values become types, and types become trait bounds. The core concept is the type operator, which is a function that takes type arguments and produce types. Trait bounds are optionally added to input and output types.

```rust
fn TypeOperatorName<generic1, generic2>(type1: _, type2: Trait1 + Trait2) -> TraitBound { ... }
```

- `<generic1, generic2>` lists the generic identifiers that helps disginguishing from public types.
- `type1` and `type2` are input types composed of generics and public types.
- `type1: _` means the type has no trait bound.
- The output trait bound `fn() -> TraitBound` is optional.


The snipplet demonstrates a simple type operator.

```rust
typ! {
    use typenum::Unsigned;

    fn Add<lhs, rhs>(lhs: Unsigned, rhs: Unsigned) -> Unsigned {
        lhs + rhs
    }
}
```

### Built-in typenum support

TYP has first-class support to [typenum](https://github.com/paholg/typenum). Integer literals are translated to typenum types. The following literals are understood by TYP.

- Signed integers: `7` or `7i`
- Unsigned integers: `7u`
- Bits: `true` and `false`

Common binary and unary operators applies on types with appropriate traits. For example, `A + B` expands to `<A as Add<B>>::Output` if `A` implements `Add<B>`.

```rust
typ! {
    use typenum::{Integer, Bit};

    fn IsOdd<value>(value: Integer) -> Bit {
        if value % 2 == 1 {
            true
        } else {
            false
        }
    }
}
```

### Type matching

Like normal Rust, the `match` syntax lets you match and unpack types. You can bind new generics on a pattern using `#[generics(...)]` attribute.

The example demonstrates a type operator that appends a type at the end of type-level list. It's done by recursively unpack the list into `Cons` nodes and `Nil` end-of-list marker.

```rust
pub trait List {}

pub struct Cons<Head, Tail: List> { /* omit */ }
impl<Head, Tail: List> List for Cons<Head, Tail> {}

pub struct Nil;
impl List for Nil {}

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

```

## Examples

More advanced examples can be found in [tests/](tests) directory.

- [if/else](tests/macro/if_.rs)
- [match](tests/macro/match_.rs)
- [binary GCD](tests/macro/recursion.rs)

## License

MIT license. See [LICENSE.txt](LICENSE.txt).
