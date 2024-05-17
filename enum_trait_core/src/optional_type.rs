use enum_trait::meta;

use crate::meta_bool::*;

meta! {
    pub enum trait OptionalType<trait Bound: ?Sized> {
        NoType,
        SomeType<T: Bound>,
    }

    trait impl<trait Bound: ?Sized> OptionalType<Bound> {
        pub type IsNone: MetaBool = match <Self> {
            NoType => True,
            SomeType<T: Bound> => False,
        };

        pub type IsSome: MetaBool = match <Self> {
            NoType => False,
            SomeType<T: Bound> => True,
        };

        /*pub type UnwrapOr<X: Bound>: Bound = match <Self> {
            NoType => X,
            SomeType<T: Bound> => T,
        };*/
    }
}

#[cfg(test)]
mod tests {
    use enum_trait::const_test;

    use super::*;

    #[const_test]
    const fn is_none_or_some() {
        assert!(<NoType as OptionalType>::IsNone::VALUE);
        assert!(!<NoType as OptionalType>::IsSome::VALUE);
        assert!(!<SomeType<()> as OptionalType>::IsNone::VALUE);
        assert!(<SomeType<()> as OptionalType>::IsSome::VALUE);
    }
}
