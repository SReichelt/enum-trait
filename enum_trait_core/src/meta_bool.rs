use enum_trait::meta;

meta! {
    /// A trait that represents a meta-level `bool`. A type param constrained to `MetaBool` is
    /// roughly equivalent to a const param of type `bool`, and in fact it is possible to convert
    /// between the two representations via `VALUE` and `ConstToMetaBool`. However, `MetaBool`
    /// parameters are more flexible due to type-level matching.
    pub enum trait MetaBool {
        False,
        True,
    }

    trait impl MetaBool {
        /// Converts this `MetaBool` instance to a `bool` constant.
        pub const VALUE: bool = match <Self> {
            False => false,
            True => true,
        };

        /// Takes two type parameters and selects one of them depending on whether the given
        /// `MetaBool` instance is `True` or `False`.
        ///
        /// All matching on `MetaBool` instances could be regarded as special cases of `IfElse`,
        /// except that `IfElse` would need to be generic over a type bound, which is not possible.
        /// In fact, this limitation makes `IfElse` almost useless. Instead, to match on a
        /// `MetaBool`, define a trait alias and then use `match`, as described in the documentation
        /// of the `enum_trait` crate.
        pub type IfElse<'a, TrueT: ?Sized + 'a, FalseT: ?Sized + 'a>: ?Sized + 'a = match <Self> {
            False => FalseT,
            True => TrueT,
        };

        /// Converts this `MetaBool` instance to the corresponding type of the `typenum` crate.
        #[cfg(feature = "typenum")]
        pub type ToTypeNum: typenum::Bit = match <Self> {
            False => typenum::B0,
            True => typenum::B1,
        };
    }

    pub type Not<B: MetaBool>: MetaBool = match <B> {
        False => True,
        True => False,
    };

    pub type And<A: MetaBool, B: MetaBool>: MetaBool = match <B> {
        False => False,
        True => A,
    };

    pub type Or<A: MetaBool, B: MetaBool>: MetaBool = match <B> {
        False => A,
        True => True,
    };

    pub type XOr<A: MetaBool, B: MetaBool>: MetaBool = match <B> {
        False => A,
        True => Not<A>,
    };

    pub type Equal<A: MetaBool, B: MetaBool>: MetaBool = match <B> {
        False => Not<A>,
        True => A,
    };
}

/// Converts the given `bool` constant to a `MetaBool` instance.
pub type ConstToMetaBool<const B: bool> =
    <internal::ConstBool<B> as internal::ToMetaBool>::ToMetaBool;

pub type ToMetaBool<B> = <B as internal::ToMetaBool>::ToMetaBool;

pub mod internal {
    use super::*;

    pub trait ToMetaBool {
        type ToMetaBool: MetaBool;
    }

    pub struct ConstBool<const B: bool>(());

    impl ToMetaBool for ConstBool<false> {
        type ToMetaBool = False;
    }

    impl ToMetaBool for ConstBool<true> {
        type ToMetaBool = True;
    }

    #[cfg(feature = "typenum")]
    impl ToMetaBool for typenum::B0 {
        type ToMetaBool = False;
    }

    #[cfg(feature = "typenum")]
    impl ToMetaBool for typenum::B1 {
        type ToMetaBool = True;
    }
}

#[cfg(test)]
mod tests {
    use enum_trait::const_test;

    use super::*;

    #[const_test]
    const fn values() {
        assert!(!False::VALUE);
        assert!(True::VALUE);
        assert!(!<ConstToMetaBool<false>>::VALUE);
        assert!(<ConstToMetaBool<true>>::VALUE);
    }

    #[const_test]
    const fn if_else() {
        assert!(<<False as MetaBool>::IfElse<'static, i8, u8>>::MAX == 255);
        assert!(<<True as MetaBool>::IfElse<'static, i8, u8>>::MAX == 127);
    }

    #[cfg(feature = "typenum")]
    mod typenum_tests {
        use super::*;

        typenum::assert_type_eq!(<False as MetaBool>::ToTypeNum, typenum::B0);
        typenum::assert_type_eq!(<True as MetaBool>::ToTypeNum, typenum::B1);

        typenum::assert_type_eq!(ToMetaBool<typenum::B0>, False);
        typenum::assert_type_eq!(ToMetaBool<typenum::B1>, True);
    }

    #[const_test]
    const fn op_not() {
        assert!(<Not<False>>::VALUE);
        assert!(!<Not<True>>::VALUE);
        assert!(<Not<Not<True>>>::VALUE);
        assert!(<Not<ConstToMetaBool<{ !true }>>>::VALUE);
    }

    #[const_test]
    const fn op_and() {
        assert!(!<And<False, False>>::VALUE);
        assert!(!<And<False, True>>::VALUE);
        assert!(!<And<True, False>>::VALUE);
        assert!(<And<True, True>>::VALUE);
    }

    #[const_test]
    const fn op_or() {
        assert!(!<Or<False, False>>::VALUE);
        assert!(<Or<False, True>>::VALUE);
        assert!(<Or<True, False>>::VALUE);
        assert!(<Or<True, True>>::VALUE);
    }

    #[const_test]
    const fn op_xor() {
        assert!(!<XOr<False, False>>::VALUE);
        assert!(<XOr<False, True>>::VALUE);
        assert!(<XOr<True, False>>::VALUE);
        assert!(!<XOr<True, True>>::VALUE);
    }

    #[const_test]
    const fn op_equal() {
        assert!(<Equal<False, False>>::VALUE);
        assert!(!<Equal<False, True>>::VALUE);
        assert!(!<Equal<True, False>>::VALUE);
        assert!(<Equal<True, True>>::VALUE);
    }
}
