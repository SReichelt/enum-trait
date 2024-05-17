use enum_trait::meta;

use crate::meta_bool::*;

meta! {
    /// A trait that represents a meta-level unsigned integer. A type param constrained to `MetaNum`
    /// is roughly equivalent to a const param of an unsigned integer type, and can be converted to
    /// a constant via `VALUE`.
    ///
    /// Due to the simple unary representation, the range of numbers is limited by the compiler
    /// recursion depth. This is not a problem for the main use case of `MetaNum`, which is to
    /// represent a type-level length (in particular, of a `TypeList`). In particular, `MetaNum` is
    /// not intended as a replacement for the `typenum` crate. (Conversion to and from
    /// `typenum::Unsigned` is implemented, however.)
    pub enum trait MetaNum {
        Zero,
        Succ<N: MetaNum>,
    }

    trait impl MetaNum {
        pub const VALUE: usize = match <Self> {
            Zero => 0,
            Succ<N: MetaNum> => N::VALUE + 1,
        };

        pub type IsZero: MetaBool = match <Self> {
            Zero => True,
            Succ<N: MetaNum> => False,
        };

        pub type IsEven: MetaBool = match <Self> {
            Zero => True,
            Succ<N: MetaNum> => N::IsOdd,
        };

        pub type IsOdd: MetaBool = match <Self> {
            Zero => False,
            Succ<N: MetaNum> => N::IsEven,
        };

        /// Converts this `MetaNum` instance to the corresponding type of the `typenum` crate.
        #[cfg(feature = "typenum")]
        pub type ToTypeNum: typenum::Unsigned = match <Self> {
            Zero => typenum::UTerm,
            Succ<N: MetaNum> => typenum::UInt<<Div2Ceil<N> as MetaNum>::ToTypeNum,
                                              <N::IsEven as MetaBool>::ToTypeNum>,
        };
    }

    pub type Add<M: MetaNum, N: MetaNum>: MetaNum = match <N> {
        Zero => M,
        Succ<P: MetaNum> => Succ<Add<M, P>>,
    };

    pub type Mul<M: MetaNum, N: MetaNum>: MetaNum = match <N> {
        Zero => Zero,
        Succ<P: MetaNum> => Add<M, Mul<M, P>>,
    };

    pub type Pow<M: MetaNum, N: MetaNum>: MetaNum = match <N> {
        Zero => Succ<Zero>,
        Succ<P: MetaNum> => Mul<M, Pow<M, P>>,
    };

    pub type Equal<M: MetaNum, N: MetaNum>: MetaBool = match <M> {
        Zero => N::IsZero,
        Succ<O: MetaNum> => match <N> {
            Zero => False,
            Succ<P: MetaNum> => Equal<O, P>,
        }
    };

    pub type LessThan<M: MetaNum, N: MetaNum>: MetaBool = match <N> {
        Zero => False,
        Succ<P: MetaNum> => LessOrEqual<M, P>,
    };

    pub type LessOrEqual<M: MetaNum, N: MetaNum>: MetaBool = match <M> {
        Zero => True,
        Succ<O: MetaNum> => LessThan<O, N>,
    };

    pub type GreaterThan<M: MetaNum, N: MetaNum>: MetaBool = LessThan<N, M>;

    pub type GreaterOrEqual<M: MetaNum, N: MetaNum>: MetaBool = LessOrEqual<N, M>;

    pub type Mul2<N: MetaNum>: MetaNum = match <N> {
        Zero => Zero,
        Succ<P: MetaNum> => Succ<Succ<Mul2<P>>>,
    };

    pub type Div2<N: MetaNum>: MetaNum = match <N> {
        Zero => Zero,
        Succ<P: MetaNum> => Div2Ceil<P>,
    };

    pub type Div2Ceil<N: MetaNum>: MetaNum = match <N> {
        Zero => Zero,
        Succ<P: MetaNum> => Succ<Div2<P>>,
    };

    /*pub trait MetaNumLessThan<N: MetaNum> = MetaNum[|M| LessThan<M, N>::VALUE];

    pub trait MetaNumLessOrEqual<N: MetaNum>: MetaNum[|M| LessOrEqual<M, N>::VALUE];

    pub type Sub<M: MetaNum, N: MetaNumLessOrEqual<M>>: MetaNumLessOrEqual<M> = match <N> {
        Zero => M,
        Succ<P: MetaNum> => match <M> {
            Zero => unreachable!(),
            Succ<O: MetaNum> => Sub<O, P>,
        }
    };*/
}

#[cfg(feature = "typenum")]
pub type ToMetaNum<N> = <N as internal::ToMetaNum>::ToMetaNum;

pub mod internal {
    use super::*;

    pub trait ToMetaNum {
        type ToMetaNum: MetaNum;
    }

    #[cfg(feature = "typenum")]
    impl ToMetaNum for typenum::UTerm {
        type ToMetaNum = Zero;
    }

    #[cfg(feature = "typenum")]
    impl<U: typenum::Unsigned + ToMetaNum> ToMetaNum for typenum::UInt<U, typenum::B0> {
        type ToMetaNum = Mul2<<U as ToMetaNum>::ToMetaNum>;
    }

    #[cfg(feature = "typenum")]
    impl<U: typenum::Unsigned + ToMetaNum> ToMetaNum for typenum::UInt<U, typenum::B1> {
        type ToMetaNum = Succ<Mul2<<U as ToMetaNum>::ToMetaNum>>;
    }
}

#[cfg(test)]
mod tests {
    use enum_trait::const_test;

    use super::*;

    #[const_test]
    const fn values() {
        // Note: Currently `assert_eq!` cannot be used in const fns.
        assert!(Zero::VALUE == 0);
        assert!(<Succ<Zero>>::VALUE == 1);
        assert!(<Succ<Succ<Zero>>>::VALUE == 2);
    }

    #[cfg(feature = "typenum")]
    mod typenum_tests {
        use super::*;

        typenum::assert_type_eq!(<Zero as MetaNum>::ToTypeNum, typenum::U0);
        typenum::assert_type_eq!(<Succ<Zero> as MetaNum>::ToTypeNum, typenum::U1);
        typenum::assert_type_eq!(<Succ<Succ<Zero>> as MetaNum>::ToTypeNum, typenum::U2);
        typenum::assert_type_eq!(<Succ<Succ<Succ<Zero>>> as MetaNum>::ToTypeNum, typenum::U3);
        typenum::assert_type_eq!(
            <Succ<Succ<Succ<Succ<Zero>>>> as MetaNum>::ToTypeNum,
            typenum::U4
        );
        typenum::assert_type_eq!(
            <Succ<Succ<Succ<Succ<Succ<Zero>>>>> as MetaNum>::ToTypeNum,
            typenum::U5
        );

        typenum::assert_type_eq!(ToMetaNum<typenum::U0>, Zero);
        typenum::assert_type_eq!(ToMetaNum<typenum::U1>, Succ<Zero>);
        typenum::assert_type_eq!(ToMetaNum<typenum::U2>, Succ<Succ<Zero>>);
        typenum::assert_type_eq!(ToMetaNum<typenum::U3>, Succ<Succ<Succ<Zero>>>);
        typenum::assert_type_eq!(ToMetaNum<typenum::U4>, Succ<Succ<Succ<Succ<Zero>>>>);
    }

    #[const_test]
    const fn even() {
        assert!(<Zero as MetaNum>::IsEven::VALUE);
        assert!(!<Succ<Zero> as MetaNum>::IsEven::VALUE);
        assert!(<Succ<Succ<Zero>> as MetaNum>::IsEven::VALUE);
        assert!(!<Succ<Succ<Succ<Zero>>> as MetaNum>::IsEven::VALUE);
    }

    #[const_test]
    const fn odd() {
        assert!(!<Zero as MetaNum>::IsOdd::VALUE);
        assert!(<Succ<Zero> as MetaNum>::IsOdd::VALUE);
        assert!(!<Succ<Succ<Zero>> as MetaNum>::IsOdd::VALUE);
        assert!(<Succ<Succ<Succ<Zero>>> as MetaNum>::IsOdd::VALUE);
    }

    #[const_test]
    const fn op_add() {
        assert!(<Add<Zero, Zero>>::VALUE == 0);
        assert!(<Add<Zero, Succ<Zero>>>::VALUE == 1);
        assert!(<Add<Succ<Zero>, Zero>>::VALUE == 1);
        assert!(<Add<Succ<Zero>, Succ<Zero>>>::VALUE == 2);
        assert!(<Add<Succ<Succ<Succ<Zero>>>, Succ<Succ<Zero>>>>::VALUE == 5);
    }

    #[const_test]
    const fn op_mul() {
        assert!(<Mul<Zero, Zero>>::VALUE == 0);
        assert!(<Mul<Zero, Succ<Zero>>>::VALUE == 0);
        assert!(<Mul<Succ<Zero>, Zero>>::VALUE == 0);
        assert!(<Mul<Succ<Zero>, Succ<Zero>>>::VALUE == 1);
        assert!(<Mul<Succ<Zero>, Succ<Succ<Zero>>>>::VALUE == 2);
        assert!(<Mul<Succ<Succ<Zero>>, Succ<Zero>>>::VALUE == 2);
        assert!(<Mul<Succ<Succ<Zero>>, Succ<Succ<Zero>>>>::VALUE == 4);
        assert!(<Mul<Succ<Succ<Succ<Zero>>>, Succ<Succ<Zero>>>>::VALUE == 6);
    }

    #[const_test]
    const fn op_pow() {
        assert!(<Pow<Zero, Zero>>::VALUE == 1);
        assert!(<Pow<Zero, Succ<Zero>>>::VALUE == 0);
        assert!(<Pow<Succ<Zero>, Zero>>::VALUE == 1);
        assert!(<Pow<Succ<Zero>, Succ<Zero>>>::VALUE == 1);
        assert!(<Pow<Succ<Zero>, Succ<Succ<Zero>>>>::VALUE == 1);
        assert!(<Pow<Succ<Succ<Zero>>, Succ<Zero>>>::VALUE == 2);
        assert!(<Pow<Succ<Succ<Zero>>, Succ<Succ<Zero>>>>::VALUE == 4);
        assert!(<Pow<Succ<Succ<Succ<Zero>>>, Succ<Succ<Zero>>>>::VALUE == 9);
    }

    #[const_test]
    const fn op_equal() {
        assert!(<Equal<Zero, Zero>>::VALUE);
        assert!(!<Equal<Zero, Succ<Zero>>>::VALUE);
        assert!(!<Equal<Zero, Succ<Succ<Zero>>>>::VALUE);
        assert!(!<Equal<Succ<Zero>, Zero>>::VALUE);
        assert!(<Equal<Succ<Zero>, Succ<Zero>>>::VALUE);
        assert!(!<Equal<Succ<Zero>, Succ<Succ<Zero>>>>::VALUE);
        assert!(!<Equal<Succ<Zero>, Succ<Succ<Succ<Zero>>>>>::VALUE);
        assert!(!<Equal<Succ<Succ<Zero>>, Zero>>::VALUE);
        assert!(!<Equal<Succ<Succ<Zero>>, Succ<Zero>>>::VALUE);
        assert!(<Equal<Succ<Succ<Zero>>, Succ<Succ<Zero>>>>::VALUE);
        assert!(!<Equal<Succ<Succ<Zero>>, Succ<Succ<Succ<Zero>>>>>::VALUE);
    }

    #[const_test]
    const fn op_less_than() {
        assert!(!<LessThan<Zero, Zero>>::VALUE);
        assert!(<LessThan<Zero, Succ<Zero>>>::VALUE);
        assert!(<LessThan<Zero, Succ<Succ<Zero>>>>::VALUE);
        assert!(!<LessThan<Succ<Zero>, Zero>>::VALUE);
        assert!(!<LessThan<Succ<Zero>, Succ<Zero>>>::VALUE);
        assert!(<LessThan<Succ<Zero>, Succ<Succ<Zero>>>>::VALUE);
        assert!(<LessThan<Succ<Zero>, Succ<Succ<Succ<Zero>>>>>::VALUE);
        assert!(!<LessThan<Succ<Succ<Zero>>, Zero>>::VALUE);
        assert!(!<LessThan<Succ<Succ<Zero>>, Succ<Zero>>>::VALUE);
        assert!(!<LessThan<Succ<Succ<Zero>>, Succ<Succ<Zero>>>>::VALUE);
        assert!(<LessThan<Succ<Succ<Zero>>, Succ<Succ<Succ<Zero>>>>>::VALUE);
    }

    #[const_test]
    const fn op_less_or_equal() {
        assert!(<LessOrEqual<Zero, Zero>>::VALUE);
        assert!(<LessOrEqual<Zero, Succ<Zero>>>::VALUE);
        assert!(<LessOrEqual<Zero, Succ<Succ<Zero>>>>::VALUE);
        assert!(!<LessOrEqual<Succ<Zero>, Zero>>::VALUE);
        assert!(<LessOrEqual<Succ<Zero>, Succ<Zero>>>::VALUE);
        assert!(<LessOrEqual<Succ<Zero>, Succ<Succ<Zero>>>>::VALUE);
        assert!(<LessOrEqual<Succ<Zero>, Succ<Succ<Succ<Zero>>>>>::VALUE);
        assert!(!<LessOrEqual<Succ<Succ<Zero>>, Zero>>::VALUE);
        assert!(!<LessOrEqual<Succ<Succ<Zero>>, Succ<Zero>>>::VALUE);
        assert!(<LessOrEqual<Succ<Succ<Zero>>, Succ<Succ<Zero>>>>::VALUE);
        assert!(<LessOrEqual<Succ<Succ<Zero>>, Succ<Succ<Succ<Zero>>>>>::VALUE);
    }

    #[const_test]
    const fn op_greater_than() {
        assert!(!<GreaterThan<Zero, Zero>>::VALUE);
        assert!(!<GreaterThan<Zero, Succ<Zero>>>::VALUE);
        assert!(<GreaterThan<Succ<Zero>, Zero>>::VALUE);
        assert!(!<GreaterThan<Succ<Zero>, Succ<Zero>>>::VALUE);
    }

    #[const_test]
    const fn op_greater_or_equal() {
        assert!(<GreaterOrEqual<Zero, Zero>>::VALUE);
        assert!(!<GreaterOrEqual<Zero, Succ<Zero>>>::VALUE);
        assert!(<GreaterOrEqual<Succ<Zero>, Zero>>::VALUE);
        assert!(<GreaterOrEqual<Succ<Zero>, Succ<Zero>>>::VALUE);
    }

    #[const_test]
    const fn op_mul2() {
        assert!(<Mul2<Zero>>::VALUE == 0);
        assert!(<Mul2<Succ<Zero>>>::VALUE == 2);
        assert!(<Mul2<Succ<Succ<Zero>>>>::VALUE == 4);
        assert!(<Mul2<Succ<Succ<Succ<Zero>>>>>::VALUE == 6);
    }

    #[const_test]
    const fn op_div2() {
        assert!(<Div2<Zero>>::VALUE == 0);
        assert!(<Div2<Succ<Zero>>>::VALUE == 0);
        assert!(<Div2<Succ<Succ<Zero>>>>::VALUE == 1);
        assert!(<Div2<Succ<Succ<Succ<Zero>>>>>::VALUE == 1);
        assert!(<Div2<Succ<Succ<Succ<Succ<Zero>>>>>>::VALUE == 2);
        assert!(<Div2<Succ<Succ<Succ<Succ<Succ<Zero>>>>>>>::VALUE == 2);
        assert!(<Div2<Succ<Succ<Succ<Succ<Succ<Succ<Zero>>>>>>>>::VALUE == 3);
    }
}
