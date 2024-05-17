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

#[macro_export]
macro_rules! meta_num {
    ($n:literal) => (
        enum_trait::iterate!($n, $crate::meta_num::Zero, |<N: MetaNum>| $crate::meta_num::Succ<N>)
    );
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
        assert!(<meta_num!(0)>::VALUE == 0);
        assert!(<meta_num!(1)>::VALUE == 1);
        assert!(<meta_num!(2)>::VALUE == 2);
    }

    #[cfg(feature = "typenum")]
    mod typenum_tests {
        use super::*;

        typenum::assert_type_eq!(<meta_num!(0) as MetaNum>::ToTypeNum, typenum::U0);
        typenum::assert_type_eq!(<meta_num!(1) as MetaNum>::ToTypeNum, typenum::U1);
        typenum::assert_type_eq!(<meta_num!(2) as MetaNum>::ToTypeNum, typenum::U2);
        typenum::assert_type_eq!(<meta_num!(3) as MetaNum>::ToTypeNum, typenum::U3);
        typenum::assert_type_eq!(<meta_num!(4) as MetaNum>::ToTypeNum, typenum::U4);
        typenum::assert_type_eq!(<meta_num!(5) as MetaNum>::ToTypeNum, typenum::U5);

        typenum::assert_type_eq!(ToMetaNum<typenum::U0>, meta_num!(0));
        typenum::assert_type_eq!(ToMetaNum<typenum::U1>, meta_num!(1));
        typenum::assert_type_eq!(ToMetaNum<typenum::U2>, meta_num!(2));
        typenum::assert_type_eq!(ToMetaNum<typenum::U3>, meta_num!(3));
        typenum::assert_type_eq!(ToMetaNum<typenum::U4>, meta_num!(4));
    }

    #[const_test]
    const fn even() {
        assert!(<meta_num!(0) as MetaNum>::IsEven::VALUE);
        assert!(!<meta_num!(1) as MetaNum>::IsEven::VALUE);
        assert!(<meta_num!(2) as MetaNum>::IsEven::VALUE);
        assert!(!<meta_num!(3) as MetaNum>::IsEven::VALUE);
    }

    #[const_test]
    const fn odd() {
        assert!(!<meta_num!(0) as MetaNum>::IsOdd::VALUE);
        assert!(<meta_num!(1) as MetaNum>::IsOdd::VALUE);
        assert!(!<meta_num!(2) as MetaNum>::IsOdd::VALUE);
        assert!(<meta_num!(3) as MetaNum>::IsOdd::VALUE);
    }

    #[const_test]
    const fn op_add() {
        assert!(<Add<meta_num!(0), meta_num!(0)>>::VALUE == 0);
        assert!(<Add<meta_num!(0), meta_num!(1)>>::VALUE == 1);
        assert!(<Add<meta_num!(1), meta_num!(0)>>::VALUE == 1);
        assert!(<Add<meta_num!(1), meta_num!(1)>>::VALUE == 2);
        assert!(<Add<meta_num!(3), meta_num!(2)>>::VALUE == 5);
    }

    #[const_test]
    const fn op_mul() {
        assert!(<Mul<meta_num!(0), meta_num!(0)>>::VALUE == 0);
        assert!(<Mul<meta_num!(0), meta_num!(1)>>::VALUE == 0);
        assert!(<Mul<meta_num!(1), meta_num!(0)>>::VALUE == 0);
        assert!(<Mul<meta_num!(1), meta_num!(1)>>::VALUE == 1);
        assert!(<Mul<meta_num!(1), meta_num!(2)>>::VALUE == 2);
        assert!(<Mul<meta_num!(2), meta_num!(1)>>::VALUE == 2);
        assert!(<Mul<meta_num!(2), meta_num!(2)>>::VALUE == 4);
        assert!(<Mul<meta_num!(3), meta_num!(2)>>::VALUE == 6);
    }

    #[const_test]
    const fn op_pow() {
        assert!(<Pow<meta_num!(0), meta_num!(0)>>::VALUE == 1);
        assert!(<Pow<meta_num!(0), meta_num!(1)>>::VALUE == 0);
        assert!(<Pow<meta_num!(1), meta_num!(0)>>::VALUE == 1);
        assert!(<Pow<meta_num!(1), meta_num!(1)>>::VALUE == 1);
        assert!(<Pow<meta_num!(1), meta_num!(2)>>::VALUE == 1);
        assert!(<Pow<meta_num!(2), meta_num!(1)>>::VALUE == 2);
        assert!(<Pow<meta_num!(2), meta_num!(2)>>::VALUE == 4);
        assert!(<Pow<meta_num!(3), meta_num!(2)>>::VALUE == 9);
    }

    #[const_test]
    const fn op_equal() {
        assert!(<Equal<meta_num!(0), meta_num!(0)>>::VALUE);
        assert!(!<Equal<meta_num!(0), meta_num!(1)>>::VALUE);
        assert!(!<Equal<meta_num!(0), meta_num!(2)>>::VALUE);
        assert!(!<Equal<meta_num!(1), meta_num!(0)>>::VALUE);
        assert!(<Equal<meta_num!(1), meta_num!(1)>>::VALUE);
        assert!(!<Equal<meta_num!(1), meta_num!(2)>>::VALUE);
        assert!(!<Equal<meta_num!(1), meta_num!(3)>>::VALUE);
        assert!(!<Equal<meta_num!(2), meta_num!(0)>>::VALUE);
        assert!(!<Equal<meta_num!(2), meta_num!(1)>>::VALUE);
        assert!(<Equal<meta_num!(2), meta_num!(2)>>::VALUE);
        assert!(!<Equal<meta_num!(2), meta_num!(3)>>::VALUE);
    }

    #[const_test]
    const fn op_less_than() {
        assert!(!<LessThan<meta_num!(0), meta_num!(0)>>::VALUE);
        assert!(<LessThan<meta_num!(0), meta_num!(1)>>::VALUE);
        assert!(<LessThan<meta_num!(0), meta_num!(2)>>::VALUE);
        assert!(!<LessThan<meta_num!(1), meta_num!(0)>>::VALUE);
        assert!(!<LessThan<meta_num!(1), meta_num!(1)>>::VALUE);
        assert!(<LessThan<meta_num!(1), meta_num!(2)>>::VALUE);
        assert!(<LessThan<meta_num!(1), meta_num!(3)>>::VALUE);
        assert!(!<LessThan<meta_num!(2), meta_num!(0)>>::VALUE);
        assert!(!<LessThan<meta_num!(2), meta_num!(1)>>::VALUE);
        assert!(!<LessThan<meta_num!(2), meta_num!(2)>>::VALUE);
        assert!(<LessThan<meta_num!(2), meta_num!(3)>>::VALUE);
    }

    #[const_test]
    const fn op_less_or_equal() {
        assert!(<LessOrEqual<meta_num!(0), meta_num!(0)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(0), meta_num!(1)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(0), meta_num!(2)>>::VALUE);
        assert!(!<LessOrEqual<meta_num!(1), meta_num!(0)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(1), meta_num!(1)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(1), meta_num!(2)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(1), meta_num!(3)>>::VALUE);
        assert!(!<LessOrEqual<meta_num!(2), meta_num!(0)>>::VALUE);
        assert!(!<LessOrEqual<meta_num!(2), meta_num!(1)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(2), meta_num!(2)>>::VALUE);
        assert!(<LessOrEqual<meta_num!(2), meta_num!(3)>>::VALUE);
    }

    #[const_test]
    const fn op_greater_than() {
        assert!(!<GreaterThan<meta_num!(0), meta_num!(0)>>::VALUE);
        assert!(!<GreaterThan<meta_num!(0), meta_num!(1)>>::VALUE);
        assert!(<GreaterThan<meta_num!(1), meta_num!(0)>>::VALUE);
        assert!(!<GreaterThan<meta_num!(1), meta_num!(1)>>::VALUE);
    }

    #[const_test]
    const fn op_greater_or_equal() {
        assert!(<GreaterOrEqual<meta_num!(0), meta_num!(0)>>::VALUE);
        assert!(!<GreaterOrEqual<meta_num!(0), meta_num!(1)>>::VALUE);
        assert!(<GreaterOrEqual<meta_num!(1), meta_num!(0)>>::VALUE);
        assert!(<GreaterOrEqual<meta_num!(1), meta_num!(1)>>::VALUE);
    }

    #[const_test]
    const fn op_mul2() {
        assert!(<Mul2<meta_num!(0)>>::VALUE == 0);
        assert!(<Mul2<meta_num!(1)>>::VALUE == 2);
        assert!(<Mul2<meta_num!(2)>>::VALUE == 4);
        assert!(<Mul2<meta_num!(3)>>::VALUE == 6);
    }

    #[const_test]
    const fn op_div2() {
        assert!(<Div2<meta_num!(0)>>::VALUE == 0);
        assert!(<Div2<meta_num!(1)>>::VALUE == 0);
        assert!(<Div2<meta_num!(2)>>::VALUE == 1);
        assert!(<Div2<meta_num!(3)>>::VALUE == 1);
        assert!(<Div2<meta_num!(4)>>::VALUE == 2);
        assert!(<Div2<meta_num!(5)>>::VALUE == 2);
        assert!(<Div2<meta_num!(6)>>::VALUE == 3);
    }
}
