use std::marker::PhantomData;

use enum_trait::meta;

meta! {
    pub enum trait EitherOr {
        Fst<A, B>,
        Snd<C: EitherOr>,
    }

    trait impl EitherOr {
        pub type Test0: EitherOr = Self;

        pub type Test1: EitherOr = match <Self> {
            Fst<A, B> => Self,
            Snd<C: EitherOr> => C,
        };

        pub type Test2<A: EitherOr>: EitherOr = match <A> {
            Fst<B, C> => match <Self> {
                Fst<C, D> => A,
                Snd<B: EitherOr> => B,
            }
            Snd<B: EitherOr> => A,
        };

        pub type Test3 = match <Self> {
            Fst<B, C> => match <Self> {
                Fst<C, B> => C,
                Snd<D: EitherOr> => C,
            }
            Snd<B: EitherOr> => Self,
        };

        pub const TEST0: bool = true;

        pub const TEST1: bool = match <Self> {
            Fst<A, B> => false,
            Snd<C: EitherOr> => C::TEST0,
        };

        pub const TEST2: bool = match <Self> {
            Fst<B, C> => match <Self> {
                Fst<C, D> => Self::TEST1,
                Snd<B: EitherOr> => B::TEST1,
            }
            Snd<B: EitherOr> => Self::TEST1,
        };
    }
}

macro_rules! assert_type_eq {
    ($X:ty, $Y:ty $(,)?) => {
        const _: PhantomData<$X> = PhantomData::<$Y>;
    };
}

macro_rules! assert_const_eq {
    ($x:expr, $y:expr $(,)?) => {
        const _: () = assert!($x == $y);
    };
}

assert_type_eq!(<Fst<u8, u16> as EitherOr>::Test0, Fst<u8, u16>);
assert_type_eq!(<Snd<Fst<u8, u16>> as EitherOr>::Test0, Snd<Fst<u8, u16>>);

assert_type_eq!(<Fst<u8, u16> as EitherOr>::Test1, Fst<u8, u16>);
assert_type_eq!(<Snd<Fst<u8, u16>> as EitherOr>::Test1, Fst<u8, u16>);

assert_type_eq!(<Fst<u8, u16> as EitherOr>::Test2<Fst<u32, u64>>, Fst<u32, u64>);
assert_type_eq!(<Snd<Fst<u8, u16>> as EitherOr>::Test2<Fst<u32, u64>>, Fst<u8, u16>);
assert_type_eq!(
    <Snd<Fst<u8, u16>> as EitherOr>::Test2<Snd<Fst<u32, u64>>>,
    Snd<Fst<u32, u64>>,
);

assert_type_eq!(<Fst<u8, u16> as EitherOr>::Test3, u8);
assert_type_eq!(<Snd<Fst<u8, u16>> as EitherOr>::Test3, Snd<Fst<u8, u16>>);

assert_const_eq!(<Fst<u8, u16> as EitherOr>::TEST0, true);
assert_const_eq!(<Snd<Fst<u8, u16>> as EitherOr>::TEST0, true);

assert_const_eq!(<Fst<u8, u16> as EitherOr>::TEST1, false);
assert_const_eq!(<Snd<Fst<u8, u16>> as EitherOr>::TEST1, true);
