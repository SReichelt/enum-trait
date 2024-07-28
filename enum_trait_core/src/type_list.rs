use enum_trait::meta;

use crate::{meta_bool::*, meta_num::*, optional_type::*};

meta! {
    pub enum trait TypeList<trait ItemBound: ?Sized> {
        Empty,
        NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>,
    }

    trait impl<trait ItemBound: ?Sized> TypeList<ItemBound> {
        pub type IsEmpty: MetaBool = match <Self> {
            Empty => True,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> => False,
        };

        pub type Len: MetaNum = match <Self> {
            Empty => Zero,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> => Succ<Tail::Len>,
        };

        pub type Get<I: ValidIndex<ItemBound, Self>>: ItemBound = match <Self, I> {
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Zero => Head,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Succ<P: ValidIndex<ItemBound, Tail>> =>
                <Tail as TypeList<ItemBound>>::Get<P>,
        };

        pub type GetOpt<I: ExtendedIndex<ItemBound, Self>>: OptionalType<ItemBound> = match <Self, I> {
            Empty, Zero => NoType,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Zero => SomeType<Head>,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Succ<P: ExtendedIndex<ItemBound, Tail>> =>
                <Tail as TypeList<ItemBound>>::GetOpt<P>,
        };

        pub type Append<T: ItemBound>: TypeList<ItemBound> = match <Self> {
            Empty => NonEmpty<T, Empty>,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> =>
                NonEmpty<Head, <Tail as TypeList<ItemBound>>::Append<T>>,
        };

        pub type AppendAll<List: TypeList<ItemBound>>: TypeList<ItemBound> = match <Self> {
            Empty => List,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> =>
                NonEmpty<Head, <Tail as TypeList<ItemBound>>::AppendAll<List>>,
        };

        pub type Reverse: TypeList<ItemBound> = match <Self> {
            Empty => Empty,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> =>
                <<Tail as TypeList<ItemBound>>::Reverse as TypeList<ItemBound>>::Append<Head>,
        };

        // TODO: These require a lifetime parameter `'a` at `TypeList`, with `ItemBound: 'a` and
        //       'a: 'b`.
        /*
        pub type MapToRefs<'b>: SizedTypeList = match <Self> {
            Empty => Empty,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> =>
                NonEmpty<&'b Head, <Tail as TypeList<ItemBound>>::MapToRefs<'b>>,
        };

        pub type MapToMutRefs<'b>: SizedTypeList = match <Self> {
            Empty => Empty,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> =>
                NonEmpty<&'b mut Head, <Tail as TypeList<ItemBound>>::MapToMutRefs<'b>>,
        };
        */
    }

    pub trait ValidIndex<trait ItemBound: ?Sized, List: TypeList<ItemBound>> =
        MetaNumLessThan<List::Len>;

    pub trait ExtendedIndex<trait ItemBound: ?Sized, List: TypeList<ItemBound>> =
        MetaNumLessOrEqual<List::Len>;

    pub trait SizedTypeList<trait ItemBound: Sized> = TypeList<
        ItemBound,
        trait ValidIndex = SizedValidIndex,
        trait ExtendedIndex = SizedExtendedIndex,
        trait OptionalType = SizedOptionalType,
    >;

    pub trait SizedValidIndex<
        trait ItemBound: Sized,
        List: SizedTypeList<ItemBound>,
    > = ValidIndex<
        ItemBound,
        List,
        trait TypeList = SizedTypeList,
    >;

    pub trait SizedExtendedIndex<
        trait ItemBound: Sized,
        List: SizedTypeList<ItemBound>,
    > = ExtendedIndex<
        ItemBound,
        List,
        trait TypeList = SizedTypeList,
        trait OptionalType = SizedOptionalType,
    >;

    pub type NestedTupleWith<trait ItemBound: Sized, List: SizedTypeList<ItemBound>, T: Sized>: Sized =
        match <List> {
            Empty => T,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>> =>
                (Head, NestedTupleWith<ItemBound, Tail, T>),
        };

    pub type NestedTuple<trait ItemBound: Sized, List: SizedTypeList<ItemBound>>: Sized =
        NestedTupleWith<ItemBound, List, ()>;

    pub fn nested_tuple_item<
        trait ItemBound: Sized,
        List: SizedTypeList<ItemBound>,
        T: Sized,
        I: SizedExtendedIndex<ItemBound, List>
    >(
        tuple: &NestedTupleWith<ItemBound, List, T>,
    ) -> &<<List as SizedTypeList<ItemBound>>::GetOpt<I> as SizedOptionalType<ItemBound>>::UnwrapOr<T> {
        match <List, I> {
            Empty, Zero => tuple,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>>, Zero => &tuple.0,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>>, Succ<P: SizedExtendedIndex<ItemBound, Tail>> =>
                nested_tuple_item::<ItemBound, Tail, T, P>(&tuple.1),
        }
    }

    pub fn nested_tuple_item_mut<
        trait ItemBound: Sized,
        List: SizedTypeList<ItemBound>,
        T: Sized,
        I: SizedExtendedIndex<ItemBound, List>
    >(
        tuple: &mut NestedTupleWith<ItemBound, List, T>,
    ) -> &mut <<List as SizedTypeList<ItemBound>>::GetOpt<I> as SizedOptionalType<ItemBound>>::UnwrapOr<T> {
        match <List, I> {
            Empty, Zero => tuple,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>>, Zero => &mut tuple.0,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>>, Succ<P: SizedExtendedIndex<ItemBound, Tail>> =>
                nested_tuple_item_mut::<ItemBound, Tail, T, P>(&mut tuple.1),
        }
    }
}

#[macro_export]
macro_rules! type_list {
    [] => ($crate::type_list::Empty);
    [..$List:ty] => ($List);
    [$Head:ty $(, $($Tail:tt)*)?] => (
        $crate::type_list::NonEmpty<$Head, $crate::type_list![$($($Tail)*)?]>
    );
    [$Item:ty; $n:literal] => (
        enum_trait::iterate!(
            $n,
            $crate::type_list::Empty,
            |<List: TypeList>| $crate::type_list::NonEmpty<$Item, List>,
        )
    );
}

pub use type_list;

#[cfg(test)]
mod tests {
    use enum_trait::const_test;

    use super::*;

    type EmptyTypeList = type_list![];
    type TwoItemTypeList = type_list![&'static str, u8];
    type ThreeItemTypeList = type_list![bool; 3];
    type FiveItemTypeList = <TwoItemTypeList as TypeList>::AppendAll<ThreeItemTypeList>;

    #[const_test]
    const fn properties() {
        // Note: Currently `assert_eq!` cannot be used in const fns.
        assert!(<EmptyTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<EmptyTypeList as TypeList>::Len::VALUE == 0);
        assert!(!<TwoItemTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<TwoItemTypeList as TypeList>::Len::VALUE == 2);
        assert!(!<ThreeItemTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<ThreeItemTypeList as TypeList>::Len::VALUE == 3);
        assert!(!<FiveItemTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<FiveItemTypeList as TypeList>::Len::VALUE == 5);
    }

    #[const_test]
    const fn getters() {
        let _: <TwoItemTypeList as TypeList>::Get<meta_num!(0)> = "test";
        let _: <TwoItemTypeList as TypeList>::Get<meta_num!(1)> = 42;

        let _: <<TwoItemTypeList as TypeList>::GetOpt<meta_num!(0)> as OptionalType>::UnwrapOr<()> =
            "test";
        let _: <<TwoItemTypeList as TypeList>::GetOpt<meta_num!(1)> as OptionalType>::UnwrapOr<()> =
            42;
        let _: <<TwoItemTypeList as TypeList>::GetOpt<meta_num!(2)> as OptionalType>::UnwrapOr<()> =
            ();

        let _: <<TwoItemTypeList as TypeList>::Reverse as TypeList>::Get<meta_num!(0)> = 42;
        let _: <<TwoItemTypeList as TypeList>::Reverse as TypeList>::Get<meta_num!(1)> = "test";
    }

    #[test]
    fn nested_tuples() {
        assert_eq!(
            nested_tuple_item::<type_list![], (), meta_num!(0)>(&()),
            &()
        );
        assert_eq!(
            nested_tuple_item::<type_list![], i32, meta_num!(0)>(&42),
            &42
        );
        assert_eq!(
            nested_tuple_item::<type_list![i32], (), meta_num!(0)>(&(42, ())),
            &42
        );
        assert_eq!(
            nested_tuple_item::<type_list![i32], (), meta_num!(1)>(&(42, ())),
            &()
        );
        assert_eq!(
            nested_tuple_item::<type_list![i32, &str], bool, meta_num!(0)>(&(42, ("test", true))),
            &42
        );
        assert_eq!(
            nested_tuple_item::<type_list![i32, &str], bool, meta_num!(1)>(&(42, ("test", true))),
            &"test"
        );
        assert_eq!(
            nested_tuple_item::<type_list![i32, &str], bool, meta_num!(2)>(&(42, ("test", true))),
            &true
        );
    }
}
