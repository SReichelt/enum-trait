use enum_trait::meta;

use crate::{meta_bool::*, meta_num::*};

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

        pub type ItemAt<I: ValidIndex<ItemBound, Self>>: ItemBound = match <Self, I> {
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Zero => Head,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Succ<P: ValidIndex<ItemBound, Tail>> =>
                Tail::ItemAt<P>,
        };

        /*pub type OptionalItemAt<I: ExtendedIndex<ItemBound, Self>>: OptionalType<ItemBound> = match <Self, I> {
            Empty, Zero => NoType,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Zero => SomeType<Head>,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Succ<P: ExtendedIndex<ItemBound, Tail>> =>
                Tail::OptionalItemAt<P>,
        };*/

        /*pub type MapToRefs<'a>: SizedTypeList<Deref<Target: ItemBound> + 'a> = match <Self> {
            Empty => Empty,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> => NonEmpty<&'a Head, Tail::MapToRefs<'a>>,
        };*/
    }

    pub trait ValidIndex<trait ItemBound: ?Sized, List: TypeList<ItemBound>> =
        MetaNumLessThan<List::Len>;

    /*pub trait ExtendedIndex<trait ItemBound: ?Sized, List: TypeList<ItemBound>> =
        MetaNumLessOrEqual<List::Len>;*/

    /*
    pub trait SizedTypeList<trait ItemBound: Sized> = TypeList<ItemBound>;

    pub type NestedTupleWith<trait ItemBound: Sized, List: SizedTypeList<ItemBound>, T: Sized>: Sized =
        match <List> {
            Empty => T,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>> =>
                (Head, NestedTupleWith<ItemBound, List, T>),
        };

    pub type NestedTuple<trait ItemBound: Sized, List: SizedTypeList<ItemBound>>: Sized =
        NestedTupleWith<ItemBound, List, ()>;

    pub fn get_nested_tuple_item<
        trait ItemBound: Sized,
        List: SizedTypeList<ItemBound>,
        T: Sized,
        I: ExtendedIndex<ItemBound, List>
    >(
        tuple: &NestedTupleWith<ItemBound, List, T>,
    ) -> &<List::OptionalItemAt<I> as OptionalType<ItemBound>>::UnwrapOr<T> {
        match <List, I> {
            Empty, Zero => tuple,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Zero => &tuple.0,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>>, Succ<P: ExtendedIndex<ItemBound, Tail>> =>
                get_nested_tuple_item<ItemBound, Tail, T, P>(&tuple.1),
        }
    }
    */
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

    #[const_test]
    const fn properties() {
        // Note: Currently `assert_eq!` cannot be used in const fns.
        assert!(<EmptyTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<EmptyTypeList as TypeList>::Len::VALUE == 0);
        assert!(!<TwoItemTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<TwoItemTypeList as TypeList>::Len::VALUE == 2);
        assert!(!<ThreeItemTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<ThreeItemTypeList as TypeList>::Len::VALUE == 3);
    }

    #[const_test]
    const fn item_at() {
        let _: <TwoItemTypeList as TypeList>::ItemAt<meta_num!(0)> = "test";
        let _: <TwoItemTypeList as TypeList>::ItemAt<meta_num!(1)> = 42;
    }
}
