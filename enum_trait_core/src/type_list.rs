use enum_trait::meta;

use crate::meta_bool::*;
use crate::meta_num::*;

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

        /*pub type ItemAt<I: ValidIndex<ItemBound, Self>>: ItemBound = match <Self> {
            Empty => unreachable!(),
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> => match <I> {
                Zero => Head,
                Succ<P: MetaNum> => Tail::ItemAt<P>,
            }
        };*/

        /*pub type MapToRefs<'a>: SizedTypeList<Deref<Target: ItemBound> + 'a> = match <Self> {
            Empty => Empty,
            NonEmpty<Head: ItemBound, Tail: TypeList<ItemBound>> => NonEmpty<&'a Head, Tail::MapToRefs<'a>>,
        };*/
    }

    /*pub trait ValidIndex<trait ItemBound: Sized, L: TypeList<ItemBound>> =
        MetaNumLessThan<L::Len>;

    pub trait SizedTypeList<trait ItemBound: Sized> = TypeList<ItemBound>;

    trait impl<trait ItemBound: Sized> SizedTypeList<ItemBound> {
        pub type NestedTupleWith<T: Sized>: Sized = match <Self> {
            Empty => T,
            NonEmpty<Head: ItemBound, Tail: SizedTypeList<ItemBound>> => (Head, Tail::NestedTupleWith<T>),
        };

        pub type NestedTuple: Sized = Self::NestedTupleWith<()>;

        // TODO: tuple accessor
    }*/
}

#[macro_export]
macro_rules! type_list {
    [] => ($crate::type_list::Empty);
    [..$List:ty] => ($List);
    [$Head:ty $(, $($Tail:tt)*)?] =>
        ($crate::type_list::NonEmpty<$Head, $crate::type_list![$($($Tail)*)?]>);
}

#[cfg(test)]
mod tests {
    use enum_trait::const_test;

    use super::*;

    type EmptyTypeList = type_list![];
    type TwoItemTypeList = type_list![str, u8];

    #[const_test]
    const fn properties() {
        // Note: Currently `assert_eq!` cannot be used in const fns.
        assert!(<EmptyTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<EmptyTypeList as TypeList>::Len::VALUE == 0);
        assert!(!<TwoItemTypeList as TypeList>::IsEmpty::VALUE);
        assert!(<TwoItemTypeList as TypeList>::Len::VALUE == 2);
    }
}
