use core::ops::Deref;

use enum_trait::meta;

use crate::meta_bool::*;

meta! {
    pub enum trait RefKind {
        Shared,
        Mutable,
    }

    trait impl RefKind {
        pub type IsMutable: MetaBool = match <Self> {
            Shared => False,
            Mutable => True,
        };

        pub type Ref<'a, T: 'a>: Deref<Target = T> + 'a = match <Self> {
            Shared => &'a T,
            Mutable => &'a mut T,
        };

        type PairRefSplitter<'a, T1, T2>: PairRefSplitter<'a, Self, T1, T2> = match <Self> {
            Shared => (),
            Mutable => (),
        };
    }

    /*pub fn pair_ref_to_ref_pair<K: RefKind, T1, T2>(
        pair: K::Ref<'_, (T1, T2)>,
    ) -> (K::Ref<'_, T1>, K::Ref<'_, T2>) {
        match <K> {
            Shared => (&pair.0, &pair.1),
            Mutable => (&mut pair.0, &mut pair.1),
        }
    }*/
}

// TODO: Use sized type list and tuple instead.
// TODO: It would be nice to define such functions in the `meta!` block, with matching.
trait PairRefSplitter<'a, K: RefKind, T1, T2> {
    fn pair_ref_to_ref_pair(pair: K::Ref<'a, (T1, T2)>) -> (K::Ref<'a, T1>, K::Ref<'a, T2>);
}

impl<'a, T1, T2> PairRefSplitter<'a, Shared, T1, T2> for () {
    fn pair_ref_to_ref_pair(pair: &'a (T1, T2)) -> (&'a T1, &'a T2) {
        (&pair.0, &pair.1)
    }
}

impl<'a, T1, T2> PairRefSplitter<'a, Mutable, T1, T2> for () {
    fn pair_ref_to_ref_pair(pair: &'a mut (T1, T2)) -> (&'a mut T1, &'a mut T2) {
        (&mut pair.0, &mut pair.1)
    }
}

pub fn pair_ref_to_ref_pair<K: RefKind, T1, T2>(
    pair: K::Ref<'_, (T1, T2)>,
) -> (K::Ref<'_, T1>, K::Ref<'_, T2>) {
    K::PairRefSplitter::pair_ref_to_ref_pair(pair)
}
