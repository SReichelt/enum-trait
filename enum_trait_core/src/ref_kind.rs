use core::{ops::Deref, slice};

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

        pub type Ref<'a, T: ?Sized + 'a>: Deref<Target = T> + 'a = match <Self> {
            Shared => &'a T,
            Mutable => &'a mut T,
        };
    }

    // Note that this can also be implemented using the `IntoIterator` instance of slice references,
    // but that would not support the `K`-dependent return bound we specify here.
    pub type SliceIter<'a, K: RefKind, T: 'a>: Iterator<Item = K::Ref<'a, T>> = match <K> {
        Shared => slice::Iter<'a, T>,
        Mutable => slice::IterMut<'a, T>,
    };

    pub fn slice_iter<'a, K: RefKind, T: 'a>(slice: K::Ref<'a, [T]>) -> SliceIter<'a, K, T> {
        match <K> {
            Shared => slice.iter(),
            Mutable => slice.iter_mut(),
        }
    }

    // TODO: Use sized type list and nested tuple instead.
    pub fn pair_ref_to_ref_pair<'a, K: RefKind, T1, T2>(
        pair: K::Ref<'a, (T1, T2)>,
    ) -> (K::Ref<'a, T1>, K::Ref<'a, T2>) {
        match <K> {
            Shared => (&pair.0, &pair.1),
            Mutable => (&mut pair.0, &mut pair.1),
        }
    }
}
