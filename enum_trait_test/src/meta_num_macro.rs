// Check that the `MetaNum` trait can be used conveniently, and that the `meta_num` macro can be
// called outside of `enum_trait_core`.

use std::marker::PhantomData;

use enum_trait_core::meta_num::{meta_num, MetaNum};

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TypeWithMetaNum<N: MetaNum>(PhantomData<N>);

pub trait IsStatic: 'static {}

impl<N: MetaNum> IsStatic for TypeWithMetaNum<N> {}

pub fn create_instance_with_meta_num() -> TypeWithMetaNum<meta_num!(42)> {
    Default::default()
}

pub fn copy_instance_with_meta_num<N: MetaNum>(inst: &TypeWithMetaNum<N>) -> TypeWithMetaNum<N> {
    *inst
}
