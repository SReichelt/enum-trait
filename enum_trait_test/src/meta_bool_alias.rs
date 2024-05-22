use std::marker::PhantomData;

use enum_trait::meta;

use enum_trait_core::meta_bool::*;

meta! {
    pub trait MetaBoolAlias = MetaBool;
}

#[derive(Default)]
pub struct AliasTest<B: MetaBoolAlias>(PhantomData<B>);

pub fn alias_test_false() -> AliasTest<False> {
    Default::default()
}

pub fn alias_test_true() -> AliasTest<True> {
    Default::default()
}
