use std::marker::PhantomData;

use enum_trait::meta;

use enum_trait_core::meta_bool::*;

use crate::meta_bool_alias::*;

meta! {
    pub trait MetaBoolAliasAlias = MetaBoolAlias;
}

#[derive(Default)]
pub struct AliasTest<B: MetaBoolAliasAlias>(PhantomData<B>);

pub fn alias_test_false() -> AliasTest<False> {
    Default::default()
}

pub fn alias_test_true() -> AliasTest<True> {
    Default::default()
}
