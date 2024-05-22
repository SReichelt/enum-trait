use enum_trait::meta;

use enum_trait_core::{meta_bool::*, meta_num::*};

meta! {
    pub trait MetaBoolWithNumConv = MetaBool;

    trait impl MetaBoolWithNumConv {
        pub type ToMetaNum: MetaNum = match <Self> {
            False => meta_num!(0),
            True => meta_num!(1),
        };
    }
}

pub const fn bool_match_test<B: MetaBoolWithNumConv>() -> usize {
    B::ToMetaNum::VALUE
}

const _: () = {
    assert!(bool_match_test::<False>() == 0);
    assert!(bool_match_test::<True>() == 1);
};
