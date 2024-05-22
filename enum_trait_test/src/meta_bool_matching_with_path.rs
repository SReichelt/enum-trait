use enum_trait::meta;

use enum_trait_core::{meta_bool, meta_num::*};

meta! {
    pub trait MetaBoolWithNumConv2 = meta_bool::MetaBool;

    trait impl MetaBoolWithNumConv2 {
        pub type ToMetaNum: MetaNum = match <Self> {
            meta_bool::False => meta_num!(0),
            meta_bool::True => meta_num!(1),
        };
    }
}

pub const fn bool_match_test<B: MetaBoolWithNumConv2>() -> usize {
    B::ToMetaNum::VALUE
}

const _: () = {
    assert!(bool_match_test::<meta_bool::False>() == 0);
    assert!(bool_match_test::<meta_bool::True>() == 1);
};
