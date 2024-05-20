use enum_trait::meta;

meta! {
    pub trait MetaBoolWithNumConv = MetaBool;

    trait impl MetaBoolWithNumConv {
        pub type ToMetaNum: MetaNum = match <Self> {
            False => meta_num!(0),
            True => meta_num!(1),
        }
    }
}
