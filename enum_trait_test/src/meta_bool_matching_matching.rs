use enum_trait::meta;

use enum_trait_core::{meta_bool::*, meta_num::*};

use crate::meta_bool_matching::*;

meta! {
    pub trait MetaBoolWithOption = MetaBoolWithNumConv;

    pub type OptionalOption<B: MetaBoolWithOption, T> = match <B> {
        False => T,
        True => Option<T>,
    };

    pub fn unwrap_if_necessary<B: MetaBoolWithOption, T>(obj: OptionalOption<B, T>) -> T {
        match <B> {
            False => obj,
            True => obj.unwrap(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unwrap_optional_option() {
        let without_option: u32 = 42;
        let with_option: Option<u32> = Some(23);

        let without_option_unwrapped: u32 = unwrap_if_necessary::<False, u32>(without_option);
        let with_option_unwrapped: u32 = unwrap_if_necessary::<True, u32>(with_option);

        assert_eq!(without_option_unwrapped, without_option);
        assert_eq!(Some(with_option_unwrapped), with_option);
    }
}
