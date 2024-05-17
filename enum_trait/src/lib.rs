use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemFn, Result,
};

mod expr;
mod generics;
mod item;
mod output;
mod subst;

use item::MetaItemList;

#[proc_macro]
pub fn meta(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let block = parse_macro_input!(input as MetaBlock);
    block.0.into()
}

struct MetaBlock(TokenStream);

impl Parse for MetaBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let input_list: MetaItemList = input.parse()?;
        let output_list = input_list.output()?;
        Ok(MetaBlock(output_list.into_token_stream()))
    }
}

#[proc_macro_attribute]
pub fn const_test(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_fn = parse_macro_input!(item as ItemFn);
    let ident = item_fn.sig.ident.clone();
    let mut output_tokens = quote!(#[test]);
    item_fn.to_tokens(&mut output_tokens);
    output_tokens.append_all(quote!(const _: () = #ident();));
    output_tokens.into()
}
