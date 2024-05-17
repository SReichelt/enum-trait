use expr::TypeLevelLambda;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use subst::{ParamSubstArg, Substitutable};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    Error, GenericArgument, ItemFn, LitInt, Result, Token, Type,
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

#[proc_macro]
pub fn iterate(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let iteration = parse_macro_input!(input as TypeIteration);
    iteration.0.into()
}

struct TypeIteration(TokenStream);

impl Parse for TypeIteration {
    fn parse(input: ParseStream) -> Result<Self> {
        let count: LitInt = input.parse()?;
        input.parse::<Token![,]>()?;
        let mut ty: Type = input.parse()?;
        input.parse::<Token![,]>()?;
        let lambda: TypeLevelLambda<Type> = input.parse()?;
        input.parse::<Option<Token![,]>>()?;
        if lambda.generics.params.len() != 1 {
            return Err(Error::new(
                lambda.generics.span(),
                "exactly one type parameter expected",
            ));
        }
        let param = lambda.generics.params.first().unwrap();
        for _ in 0..count.base10_parse()? {
            let mut body = lambda.body.clone();
            body.substitute(param, ParamSubstArg::Arg(&GenericArgument::Type(ty)))?;
            ty = body;
        }
        Ok(TypeIteration(ty.into_token_stream()))
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
