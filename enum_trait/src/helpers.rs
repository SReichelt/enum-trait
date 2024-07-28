use proc_macro2::{Delimiter, Group, Span, TokenStream, TokenTree};
use quote::ToTokens;
use std::{collections::HashMap, iter};
use syn::{parse::ParseStream, punctuated::Punctuated, spanned::Spanned, *};

pub const SELF_TYPE_NAME: &str = "Self";

pub fn self_type_ident(span: Option<Span>) -> Ident {
    Ident::new(SELF_TYPE_NAME, span.unwrap_or_else(Span::call_site))
}

pub fn self_type_param(span: Option<Span>, bounds: TypeParamBounds) -> GenericParam {
    GenericParam::Type(TypeParam {
        attrs: Vec::new(),
        ident: self_type_ident(span),
        colon_token: None,
        bounds,
        eq_token: None,
        default: None,
    })
}

pub fn ident_with_prefix(ident: &Ident, prefix: &str, preserve_span: bool) -> Ident {
    Ident::new(
        &format!("{prefix}{ident}"),
        ident_span(ident, preserve_span),
    )
}

pub fn ident_with_suffix(ident: &Ident, suffix: &str, preserve_span: bool) -> Ident {
    Ident::new(
        &format!("{ident}{suffix}"),
        ident_span(ident, preserve_span),
    )
}

fn ident_span(ident: &Ident, preserve_span: bool) -> Span {
    if preserve_span {
        ident.span()
    } else {
        Span::call_site()
    }
}

pub fn get_type_ident(ty: &Type) -> Option<&Ident> {
    let Type::Path(TypePath { qself: None, path }) = ty else {
        return None;
    };
    path.get_ident()
}

pub fn type_is_ident<I>(ty: &Type, ident: &I) -> bool
where
    I: ?Sized,
    Ident: PartialEq<I>,
{
    let Type::Path(TypePath { qself: None, path }) = ty else {
        return false;
    };
    path.is_ident(ident)
}

pub fn build_generics(params: Punctuated<GenericParam, Token![,]>) -> Generics {
    let (lt_token, gt_token) = if params.is_empty() {
        (None, None)
    } else {
        (Some(Default::default()), Some(Default::default()))
    };
    Generics {
        lt_token,
        params,
        gt_token,
        where_clause: None,
    }
}

pub fn build_path_arguments(args: Punctuated<GenericArgument, Token![,]>) -> PathArguments {
    if args.is_empty() {
        PathArguments::None
    } else {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: Default::default(),
            args,
            gt_token: Default::default(),
        })
    }
}

pub fn generic_param_arg(param: &GenericParam, span: Option<Span>) -> GenericArgument {
    match param {
        GenericParam::Lifetime(lifetime_param) => {
            let mut lifetime = lifetime_param.lifetime.clone();
            if let Some(span) = span {
                lifetime.ident.set_span(span);
            }
            GenericArgument::Lifetime(lifetime)
        }
        GenericParam::Type(type_param) => {
            let mut ident = type_param.ident.clone();
            if let Some(span) = span {
                ident.set_span(span);
            }
            GenericArgument::Type(Type::Path(TypePath {
                qself: None,
                path: ident.into(),
            }))
        }
        GenericParam::Const(const_param) => {
            let mut ident = const_param.ident.clone();
            if let Some(span) = span {
                ident.set_span(span);
            }
            GenericArgument::Type(Type::Path(TypePath {
                qself: None,
                path: ident.into(),
            }))
        }
    }
}

pub fn generic_args(generics: &Generics) -> PathArguments {
    if generics.params.is_empty() {
        PathArguments::None
    } else {
        let mut args = Punctuated::new();
        for param in &generics.params {
            args.push(generic_param_arg(param, None));
        }
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: generics.lt_token.unwrap_or_default(),
            args,
            gt_token: generics.gt_token.unwrap_or_default(),
        })
    }
}

pub type TypeParamBounds = Punctuated<TypeParamBound, Token![+]>;

pub fn parse_type_param_bounds(input: ParseStream) -> Result<TypeParamBounds> {
    let mut bounds = Punctuated::new();
    loop {
        bounds.push_value(input.parse::<TypeParamBound>()?);
        if !input.peek(Token![+]) {
            break;
        }
        bounds.push_punct(input.parse::<Token![+]>()?);
    }
    Ok(bounds)
}

pub fn check_token_equality<T: ToTokens>(actual: &T, expected: &T) -> Result<()> {
    let actual_tokens = actual.to_token_stream();
    let expected_tokens = expected.to_token_stream();
    let mut expected_iter = expected_tokens.into_iter();
    for actual_token in actual_tokens {
        let Some(expected_token) = expected_iter.next() else {
            return Err(Error::new(actual_token.span(), format!("expected end")));
        };
        match (&actual_token, &expected_token) {
            (TokenTree::Group(actual_group), TokenTree::Group(expected_group)) => {
                check_token_equality(actual_group, expected_group)?
            }
            _ => {
                let actual_token_str = actual_token.to_string();
                let expected_token_str = expected_token.to_string();
                if actual_token_str != expected_token_str {
                    return Err(Error::new(
                        actual_token.span(),
                        format!("expected `{expected_token_str}`"),
                    ));
                }
            }
        }
    }
    if let Some(expected_token) = expected_iter.next() {
        let expected_token_str = expected_token.to_string();
        return Err(Error::new(
            actual.span(),
            format!("expected continuation with `{expected_token_str}`"),
        ));
    }
    Ok(())
}

pub fn replace_tokens(stream: TokenStream, src: &Ident, dst: &impl ToTokens) -> TokenStream {
    let mut result = TokenStream::new();
    for token in stream {
        match token {
            TokenTree::Group(group) => {
                let group_result = replace_tokens(group.stream(), src, dst);
                result.extend(iter::once(TokenTree::Group(Group::new(
                    group.delimiter(),
                    group_result,
                ))));
            }
            TokenTree::Ident(ident) if &ident == src => dst.to_tokens(&mut result),
            _ => result.extend(iter::once(token)),
        }
    }
    result
}

#[derive(Clone)]
pub enum MacroArg {
    Single(TokenTree),
    Multi(Vec<MacroArg>),
}

impl MacroArg {
    pub fn ident(ident: Ident) -> Self {
        MacroArg::Single(TokenTree::Ident(ident))
    }

    pub fn single(tokens: &impl ToTokens) -> Self {
        MacroArg::Single(TokenTree::Group(Group::new(
            Delimiter::None,
            tokens.to_token_stream(),
        )))
    }

    pub fn multi_tokens(tokens: &impl ToTokens) -> Self {
        MacroArg::Multi(
            tokens
                .to_token_stream()
                .into_iter()
                .map(MacroArg::Single)
                .collect(),
        )
    }
}

pub fn expand_macro_body(body: TokenStream, args: &HashMap<Ident, MacroArg>) -> TokenStream {
    expand_macro_part(body, args, &mut Vec::new()).unwrap()
}

fn expand_macro_part(
    body: TokenStream,
    args: &HashMap<Ident, MacroArg>,
    multi_arg_indices: &mut Vec<usize>,
) -> Option<TokenStream> {
    let mut result = TokenStream::new();
    let mut multi_arg_indices_matched = 0;
    let mut iter = body.into_iter();
    while let Some(token) = iter.next() {
        match token {
            TokenTree::Group(group) => {
                let group_result = expand_macro_body(group.stream(), args);
                result.extend(iter::once(TokenTree::Group(Group::new(
                    group.delimiter(),
                    group_result,
                ))));
            }
            TokenTree::Punct(punct) if punct.as_char() == '$' => match iter.next().unwrap() {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    let (multiplier, separator) = macro_part_multiplier(&mut iter);
                    let mut arg_idx = 0;
                    loop {
                        multi_arg_indices.push(arg_idx);
                        let part_result =
                            expand_macro_part(group.stream(), args, multi_arg_indices);
                        multi_arg_indices.pop();
                        if let Some(part_result) = part_result {
                            if arg_idx > 0 {
                                separator.to_tokens(&mut result);
                            }
                            result.extend(part_result);
                        } else {
                            break;
                        }
                        arg_idx += 1;
                    }
                    if multiplier == '+' && arg_idx == 0 {
                        panic!("expected at least one item in argument due to `+` multiplier");
                    }
                    if multiplier == '?' && arg_idx > 1 {
                        panic!("expected at most one item in argument due to `?` multiplier");
                    }
                }
                TokenTree::Ident(ident) => {
                    let Some(mut arg) = &args.get(&ident) else {
                        panic!("missing arg for param `{ident}`");
                    };
                    let mut cur_depth = 0;
                    loop {
                        match arg {
                            MacroArg::Single(token) => {
                                token.to_tokens(&mut result);
                                break;
                            }
                            MacroArg::Multi(items) => {
                                if cur_depth >= multi_arg_indices.len() {
                                    panic!("multi-arg `{ident}` referenced without repetition");
                                }
                                let idx = multi_arg_indices[cur_depth];
                                if idx >= items.len() {
                                    return None;
                                }
                                arg = &items[idx];
                            }
                        }
                        cur_depth += 1;
                    }
                    if multi_arg_indices_matched < cur_depth {
                        multi_arg_indices_matched = cur_depth;
                    }
                }
                _ => panic!("expected parentheses or identifier after '$'"),
            },
            _ => result.extend(iter::once(token)),
        }
    }
    if multi_arg_indices_matched < multi_arg_indices.len() {
        panic!("body contains repetition without multi-arg");
    }
    Some(result)
}

fn macro_part_multiplier(iter: &mut impl Iterator<Item = TokenTree>) -> (char, Option<TokenTree>) {
    let first_token = iter.next().unwrap();
    if let TokenTree::Punct(punct) = &first_token {
        let punct = punct.as_char();
        if matches!(punct, '*' | '+' | '?') {
            return (punct, None);
        }
    }
    if let TokenTree::Punct(punct) = iter.next().unwrap() {
        let punct = punct.as_char();
        if matches!(punct, '*' | '+') {
            return (punct, Some(first_token));
        }
    }
    panic!("expected multiplier (optionally preceded by separator)");
}
