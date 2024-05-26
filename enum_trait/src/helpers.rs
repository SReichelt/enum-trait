use proc_macro2::{Group, Span, TokenStream, TokenTree};
use quote::ToTokens;
use std::iter;
use syn::{parse::ParseStream, punctuated::Punctuated, spanned::Spanned, *};

pub const SELF_TYPE_NAME: &str = "Self";

pub fn self_type_ident(span: Span) -> Ident {
    Ident::new(SELF_TYPE_NAME, span)
}

pub fn self_type_param(span: Span, bounds: TypeParamBounds) -> GenericParam {
    GenericParam::Type(TypeParam {
        attrs: Vec::new(),
        ident: self_type_ident(span),
        colon_token: None,
        bounds,
        eq_token: None,
        default: None,
    })
}

pub fn ident_with_prefix(ident: &Ident, prefix: &str) -> Ident {
    Ident::new(&format!("{prefix}{ident}"), ident.span())
}

pub fn ident_with_suffix(ident: &Ident, suffix: &str) -> Ident {
    Ident::new(&format!("{ident}{suffix}"), ident.span())
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

pub fn has_complex_type_arg(path: &Path) -> bool {
    for segment in &path.segments {
        if let PathArguments::AngleBracketed(args) = &segment.arguments {
            for arg in &args.args {
                if let GenericArgument::Type(ty) = arg {
                    if get_type_ident(ty).is_none() {
                        return true;
                    }
                }
            }
        }
    }
    false
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
    for token_tree in stream {
        match token_tree {
            TokenTree::Group(group) => {
                let group_result = replace_tokens(group.stream(), src, dst);
                result.extend(iter::once(TokenTree::Group(Group::new(
                    group.delimiter(),
                    group_result,
                ))));
            }
            TokenTree::Ident(ident) if &ident == src => dst.to_tokens(&mut result),
            _ => result.extend(iter::once(token_tree)),
        }
    }
    result
}
