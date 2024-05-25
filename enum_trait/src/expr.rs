use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    *,
};

use crate::generics::TypeParamBounds;

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

pub trait ParseExt: Parse {
    fn parse_match_arm_body(input: ParseStream) -> Result<Self> {
        input.parse()
    }

    fn requires_terminator(&self) -> bool {
        true
    }

    fn has_match_expr() -> bool;
}

impl ParseExt for Expr {
    fn parse_match_arm_body(input: ParseStream) -> Result<Self> {
        input.call(Expr::parse_with_earlier_boundary_rule)
    }

    fn requires_terminator(&self) -> bool {
        // `requires_terminator` unfortunately not exported from syn crate
        match self {
            Expr::If(_)
            | Expr::Match(_)
            | Expr::Block(_)
            | Expr::Unsafe(_)
            | Expr::While(_)
            | Expr::Loop(_)
            | Expr::ForLoop(_)
            | Expr::TryBlock(_)
            | Expr::Const(_) => false,
            _ => true,
        }
    }

    fn has_match_expr() -> bool {
        return true;
    }
}

impl ParseExt for Type {
    fn has_match_expr() -> bool {
        return false;
    }
}

#[derive(Clone)]
pub enum TypeLevelExpr<E> {
    Expr(E),
    Match(TypeLevelExprMatch<TypeLevelExpr<E>>),
}

impl<E: ParseExt> TypeLevelExpr<E> {
    fn parse_impl(
        input: ParseStream,
        parse_expr: impl FnOnce(ParseStream) -> Result<E>,
    ) -> Result<Self> {
        if input.peek(Token![match]) && (!E::has_match_expr() || input.peek2(Token![<])) {
            Ok(TypeLevelExpr::Match(input.parse()?))
        } else {
            Ok(TypeLevelExpr::Expr(parse_expr(input)?))
        }
    }
}

impl<E: ParseExt> Parse for TypeLevelExpr<E> {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse_impl(input, |input| input.parse())
    }
}

impl<E: ParseExt> ParseExt for TypeLevelExpr<E> {
    fn parse_match_arm_body(input: ParseStream) -> Result<Self> {
        Self::parse_impl(input, E::parse_match_arm_body)
    }

    fn requires_terminator(&self) -> bool {
        match self {
            TypeLevelExpr::Expr(expr) => expr.requires_terminator(),
            TypeLevelExpr::Match(_) => false,
        }
    }

    fn has_match_expr() -> bool {
        return true;
    }
}

impl<E: ToTokens> ToTokens for TypeLevelExpr<E> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TypeLevelExpr::Expr(expr) => expr.to_tokens(tokens),
            TypeLevelExpr::Match(match_expr) => match_expr.to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub struct TypeLevelExprMatch<E> {
    pub match_token: token::Match,
    pub lt_token: token::Lt,
    pub types: Punctuated<Type, token::Comma>,
    pub gt_token: token::Gt,
    pub brace_token: token::Brace,
    pub arms: Vec<TypeLevelArm<E>>,
}

impl<E: ParseExt> Parse for TypeLevelExprMatch<E> {
    fn parse(input: ParseStream) -> Result<Self> {
        let match_token: Token![match] = input.parse()?;
        let lt_token: Token![<] = input.parse()?;
        let types: Punctuated<Type, Token![,]> = Punctuated::parse_separated_nonempty(input)?;
        input.parse::<Option<Token![,]>>()?;
        let gt_token: Token![>] = input.parse()?;
        let content: ParseBuffer;
        let brace_token = braced!(content in input);
        let mut arms = Vec::new();
        while !content.is_empty() {
            arms.push(content.call(TypeLevelArm::parse)?);
        }
        Ok(TypeLevelExprMatch {
            match_token,
            lt_token,
            types,
            gt_token,
            brace_token,
            arms,
        })
    }
}

impl<E: ToTokens> ToTokens for TypeLevelExprMatch<E> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.match_token.to_tokens(tokens);
        self.lt_token.to_tokens(tokens);
        self.types.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            tokens.append_all(&self.arms);
        });
    }
}

#[derive(Clone)]
pub struct TypeLevelArm<E> {
    pub selectors: Punctuated<TypeLevelArmSelector, token::Comma>,
    pub fat_arrow_token: token::FatArrow,
    pub body: E,
    pub comma_token: Option<token::Comma>,
}

impl<E: ParseExt> Parse for TypeLevelArm<E> {
    fn parse(input: ParseStream) -> Result<Self> {
        let selectors: Punctuated<TypeLevelArmSelector, Token![,]> =
            Punctuated::parse_separated_nonempty(input)?;
        input.parse::<Option<Token![,]>>()?;
        let fat_arrow_token: Token![=>] = input.parse()?;
        let body = E::parse_match_arm_body(input)?;
        let comma_token: Option<Token![,]> = if input.is_empty() {
            None
        } else if body.requires_terminator() {
            Some(input.parse::<Token![,]>()?)
        } else {
            input.parse::<Option<Token![,]>>()?
        };
        Ok(TypeLevelArm {
            selectors,
            fat_arrow_token,
            body,
            comma_token,
        })
    }
}

impl<E: ToTokens> ToTokens for TypeLevelArm<E> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.selectors.to_tokens(tokens);
        self.fat_arrow_token.to_tokens(tokens);
        self.body.to_tokens(tokens);
        self.comma_token.to_tokens(tokens);
    }
}

#[derive(Clone)]
pub enum TypeLevelArmSelector {
    Specific { ident: Ident, generics: Generics },
    Default { underscore_token: token::Underscore },
}

impl Parse for TypeLevelArmSelector {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Some(underscore_token) = input.parse::<Option<Token![_]>>()? {
            Ok(TypeLevelArmSelector::Default { underscore_token })
        } else {
            let mut ident: Ident;
            input.parse::<Option<Token![::]>>()?;
            loop {
                ident = input.parse()?;
                if input.parse::<Option<Token![::]>>()?.is_none() {
                    break;
                }
            }
            let generics: Generics = input.parse()?;
            Ok(TypeLevelArmSelector::Specific { ident, generics })
        }
    }
}

impl ToTokens for TypeLevelArmSelector {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TypeLevelArmSelector::Specific { ident, generics } => {
                ident.to_tokens(tokens);
                generics.to_tokens(tokens);
            }
            TypeLevelArmSelector::Default { underscore_token } => {
                underscore_token.to_tokens(tokens);
            }
        }
    }
}

#[derive(Clone)]
pub struct TypeLevelLambda<E> {
    pub generics: Generics,
    pub body: E,
}

impl<E: Parse> Parse for TypeLevelLambda<E> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![|]>()?;
        let generics: Generics = input.parse()?;
        input.parse::<Token![|]>()?;
        let body: E = input.parse()?;
        Ok(TypeLevelLambda { generics, body })
    }
}
