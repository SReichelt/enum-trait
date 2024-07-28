use std::{
    borrow::Cow,
    mem::{replace, take},
};

use parse::discouraged::Speculative;
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    visit_mut::*,
    *,
};

use crate::helpers::*;

#[derive(Default)]
pub struct MetaGenerics {
    pub lt_token: Option<Token![<]>,
    pub params: Punctuated<MetaGenericParam, Token![,]>,
    pub gt_token: Option<Token![>]>,
    pub where_clause: Option<MetaWhereClause>,
}

impl Parse for MetaGenerics {
    fn parse(input: ParseStream) -> Result<Self> {
        if !input.peek(Token![<]) {
            return Ok(MetaGenerics::default());
        }

        let lt_token: Token![<] = input.parse()?;

        let mut params = Punctuated::new();
        loop {
            if input.peek(Token![>]) {
                break;
            }
            params.push_value(input.parse()?);
            if input.peek(Token![>]) {
                break;
            }
            params.push_punct(input.parse()?);
        }

        let gt_token: Token![>] = input.parse()?;

        Ok(MetaGenerics {
            lt_token: Some(lt_token),
            params,
            gt_token: Some(gt_token),
            where_clause: None,
        })
    }
}

impl ToTokens for MetaGenerics {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt_token.to_tokens(tokens);
        self.params.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
    }
}

impl MetaGenerics {
    // Checks whether `path` or any argument within `path` refers to a `TypeBoundParam` that is part
    // of `self`.
    pub fn contained_in_path(&self, path: &Path, recursive: bool) -> bool {
        if recursive {
            for segment in &path.segments {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if self.contained_in_args(args, recursive) {
                        return true;
                    }
                }
            }
        }
        self.get_bounds_if_type_bound_param(path).is_some()
    }

    // Checks whether any argument within `args` refers to a `TypeBoundParam` that is part of
    // `self`.
    fn contained_in_args(&self, args: &AngleBracketedGenericArguments, recursive: bool) -> bool {
        args.args
            .iter()
            .any(|arg| self.contained_in_arg(arg, recursive))
    }

    // Checks whether `arg` refers to a `TypeBoundParam` that is part of `self`.
    fn contained_in_arg(&self, arg: &GenericArgument, recursive: bool) -> bool {
        match arg {
            GenericArgument::Type(Type::Path(TypePath { qself: None, path })) => {
                self.contained_in_path(path, recursive)
            }
            _ => false,
        }
    }

    // If any bound in `bounds` refers to a `TypeBoundParam` that is part of `self`, replaces that
    // bound with the bounds of the `TypeBoundParam`, i.e. makes it "non-generic".
    pub fn erase_in_bounds(&self, bounds: &mut TypeParamBounds) {
        for bound in &mut *bounds {
            if let Some(replacement_bounds) = self.get_replacement_bounds(bound) {
                if replacement_bounds.len() == 1 {
                    let mut replacement_bounds = replacement_bounds.clone();
                    self.erase_in_bounds(&mut replacement_bounds);
                    if replacement_bounds.len() == 1 {
                        *bound = replacement_bounds.into_iter().next().unwrap();
                        continue;
                    }
                }
                self.erase_in_bounds_slow_path(bounds);
                return;
            }
        }
    }

    // Variant of `erase_in_bounds` that constructs completely new bounds; necessary if at least one
    // replacement consists of multiple bounds combined with `+`.
    fn erase_in_bounds_slow_path(&self, bounds: &mut TypeParamBounds) {
        let orig_bounds = replace(bounds, Punctuated::new());
        for bound_pair in orig_bounds.into_pairs() {
            if let Some(replacement_bounds) = self.get_replacement_bounds(bound_pair.value()) {
                let mut replacement_bounds = replacement_bounds.clone();
                self.erase_in_bounds(&mut replacement_bounds);
                for replacement_pair in replacement_bounds.into_pairs() {
                    let (value, punct) = replacement_pair.into_tuple();
                    bounds.push_value(value);
                    if let Some(punct) = punct.or(bound_pair.punct().cloned()) {
                        bounds.push_punct(punct);
                    }
                }
            } else {
                let (value, punct) = bound_pair.into_tuple();
                bounds.push_value(value);
                if let Some(punct) = punct {
                    bounds.push_punct(punct);
                }
            }
        }
    }

    fn get_replacement_bounds(&self, bound: &TypeParamBound) -> Option<&TypeParamBounds> {
        if let TypeParamBound::Trait(trait_bound) = bound {
            if let Some(result) = self.get_bounds_if_type_bound_param(&trait_bound.path) {
                return Some(result);
            }
        }

        None
    }

    fn get_bounds_if_type_bound_param(&self, path: &Path) -> Option<&TypeParamBounds> {
        if let Some(ident) = path.get_ident() {
            for param in &self.params {
                if let MetaGenericParam::TypeBound(type_bound_param) = param {
                    if ident == &type_bound_param.ident {
                        return Some(&type_bound_param.bounds);
                    }
                }
            }
        }
        None
    }

    // Calls `erase_in_bounds` on all bounds within generics.
    pub fn erase_in_generics(&self, generics: &mut Generics) {
        for param in &mut generics.params {
            self.erase_in_generic_param(param);
        }
        self.erase_in_where_clause(&mut generics.where_clause);
    }

    // Calls `erase_in_bounds` on all bounds within a single generic param.
    pub fn erase_in_generic_param(&self, param: &mut GenericParam) {
        if let GenericParam::Type(type_param) = param {
            self.erase_in_bounds(&mut type_param.bounds);
        }
    }

    // Calls `erase_in_bounds` on all bounds within a `where` clause.
    pub fn erase_in_where_clause(&self, where_clause: &mut Option<WhereClause>) {
        if let Some(where_clause) = where_clause {
            for predicate in &mut where_clause.predicates {
                self.erase_in_where_predicate(predicate);
            }
        }
    }

    // Calls `erase_in_bounds` on all bounds within a single `where` predicate.
    pub fn erase_in_where_predicate(&self, predicate: &mut WherePredicate) {
        if let WherePredicate::Type(type_predicate) = predicate {
            self.erase_in_bounds(&mut type_predicate.bounds);
        }
    }

    // Converts this `MetaGenerics` instance to `Generics` by
    // * omitting all type bound params, and
    // * removing references to those type bound params from the bounds of all other params, via
    //   `RemoveTypeBoundParamsFromPathArguments`.
    pub fn extract_generics(&self) -> Generics {
        let mut params = Punctuated::new();
        for pair in self.params.pairs() {
            let (value, punct) = pair.into_tuple();
            if let MetaGenericParam::Generic(generic_param) = value {
                let mut generic_param = generic_param.clone();
                RemoveTypeBoundParamsFromPathArguments(self)
                    .visit_generic_param_mut(&mut generic_param);
                params.push_value(generic_param);
                if let Some(punct) = punct {
                    params.push_punct(punct.clone());
                }
            }
        }
        let (lt_token, gt_token) = if params.is_empty() {
            (None, None)
        } else {
            (self.lt_token.clone(), self.gt_token.clone())
        };
        Generics {
            lt_token,
            params,
            gt_token,
            where_clause: self
                .where_clause
                .as_ref()
                .and_then(MetaWhereClause::extract_where_clause),
        }
    }
}

pub enum MetaGenericParam {
    Generic(GenericParam),
    TypeBound(TypeBoundParam),
}

impl Parse for MetaGenericParam {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![trait]) {
            Ok(MetaGenericParam::TypeBound(input.parse()?))
        } else {
            Ok(MetaGenericParam::Generic(input.parse()?))
        }
    }
}

impl ToTokens for MetaGenericParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MetaGenericParam::Generic(generic_param) => generic_param.to_tokens(tokens),
            MetaGenericParam::TypeBound(type_bound_param) => type_bound_param.to_tokens(tokens),
        }
    }
}

pub struct TypeBoundParam {
    pub trait_token: Token![trait],
    pub ident: Ident,
    pub colon_token: Option<Token![:]>,
    pub bounds: TypeParamBounds,
}

impl Parse for TypeBoundParam {
    fn parse(input: ParseStream) -> Result<Self> {
        let trait_token: Token![trait] = input.parse()?;
        let ident: Ident = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let bounds = if colon_token.is_some() {
            parse_type_param_bounds(input)?
        } else {
            Punctuated::new()
        };

        Ok(TypeBoundParam {
            trait_token,
            ident,
            colon_token,
            bounds,
        })
    }
}

impl ToTokens for TypeBoundParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.trait_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.bounds.to_tokens(tokens);
    }
}

pub struct MetaWhereClause {
    pub where_token: Token![where],
    pub predicates: Punctuated<MetaWherePredicate, Token![,]>,
}

impl Parse for MetaWhereClause {
    fn parse(input: ParseStream) -> Result<Self> {
        let where_token: Token![where] = input.parse()?;
        let mut predicates = Punctuated::new();
        loop {
            if input.is_empty()
                || input.peek(token::Brace)
                || input.peek(Token![,])
                || input.peek(Token![;])
                || input.peek(Token![:]) && !input.peek(Token![::])
                || input.peek(Token![=])
            {
                break;
            }
            let value = input.parse()?;
            predicates.push_value(value);
            if !input.peek(Token![,]) {
                break;
            }
            let punct = input.parse()?;
            predicates.push_punct(punct);
        }
        Ok(MetaWhereClause {
            where_token,
            predicates,
        })
    }
}

impl ToTokens for MetaWhereClause {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if !self.predicates.is_empty() {
            self.where_token.to_tokens(tokens);
            self.predicates.to_tokens(tokens);
        }
    }
}

impl MetaWhereClause {
    fn parse_option(input: ParseStream) -> Result<Option<Self>> {
        if input.peek(Token![where]) {
            input.parse().map(Some)
        } else {
            Ok(None)
        }
    }

    pub fn extract_where_clause(&self) -> Option<WhereClause> {
        // Omit type equalities, as we don't have a robust proof mechanism to convince Rust that
        // they are satisfied. Instead, we trust the user to define the variant combinations
        // equivalently to the `where` clause.
        let mut predicates = Punctuated::new();
        for pair in self.predicates.pairs() {
            let (predicate, punct) = pair.into_tuple();
            if let MetaWherePredicate::Predicate(predicate) = predicate {
                predicates.push_value(predicate.clone());
                if let Some(punct) = punct {
                    predicates.push_punct(punct.clone());
                }
            }
        }
        if predicates.is_empty() {
            None
        } else {
            Some(WhereClause {
                where_token: self.where_token.clone(),
                predicates,
            })
        }
    }
}

pub enum MetaWherePredicate {
    Predicate(WherePredicate),
    TypeEq(TypeEqPredicate),
}

impl Parse for MetaWherePredicate {
    fn parse(input: ParseStream) -> Result<Self> {
        if !(input.peek(Lifetime) && input.peek2(Token![:])) {
            let ahead = input.fork();
            let lifetimes: Option<BoundLifetimes> = ahead.parse()?;
            if lifetimes.is_none() {
                ahead.parse::<Type>()?;
                if ahead.peek(Token![=]) {
                    return Ok(MetaWherePredicate::TypeEq(input.parse()?));
                }
            }
        }
        Ok(MetaWherePredicate::Predicate(input.parse()?))
    }
}

impl ToTokens for MetaWherePredicate {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MetaWherePredicate::Predicate(predicate) => predicate.to_tokens(tokens),
            MetaWherePredicate::TypeEq(predicate) => predicate.to_tokens(tokens),
        }
    }
}

pub struct TypeEqPredicate {
    pub left_ty: Type,
    pub eq_token: Token![=],
    pub right_ty: Type,
}

impl Parse for TypeEqPredicate {
    fn parse(input: ParseStream) -> Result<Self> {
        let left_ty: Type = input.parse()?;
        let eq_token: Token![=] = input.parse()?;
        let right_ty: Type = input.parse()?;
        Ok(TypeEqPredicate {
            left_ty,
            eq_token,
            right_ty,
        })
    }
}

impl ToTokens for TypeEqPredicate {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.left_ty.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.right_ty.to_tokens(tokens);
    }
}

pub struct RemoveTypeBoundParamsFromPathArguments<'a>(pub &'a MetaGenerics);

impl VisitMut for RemoveTypeBoundParamsFromPathArguments<'_> {
    fn visit_path_arguments_mut(&mut self, i: &mut syn::PathArguments) {
        if let PathArguments::AngleBracketed(args) = i {
            if self.0.contained_in_args(args, false) {
                let mut new_args = Punctuated::new();
                for pair in take(&mut args.args).into_pairs() {
                    let (value, punct) = pair.into_tuple();
                    if !self.0.contained_in_arg(&value, false) {
                        new_args.push_value(value);
                        if let Some(punct) = punct {
                            new_args.push_punct(punct);
                        }
                    }
                }
                if new_args.is_empty() {
                    *i = PathArguments::None;
                } else {
                    args.args = new_args;
                }
            }
        }

        visit_path_arguments_mut(self, i);
    }
}

pub struct MetaGenericArguments {
    pub lt_token: Option<Token![<]>,
    pub args: Punctuated<MetaGenericArgument, Token![,]>,
    pub gt_token: Option<Token![>]>,
}

impl MetaGenericArguments {
    pub fn has_complex_type_arg(&self) -> bool {
        for arg in &self.args {
            if let MetaGenericArgument::Generic(GenericArgument::Type(ty)) = arg {
                if get_type_ident(ty).is_none() {
                    return true;
                }
            }
        }
        false
    }

    pub fn extract_path_arguments(&self) -> PathArguments {
        if let (Some(lt_token), Some(gt_token)) = (&self.lt_token, &self.gt_token) {
            let mut args = Punctuated::new();
            for pair in self.args.pairs() {
                let (value, punct) = pair.into_tuple();
                if let MetaGenericArgument::Generic(generic_arg) = value {
                    args.push_value(generic_arg.clone());
                    if let Some(punct) = punct {
                        args.push_punct(punct.clone());
                    }
                }
            }
            if args.is_empty() {
                PathArguments::None
            } else {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    colon2_token: None,
                    lt_token: lt_token.clone(),
                    args,
                    gt_token: gt_token.clone(),
                })
            }
        } else {
            PathArguments::None
        }
    }
}

impl Parse for MetaGenericArguments {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = Punctuated::new();
        let lt_token: Option<Token![<]> = input.parse()?;
        let mut gt_token: Option<Token![>]> = None;
        if lt_token.is_some() {
            loop {
                if input.peek(Token![>]) {
                    break;
                }
                args.push_value(input.parse()?);
                if input.peek(Token![>]) {
                    break;
                }
                args.push_punct(input.parse()?);
            }
            gt_token = Some(input.parse()?);
        }
        Ok(MetaGenericArguments {
            lt_token,
            args,
            gt_token,
        })
    }
}

impl ToTokens for MetaGenericArguments {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt_token.to_tokens(tokens);
        self.args.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
    }
}

pub enum MetaGenericArgument {
    Generic(GenericArgument),
    TraitAlias(TraitAliasArgument),
}

impl Parse for MetaGenericArgument {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![trait]) {
            Ok(MetaGenericArgument::TraitAlias(input.parse()?))
        } else {
            Ok(MetaGenericArgument::Generic(input.parse()?))
        }
    }
}

impl ToTokens for MetaGenericArgument {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MetaGenericArgument::Generic(generic_arg) => generic_arg.to_tokens(tokens),
            MetaGenericArgument::TraitAlias(alias_arg) => alias_arg.to_tokens(tokens),
        }
    }
}

pub struct MetaSignature {
    pub constness: Option<Token![const]>,
    pub asyncness: Option<Token![async]>,
    pub unsafety: Option<Token![unsafe]>,
    pub abi: Option<Abi>,
    pub fn_token: Token![fn],
    pub ident: Ident,
    pub generics: MetaGenerics,
    pub paren_token: token::Paren,
    pub inputs: Punctuated<FnArg, Token![,]>,
    pub variadic: Option<Variadic>,
    pub output: ReturnType,
}

impl Parse for MetaSignature {
    fn parse(input: ParseStream) -> Result<Self> {
        let constness: Option<Token![const]> = input.parse()?;
        let asyncness: Option<Token![async]> = input.parse()?;
        let unsafety: Option<Token![unsafe]> = input.parse()?;
        let abi: Option<Abi> = input.parse()?;
        let fn_token: Token![fn] = input.parse()?;
        let ident: Ident = input.parse()?;
        let mut generics: MetaGenerics = input.parse()?;

        let content;
        let paren_token = parenthesized!(content in input);
        let (inputs, variadic) = parse_fn_args(&content)?;

        let output: ReturnType = input.parse()?;
        generics.where_clause = input.call(MetaWhereClause::parse_option)?;

        Ok(MetaSignature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token,
            ident,
            generics,
            paren_token,
            inputs,
            variadic,
            output,
        })
    }
}

impl MetaSignature {
    pub fn extract_signature(&self) -> Signature {
        let mut sig = Signature {
            constness: self.constness.clone(),
            asyncness: self.asyncness.clone(),
            unsafety: self.unsafety.clone(),
            abi: self.abi.clone(),
            fn_token: self.fn_token.clone(),
            ident: self.ident.clone(),
            generics: self.generics.extract_generics(),
            paren_token: self.paren_token.clone(),
            inputs: self.inputs.clone(),
            variadic: self.variadic.clone(),
            output: self.output.clone(),
        };
        RemoveTypeBoundParamsFromPathArguments(&self.generics).visit_signature_mut(&mut sig);
        sig
    }
}

// Copied from `syn` crate, where unfortunately it is not public.
fn parse_fn_args(input: ParseStream) -> Result<(Punctuated<FnArg, Token![,]>, Option<Variadic>)> {
    let mut args = Punctuated::new();
    let mut variadic = None;
    let mut has_receiver = false;

    while !input.is_empty() {
        let attrs = input.call(Attribute::parse_outer)?;

        if let Some(dots) = input.parse::<Option<Token![...]>>()? {
            variadic = Some(Variadic {
                attrs,
                pat: None,
                dots,
                comma: if input.is_empty() {
                    None
                } else {
                    Some(input.parse()?)
                },
            });
            break;
        }

        let allow_variadic = true;
        let arg = match parse_fn_arg_or_variadic(input, attrs, allow_variadic)? {
            FnArgOrVariadic::FnArg(arg) => arg,
            FnArgOrVariadic::Variadic(arg) => {
                variadic = Some(Variadic {
                    comma: if input.is_empty() {
                        None
                    } else {
                        Some(input.parse()?)
                    },
                    ..arg
                });
                break;
            }
        };

        match &arg {
            FnArg::Receiver(receiver) if has_receiver => {
                return Err(Error::new(
                    receiver.self_token.span,
                    "unexpected second method receiver",
                ));
            }
            FnArg::Receiver(receiver) if !args.is_empty() => {
                return Err(Error::new(
                    receiver.self_token.span,
                    "unexpected method receiver",
                ));
            }
            FnArg::Receiver(_) => has_receiver = true,
            FnArg::Typed(_) => {}
        }
        args.push_value(arg);

        if input.is_empty() {
            break;
        }

        let comma: Token![,] = input.parse()?;
        args.push_punct(comma);
    }

    Ok((args, variadic))
}

// Copied from `syn` crate, where unfortunately it is not public.
enum FnArgOrVariadic {
    FnArg(FnArg),
    Variadic(Variadic),
}

// Copied from `syn` crate, where unfortunately it is not public.
fn parse_fn_arg_or_variadic(
    input: ParseStream,
    attrs: Vec<Attribute>,
    allow_variadic: bool,
) -> Result<FnArgOrVariadic> {
    let ahead = input.fork();
    if let Ok(mut receiver) = ahead.parse::<Receiver>() {
        input.advance_to(&ahead);
        receiver.attrs = attrs;
        return Ok(FnArgOrVariadic::FnArg(FnArg::Receiver(receiver)));
    }

    // Hack to parse pre-2018 syntax in
    // test/ui/rfc-2565-param-attrs/param-attrs-pretty.rs
    // because the rest of the test case is valuable.
    if input.peek(Ident) && input.peek2(Token![<]) {
        let span = input.fork().parse::<Ident>()?.span();
        return Ok(FnArgOrVariadic::FnArg(FnArg::Typed(PatType {
            attrs,
            pat: Box::new(Pat::Wild(PatWild {
                attrs: Vec::new(),
                underscore_token: Token![_](span),
            })),
            colon_token: Token![:](span),
            ty: input.parse()?,
        })));
    }

    let pat = Box::new(Pat::parse_single(input)?);
    let colon_token: Token![:] = input.parse()?;

    if allow_variadic {
        if let Some(dots) = input.parse::<Option<Token![...]>>()? {
            return Ok(FnArgOrVariadic::Variadic(Variadic {
                attrs,
                pat: Some((pat, colon_token)),
                dots,
                comma: None,
            }));
        }
    }

    Ok(FnArgOrVariadic::FnArg(FnArg::Typed(PatType {
        attrs,
        pat,
        colon_token,
        ty: input.parse()?,
    })))
}

pub struct TraitAliasArgument {
    pub trait_token: Token![trait],
    pub ident: Ident,
    pub eq_token: Token![=],
    pub value: Path,
}

impl Parse for TraitAliasArgument {
    fn parse(input: ParseStream) -> Result<Self> {
        let trait_token: Token![trait] = input.parse()?;
        let ident: Ident = input.parse()?;
        let eq_token: Token![=] = input.parse()?;
        let value: Path = input.parse()?;
        Ok(TraitAliasArgument {
            trait_token,
            ident,
            eq_token,
            value,
        })
    }
}

impl ToTokens for TraitAliasArgument {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.trait_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

pub enum GenericsContext<'a> {
    Empty,
    WithSelf(Cow<'a, GenericParam>, &'a GenericsContext<'a>),
    WithGenerics(&'a Generics, &'a GenericsContext<'a>),
}
