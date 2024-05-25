use std::{borrow::Cow, mem::take};

use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

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
            let punct = input.parse()?;
            params.push_punct(punct);
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
    pub fn eliminate_in_generics(&self, generics: &mut Generics) {
        for param in &mut generics.params {
            self.eliminate_in_generic_param(param);
        }
        self.eliminate_in_where_clause(&mut generics.where_clause);
    }

    pub fn eliminate_in_generic_param(&self, param: &mut GenericParam) {
        if let GenericParam::Type(type_param) = param {
            self.eliminate_in_type_param_bounds(&mut type_param.bounds);
        }
    }

    pub fn eliminate_in_type_param_bounds(&self, bounds: &mut TypeParamBounds) {
        for bound in &mut *bounds {
            self.eliminate_in_type_param_bound_1(bound);
        }

        for bound in &mut *bounds {
            if let Some(replacement_bounds) = self.get_eliminated_in_type_param_bound_2(bound) {
                if replacement_bounds.len() == 1 {
                    let mut replacement_bounds = replacement_bounds.clone();
                    self.eliminate_in_type_param_bounds(&mut replacement_bounds);
                    *bound = replacement_bounds.into_iter().next().unwrap();
                } else {
                    *bounds = self.get_eliminated_in_type_param_bounds_2(take(bounds));
                    return;
                }
            }
        }
    }

    fn eliminate_in_type_param_bound_1(&self, bound: &mut TypeParamBound) {
        if let TypeParamBound::Trait(trait_bound) = bound {
            for segment in &mut trait_bound.path.segments {
                if let PathArguments::AngleBracketed(args) = &mut segment.arguments {
                    if args.args.iter().any(|arg| match arg {
                        GenericArgument::Type(Type::Path(TypePath { qself: None, path })) => {
                            self.get_eliminated_in_path(path).is_some()
                        }
                        _ => false,
                    }) {
                        let mut new_args = Punctuated::new();
                        for pair in take(&mut args.args).into_pairs() {
                            let (value, punct) = pair.into_tuple();
                            let references_meta_param = match &value {
                                GenericArgument::Type(Type::Path(TypePath {
                                    qself: None,
                                    path,
                                })) => self.get_eliminated_in_path(path).is_some(),
                                _ => false,
                            };
                            if !references_meta_param {
                                new_args.push_value(value);
                                if let Some(punct) = punct {
                                    new_args.push_punct(punct);
                                }
                            }
                        }
                        if new_args.is_empty() {
                            segment.arguments = PathArguments::None;
                        } else {
                            args.args = new_args;
                        }
                    }
                }
            }
        }
    }

    fn get_eliminated_in_type_param_bounds_2(&self, bounds: TypeParamBounds) -> TypeParamBounds {
        let mut new_bounds = Punctuated::new();
        for bound_pair in bounds.into_pairs() {
            if let Some(replacement_bounds) =
                self.get_eliminated_in_type_param_bound_2(bound_pair.value())
            {
                let mut replacement_bounds = replacement_bounds.clone();
                self.eliminate_in_type_param_bounds(&mut replacement_bounds);
                for replacement_pair in replacement_bounds.into_pairs() {
                    let (value, punct) = replacement_pair.into_tuple();
                    new_bounds.push_value(value);
                    if let Some(punct) = punct.or(bound_pair.punct().cloned()) {
                        new_bounds.push_punct(punct);
                    }
                }
            } else {
                let (value, punct) = bound_pair.into_tuple();
                new_bounds.push_value(value);
                if let Some(punct) = punct {
                    new_bounds.push_punct(punct);
                }
            }
        }
        new_bounds
    }

    fn get_eliminated_in_type_param_bound_2(
        &self,
        bound: &TypeParamBound,
    ) -> Option<&TypeParamBounds> {
        if let TypeParamBound::Trait(trait_bound) = bound {
            if let Some(result) = self.get_eliminated_in_path(&trait_bound.path) {
                return Some(result);
            }
        }

        None
    }

    fn get_eliminated_in_path(&self, path: &Path) -> Option<&TypeParamBounds> {
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

    pub fn eliminate_in_where_clause(&self, where_clause: &mut Option<WhereClause>) {
        if let Some(where_clause) = where_clause {
            for predicate in &mut where_clause.predicates {
                self.eliminate_in_where_predicate(predicate);
            }
        }
    }

    pub fn eliminate_in_where_predicate(&self, predicate: &mut WherePredicate) {
        if let WherePredicate::Type(type_predicate) = predicate {
            self.eliminate_in_type_param_bounds(&mut type_predicate.bounds);
        }
    }

    pub fn extract_generics(&self) -> Generics {
        let mut params = Punctuated::new();
        for pair in self.params.pairs() {
            let (value, punct) = pair.into_tuple();
            if let MetaGenericParam::Generic(generic_param) = value {
                let mut generic_param = generic_param.clone();
                self.eliminate_in_generic_param(&mut generic_param);
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
        // Omit `where` clauses of traits, as we don't have a robust proof mechanism to convince
        // Rust that they are satisfied. Instead, we trust the user to define the variant
        // combinations equivalently to the `where` clause.
        Generics {
            lt_token,
            params,
            gt_token,
            where_clause: None,
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
    pub left_ty: Type,
    pub eq_token: Token![=],
    pub right_ty: Type,
}

impl Parse for MetaWhereClause {
    fn parse(input: ParseStream) -> Result<Self> {
        let where_token: Token![where] = input.parse()?;
        let left_ty: Type = input.parse()?;
        let eq_token: Token![=] = input.parse()?;
        let right_ty: Type = input.parse()?;
        Ok(MetaWhereClause {
            where_token,
            left_ty,
            eq_token,
            right_ty,
        })
    }
}

impl ToTokens for MetaWhereClause {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.where_token.to_tokens(tokens);
        self.left_ty.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.right_ty.to_tokens(tokens);
    }
}

pub enum GenericsContext<'a> {
    Empty,
    WithSelf(Cow<'a, GenericParam>, &'a GenericsContext<'a>),
    WithGenerics(&'a Generics, &'a GenericsContext<'a>),
}
