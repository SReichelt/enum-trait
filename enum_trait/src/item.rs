use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    *,
};

use crate::{expr::*, generics::*, output::*};

pub struct MetaItemList(pub Vec<MetaItem>);

impl Parse for MetaItemList {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(MetaItemList(items))
    }
}

impl MetaItemList {
    pub fn output(&self) -> Result<OutputMetaItemList> {
        let mut result = OutputMetaItemList::new();

        // Output all trait definitions first, to avoid restrictions on the order of input items.
        for item in &self.0 {
            if let MetaItem::TraitDef(trait_item) = item {
                let TraitContents::Enum { variants } = &trait_item.contents;
                result.0.push(OutputMetaItem::TraitDef(OutputItemTraitDef {
                    trait_item,
                    variants: Some(
                        variants
                            .iter()
                            .map(|variant| OutputTraitVariant {
                                variant: variant.clone(),
                                impl_items: Vec::new(),
                            })
                            .collect(),
                    ),
                    impl_items: Vec::new(),
                    next_internal_item_idx: 0,
                }));
            }
        }

        for item in &self.0 {
            match item {
                MetaItem::TraitDef(_) => {}

                MetaItem::TraitImpl(impl_item) => {
                    if impl_item.self_trait.segments.len() != 1 {
                        return Err(Error::new_spanned(
                            &impl_item.self_trait,
                            "trait impl cannot have nontrivial path",
                        ));
                    }
                    let segment = impl_item.self_trait.segments.first().unwrap();
                    let trait_def_item = result.trait_def_item(&segment.ident)?;
                    Self::check_trait_impl_params(
                        &impl_item.generics,
                        &trait_def_item.trait_item.generics,
                    )?;
                    Self::check_trait_impl_args(&impl_item.generics, &segment.arguments)?;
                    let impl_context = trait_def_item.impl_context();
                    for item in &impl_item.items {
                        let (trait_item, variants) =
                            result.create_trait_item(item.clone(), &impl_context)?;
                        let trait_def_item = result.trait_def_item(&segment.ident)?;
                        trait_def_item.add_item(trait_item, variants)?;
                    }
                }

                MetaItem::Type(type_fn_item) => {
                    let context = GenericsContext::WithGenerics(
                        &type_fn_item.generics,
                        &GenericsContext::Empty,
                    );
                    // TODO: make sure that expr does not reference Self
                    let ty = result.convert_type_level_expr_type(
                        type_fn_item.expr.clone(),
                        &context,
                        &type_fn_item.bounds,
                    )?;
                    let mut attrs = type_fn_item.attrs.clone();
                    // Note: We could strip type bounds instead of silencing the warning, but it
                    // would cause some IDE navigation and syntax highlighting to fail because the
                    // input spans would no longer be associated with anything in our output.
                    attrs.push(parse_quote!(#[allow(type_alias_bounds)]));
                    result.0.push(OutputMetaItem::Type(ItemType {
                        attrs,
                        vis: type_fn_item.vis.clone(),
                        type_token: type_fn_item.type_token.clone(),
                        ident: type_fn_item.ident.clone(),
                        generics: type_fn_item.generics.clone(),
                        eq_token: Default::default(),
                        ty: Box::new(ty),
                        semi_token: Default::default(),
                    }));
                }
            }
        }

        Ok(result)
    }

    fn check_trait_impl_params(
        impl_item_generics: &MetaGenerics,
        output_item_generics: &MetaGenerics,
    ) -> Result<()> {
        let impl_item_generics_tokens = impl_item_generics.to_token_stream();
        let output_item_generics_tokens = output_item_generics.to_token_stream();
        if impl_item_generics_tokens.to_string() != output_item_generics_tokens.to_string() {
            return Err(Error::new_spanned(
                &impl_item_generics,
                "trait impl generics must match trait generics",
            ));
        }
        Ok(())
    }

    fn check_trait_impl_args(
        impl_item_generics: &MetaGenerics,
        impl_item_args: &PathArguments,
    ) -> Result<()> {
        if impl_item_generics.params.is_empty() {
            if !impl_item_args.is_none() {
                return Err(Error::new_spanned(&impl_item_args, "no arguments expected"));
            }
        } else {
            let PathArguments::AngleBracketed(args) = impl_item_args else {
                return Err(Error::new_spanned(
                    impl_item_args,
                    "arguments in angle brackets expected",
                ));
            };
            let mut arg_iter = args.args.iter();
            for param in &impl_item_generics.params {
                let Some(arg) = arg_iter.next() else {
                    return Err(Error::new_spanned(args, "too few arguments"));
                };
                match param {
                    MetaGenericParam::Generic(generic) => match generic {
                        GenericParam::Lifetime(lifetime_param) => {
                            let GenericArgument::Lifetime(arg_lifetime) = arg else {
                                return Err(Error::new_spanned(arg, "lifetime expected"));
                            };
                            let lifetime = &lifetime_param.lifetime;
                            if arg_lifetime != lifetime {
                                return Err(Error::new_spanned(
                                    arg,
                                    format!("lifetime `{lifetime}` expected"),
                                ));
                            }
                        }
                        GenericParam::Type(type_param) => {
                            let GenericArgument::Type(Type::Path(arg_path)) = arg else {
                                return Err(Error::new_spanned(arg, "type reference expected"));
                            };
                            let type_ident = &type_param.ident;
                            if !arg_path.path.is_ident(type_ident) {
                                return Err(Error::new_spanned(
                                    arg,
                                    format!("type `{type_ident}` expected"),
                                ));
                            }
                        }
                        GenericParam::Const(const_param) => {
                            let GenericArgument::Const(Expr::Path(arg_path)) = arg else {
                                return Err(Error::new_spanned(arg, "const reference expected"));
                            };
                            let const_ident = &const_param.ident;
                            if !arg_path.path.is_ident(const_ident) {
                                return Err(Error::new_spanned(
                                    arg,
                                    format!("constant `{const_ident}` expected"),
                                ));
                            }
                        }
                    },
                    MetaGenericParam::TypeBound(type_bound_param) => {
                        let GenericArgument::Type(Type::Path(arg_path)) = arg else {
                            return Err(Error::new_spanned(arg, "type bound reference expected"));
                        };
                        let type_bound_ident = &type_bound_param.ident;
                        if !arg_path.path.is_ident(type_bound_ident) {
                            return Err(Error::new_spanned(
                                arg,
                                format!("type bound `{type_bound_ident}` expected"),
                            ));
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

pub enum MetaItem {
    TraitDef(ItemTraitDef),
    TraitImpl(ItemTraitImpl),
    Type(ItemTypeExt),
}

impl Parse for MetaItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ahead = input.fork();
        ahead.parse::<Visibility>()?;

        let mut lookahead = ahead.lookahead1();
        if lookahead.peek(Token![trait]) {
            ahead.parse::<Token![trait]>()?;
            lookahead = ahead.lookahead1();
            if lookahead.peek(Token![impl]) {
                return Ok(MetaItem::TraitImpl(ItemTraitImpl::parse(input, attrs)?));
            }
        } else if lookahead.peek(Token![enum]) {
            return Ok(MetaItem::TraitDef(ItemTraitDef::parse(input, attrs)?));
        } else if lookahead.peek(Token![type]) {
            return Ok(MetaItem::Type(ItemTypeExt::parse(input, attrs)?));
        }
        Err(lookahead.error())
    }
}

pub struct ItemTraitDef {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub trait_token: Token![trait],
    pub ident: Ident,
    pub generics: MetaGenerics,
    pub contents: TraitContents,
}

impl ItemTraitDef {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        input.parse::<Token![enum]>()?;
        let trait_token: Token![trait] = input.parse()?;
        let ident: Ident = input.parse()?;
        let generics: MetaGenerics = input.parse()?;
        let content;
        braced!(content in input);
        let variants = content.parse_terminated(TraitVariant::parse, Token![,])?;
        Ok(ItemTraitDef {
            attrs,
            vis,
            trait_token,
            ident,
            generics,
            contents: TraitContents::Enum { variants },
        })
    }
}

pub enum TraitContents {
    Enum {
        variants: Punctuated<TraitVariant, Token![,]>,
    },
}

#[derive(Clone)]
pub struct TraitVariant {
    pub attrs: Vec<Attribute>,
    pub ident: Ident,
    pub generics: Generics,
}

impl Parse for TraitVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ident: Ident = input.parse()?;
        let generics: Generics = input.parse()?;
        Ok(TraitVariant {
            attrs,
            ident,
            generics,
        })
    }
}

pub struct ItemTraitImpl {
    pub attrs: Vec<Attribute>,
    pub trait_token: Token![trait],
    pub impl_token: Token![impl],
    pub generics: MetaGenerics,
    pub self_trait: Path,
    pub items: Vec<TraitImplItem>,
}

impl ItemTraitImpl {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let trait_token: Token![trait] = input.parse()?;
        let impl_token: Token![impl] = input.parse()?;
        let generics: MetaGenerics = input.parse()?;
        let self_trait: Path = input.parse()?;
        let content;
        braced!(content in input);
        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }
        Ok(ItemTraitImpl {
            attrs,
            trait_token,
            impl_token,
            generics,
            self_trait,
            items,
        })
    }
}

pub struct ItemTypeExt {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub type_token: Token![type],
    pub ident: Ident,
    pub generics: Generics,
    pub bounds: TypeParamBounds,
    pub expr: TypeLevelExpr<Type>,
}

impl ItemTypeExt {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let type_token: Token![type] = input.parse()?;
        let ident: Ident = input.parse()?;
        let generics: Generics = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let bounds = if colon_token.is_some() {
            parse_type_param_bounds(input)?
        } else {
            Punctuated::new()
        };
        input.parse::<Token![=]>()?;
        let expr: TypeLevelExpr<Type> = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(ItemTypeExt {
            vis,
            attrs,
            type_token,
            ident,
            generics,
            bounds,
            expr,
        })
    }
}

#[derive(Clone)]
pub enum TraitImplItem {
    Const(TraitImplItemConst),
    Type(TraitImplItemType),
}

impl Parse for TraitImplItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ahead = input.fork();
        ahead.parse::<Visibility>()?;

        let lookahead = ahead.lookahead1();
        if lookahead.peek(Token![const]) {
            return Ok(TraitImplItem::Const(TraitImplItemConst::parse(
                input, attrs,
            )?));
        } else if lookahead.peek(Token![type]) {
            return Ok(TraitImplItem::Type(TraitImplItemType::parse(input, attrs)?));
        }
        Err(lookahead.error())
    }
}

#[derive(Clone)]
pub struct TraitImplItemConst {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub const_token: Token![const],
    pub ident: Ident,
    pub generics: Generics,
    pub ty: Type,
    pub expr: TypeLevelExpr<Expr>,
}

impl TraitImplItemConst {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let const_token: Token![const] = input.parse()?;
        let ident: Ident = input.parse()?;
        let generics: Generics = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;
        input.parse::<Token![=]>()?;
        let expr: TypeLevelExpr<Expr> = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(TraitImplItemConst {
            vis,
            attrs,
            const_token,
            ident,
            generics,
            ty,
            expr,
        })
    }
}

#[derive(Clone)]
pub struct TraitImplItemType {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub type_token: Token![type],
    pub ident: Ident,
    pub generics: Generics,
    pub bounds: TypeParamBounds,
    pub expr: TypeLevelExpr<Type>,
}

impl TraitImplItemType {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let type_token: Token![type] = input.parse()?;
        let ident: Ident = input.parse()?;
        let generics: Generics = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let bounds = if colon_token.is_some() {
            parse_type_param_bounds(input)?
        } else {
            Punctuated::new()
        };
        input.parse::<Token![=]>()?;
        let expr: TypeLevelExpr<Type> = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(TraitImplItemType {
            vis,
            attrs,
            type_token,
            ident,
            generics,
            bounds,
            expr,
        })
    }
}
