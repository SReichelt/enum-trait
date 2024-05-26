use syn::{
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    *,
};

use crate::{expr::*, generics::*, helpers::*, output::*, subst::*};

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
            if let MetaItem::TraitDef(trait_def) = item {
                let variants = if let TraitContents::Enum { variants } = &trait_def.contents {
                    let mut trait_generics = trait_def.generics.extract_generics();
                    add_underscores_to_all_params(&mut trait_generics)?;
                    Some(
                        variants
                            .iter()
                            .map(|variant| {
                                let mut variant = variant.clone();
                                add_underscores_to_all_params(&mut variant.generics)?;
                                Ok(OutputImplVariant {
                                    variant: ImplVariant {
                                        impl_generics: trait_generics.clone(),
                                        trait_args: generic_args(&trait_generics),
                                        variant,
                                    },
                                    impl_items: Vec::new(),
                                })
                            })
                            .collect::<Result<_>>()?,
                    )
                } else {
                    None
                };
                result.0.push(OutputMetaItem::TraitDef(OutputItemTraitDef {
                    trait_def,
                    variants,
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
                    let trait_def = trait_def_item.trait_def;
                    check_token_equality(&impl_item.generics, &trait_def.generics)?;
                    Self::check_trait_impl_args(&impl_item.generics, &segment.arguments)?;
                    let impl_context = trait_def_item.impl_context();
                    let variants_known = trait_def_item.variants.is_some();
                    for item in &impl_item.items {
                        let (trait_item, variants) = result.create_trait_item(
                            item.clone(),
                            &impl_context,
                            trait_def,
                            variants_known,
                        )?;
                        let trait_def_item = result.trait_def_item(&segment.ident)?;
                        trait_def_item.add_item(trait_item, variants)?;
                    }
                }

                MetaItem::Type(type_item) => {
                    let context =
                        GenericsContext::WithGenerics(&type_item.generics, &GenericsContext::Empty);
                    let ty = result.convert_type_level_expr_type(
                        type_item.ty.clone(),
                        &context,
                        &type_item.bounds,
                    )?;
                    let mut attrs = OutputMetaItemList::code_item_attrs(type_item.attrs.clone());
                    // Note: We could strip type bounds instead of silencing the warning, but it
                    // would cause some IDE navigation and syntax highlighting to fail because the
                    // input spans would no longer be associated with anything in our output.
                    attrs.push(parse_quote!(#[allow(type_alias_bounds)]));
                    result.0.push(OutputMetaItem::Item(Item::Type(ItemType {
                        attrs,
                        vis: type_item.vis.clone(),
                        type_token: type_item.type_token.clone(),
                        ident: type_item.ident.clone(),
                        generics: type_item.generics.clone(),
                        eq_token: Default::default(),
                        ty: Box::new(ty),
                        semi_token: Default::default(),
                    })));
                }

                MetaItem::Fn(fn_item) => {
                    let context = GenericsContext::WithGenerics(
                        &fn_item.sig.generics,
                        &GenericsContext::Empty,
                    );
                    let expr = result.convert_type_level_expr_fn(
                        fn_item.expr.clone(),
                        &context,
                        &fn_item.sig,
                    )?;
                    result.0.push(OutputMetaItem::Item(Item::Fn(ItemFn {
                        attrs: OutputMetaItemList::code_item_attrs(fn_item.attrs.clone()),
                        vis: fn_item.vis.clone(),
                        sig: fn_item.sig.clone(),
                        block: Box::new(Block {
                            brace_token: Default::default(),
                            stmts: vec![Stmt::Expr(expr, None)],
                        }),
                    })));
                }
            }
        }

        for item in &mut result.0 {
            if let OutputMetaItem::TraitDef(trait_def_item) = item {
                if trait_def_item.variants.is_none() {
                    let trait_def = trait_def_item.trait_def;
                    if let Some(where_clause) = &trait_def.generics.where_clause {
                        return Err(Error::new(
                            where_clause.span(),
                            "at least one `match` expression corresponding to `where` clause required",
                        ));
                    }
                    if let TraitContents::Alias { path } = &trait_def.contents {
                        if has_complex_type_arg(path) {
                            return Err(Error::new(
                                path.span(),
                                "at least one `match` expression corresponding to alias arguments required",
                            ));
                        }
                    }
                }
            }
        }

        Ok(result)
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
                            let GenericArgument::Type(arg_ty) = arg else {
                                return Err(Error::new_spanned(arg, "variable expected"));
                            };
                            let type_ident = &type_param.ident;
                            if !type_is_ident(arg_ty, type_ident) {
                                return Err(Error::new_spanned(
                                    arg,
                                    format!("type `{type_ident}` expected"),
                                ));
                            }
                        }
                        GenericParam::Const(const_param) => {
                            let GenericArgument::Type(arg_ty) = arg else {
                                return Err(Error::new_spanned(arg, "variable expected"));
                            };
                            let const_ident = &const_param.ident;
                            if !type_is_ident(arg_ty, const_ident) {
                                return Err(Error::new_spanned(
                                    arg,
                                    format!("constant `{const_ident}` expected"),
                                ));
                            }
                        }
                    },
                    MetaGenericParam::TypeBound(type_bound_param) => {
                        let GenericArgument::Type(arg_ty) = arg else {
                            return Err(Error::new_spanned(arg, "variable expected"));
                        };
                        let type_bound_ident = &type_bound_param.ident;
                        if !type_is_ident(arg_ty, type_bound_ident) {
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
    Fn(ItemFnExt),
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
            } else {
                return Ok(MetaItem::TraitDef(ItemTraitDef::parse(input, attrs)?));
            }
        } else if lookahead.peek(Token![enum]) {
            return Ok(MetaItem::TraitDef(ItemTraitDef::parse(input, attrs)?));
        } else if lookahead.peek(Token![type]) {
            return Ok(MetaItem::Type(ItemTypeExt::parse(input, attrs)?));
        } else if lookahead.peek(Token![const]) {
            ahead.parse::<Token![const]>()?;
            lookahead = ahead.lookahead1();
            if lookahead.peek(Token![fn]) {
                return Ok(MetaItem::Fn(ItemFnExt::parse(input, attrs)?));
            }
        } else if lookahead.peek(Token![fn]) {
            return Ok(MetaItem::Fn(ItemFnExt::parse(input, attrs)?));
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
        let enum_token = input.parse::<Option<Token![enum]>>()?;
        let trait_token: Token![trait] = input.parse()?;
        let ident: Ident = input.parse()?;
        let mut generics: MetaGenerics = input.parse()?;
        let contents = if enum_token.is_some() {
            if input.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
            };
            let content: ParseBuffer;
            braced!(content in input);
            let variants = content.parse_terminated(TraitVariant::parse, Token![,])?;
            TraitContents::Enum { variants }
        } else {
            input.parse::<Token![=]>()?;
            let path: Path = input.parse()?;
            if input.peek(Token![where]) {
                generics.where_clause = Some(input.parse()?);
            };
            input.parse::<Token![;]>()?;
            TraitContents::Alias { path }
        };
        Ok(ItemTraitDef {
            attrs,
            vis,
            trait_token,
            ident,
            generics,
            contents,
        })
    }
}

pub enum TraitContents {
    Enum {
        variants: Punctuated<TraitVariant, Token![,]>,
    },
    Alias {
        path: Path,
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
    pub generics: MetaGenerics,
    pub self_trait: Path,
    pub items: Vec<TraitImplItem>,
}

impl ItemTraitImpl {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        if let Some(first_attr) = attrs.first() {
            return Err(Error::new(
                first_attr.span(),
                "trait impl attributes are not supported",
            ));
        }
        input.parse::<Token![trait]>()?;
        input.parse::<Token![impl]>()?;
        let generics: MetaGenerics = input.parse()?;
        let self_trait: Path = input.parse()?;
        let content: ParseBuffer;
        braced!(content in input);
        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }
        Ok(ItemTraitImpl {
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
    pub ty: TypeLevelExpr<Type>,
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
        let ty: TypeLevelExpr<Type> = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(ItemTypeExt {
            attrs,
            vis,
            type_token,
            ident,
            generics,
            bounds,
            ty,
        })
    }
}

pub struct ItemFnExt {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub sig: Signature,
    pub expr: TypeLevelExpr<Expr>,
}

impl ItemFnExt {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let sig: Signature = input.parse()?;
        let content: ParseBuffer;
        braced!(content in input);
        let expr: TypeLevelExpr<Expr> = content.parse()?;
        Ok(ItemFnExt {
            attrs,
            vis,
            sig,
            expr,
        })
    }
}

#[derive(Clone)]
pub enum TraitImplItem {
    Const(TraitImplItemConst),
    Type(TraitImplItemType),
    Fn(TraitImplItemFn),
}

impl Parse for TraitImplItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let ahead = input.fork();
        ahead.parse::<Visibility>()?;

        let mut lookahead = ahead.lookahead1();
        if lookahead.peek(Token![type]) {
            return Ok(TraitImplItem::Type(TraitImplItemType::parse(input, attrs)?));
        } else if lookahead.peek(Token![const]) {
            ahead.parse::<Token![const]>()?;
            lookahead = ahead.lookahead1();
            if lookahead.peek(Token![fn]) {
                return Ok(TraitImplItem::Fn(TraitImplItemFn::parse(input, attrs)?));
            } else {
                return Ok(TraitImplItem::Const(TraitImplItemConst::parse(
                    input, attrs,
                )?));
            }
        } else if lookahead.peek(Token![fn]) {
            return Ok(TraitImplItem::Fn(TraitImplItemFn::parse(input, attrs)?));
        }
        Err(lookahead.error())
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
    pub ty: TypeLevelExpr<Type>,
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
        let ty: TypeLevelExpr<Type> = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(TraitImplItemType {
            vis,
            attrs,
            type_token,
            ident,
            generics,
            bounds,
            ty,
        })
    }
}

#[derive(Clone)]
pub struct TraitImplItemConst {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub const_token: Token![const],
    pub ident: Ident,
    pub ty: Type,
    pub expr: TypeLevelExpr<Expr>,
}

impl TraitImplItemConst {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let const_token: Token![const] = input.parse()?;
        let ident: Ident = input.parse()?;
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
            ty,
            expr,
        })
    }
}

#[derive(Clone)]
pub struct TraitImplItemFn {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub sig: Signature,
    pub expr: TypeLevelExpr<Expr>,
}

impl TraitImplItemFn {
    fn parse(input: ParseStream, attrs: Vec<Attribute>) -> Result<Self> {
        let vis: Visibility = input.parse()?;
        let sig: Signature = input.parse()?;
        let content: ParseBuffer;
        braced!(content in input);
        let expr: TypeLevelExpr<Expr> = content.parse()?;
        Ok(TraitImplItemFn {
            attrs,
            vis,
            sig,
            expr,
        })
    }
}
