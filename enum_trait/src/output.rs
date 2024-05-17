use std::borrow::Cow;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{punctuated::Punctuated, spanned::Spanned, *};

use crate::{expr::*, generics::*, item::*, subst::*};

pub struct OutputMetaItemList<'a>(pub Vec<OutputMetaItem<'a>>);

impl<'a> OutputMetaItemList<'a> {
    pub fn new() -> Self {
        OutputMetaItemList(Vec::new())
    }

    pub fn trait_def_item(&mut self, ident: &Ident) -> Result<&mut OutputItemTraitDef<'a>> {
        let Some(trait_def_item) = self.0.iter_mut().find_map(|output_item| {
            if let OutputMetaItem::TraitDef(trait_def_item) = output_item {
                if &trait_def_item.trait_item.ident == ident {
                    return Some(trait_def_item);
                }
            }
            None
        }) else {
            return Err(Error::new_spanned(
                &ident,
                format!("trait `{ident}` not defined in this meta block"),
            ));
        };
        Ok(trait_def_item)
    }

    pub fn create_trait_item(
        &mut self,
        item: TraitImplItem,
        context: &GenericsContext,
    ) -> Result<(TraitItem, Option<(Vec<OutputTraitVariant>, Span)>)> {
        // TODO: Prefix non-public items with underscores.
        match item {
            TraitImplItem::Const(const_fn_item) => {
                let mut trait_item = TraitItemConst {
                    attrs: const_fn_item.attrs.clone(),
                    const_token: const_fn_item.const_token.clone(),
                    ident: const_fn_item.ident.clone(),
                    generics: const_fn_item.generics.clone(),
                    colon_token: Default::default(),
                    ty: const_fn_item.ty.clone(),
                    default: None,
                    semi_token: Default::default(),
                };
                let mut variants = None;
                match const_fn_item.expr {
                    TypeLevelExpr::Expr(expr) => {
                        trait_item.default = Some((Default::default(), expr));
                    }
                    TypeLevelExpr::Match(match_expr) => {
                        let Type::Path(TypePath { qself: None, path }) = &match_expr.ty else {
                            panic!("todo: const fn not canonicalized (expected path)");
                        };
                        assert!(
                            path.is_ident(SELF_TYPE_NAME),
                            "todo: const fn not canonicalized (expected Self)"
                        );
                        let variants_span = match_expr.span();
                        let variants_impls = match_expr
                            .arms
                            .into_iter()
                            .map(|arm| {
                                let TypeLevelExpr::Expr(expr) = arm.body else {
                                    panic!("todo: const fn not canonicalized (expected expr body)");
                                };
                                let impl_item = ImplItemConst {
                                    attrs: const_fn_item.attrs.clone(),
                                    vis: Visibility::Inherited,
                                    defaultness: None,
                                    const_token: const_fn_item.const_token.clone(),
                                    ident: const_fn_item.ident.clone(),
                                    generics: const_fn_item.generics.clone(),
                                    colon_token: Default::default(),
                                    ty: const_fn_item.ty.clone(),
                                    eq_token: Default::default(),
                                    expr,
                                    semi_token: Default::default(),
                                };
                                OutputTraitVariant {
                                    variant: TraitVariant {
                                        attrs: Vec::new(),
                                        ident: arm.variant_ident,
                                        generics: arm.variant_generics,
                                    },
                                    impl_items: vec![ImplItem::Const(impl_item)],
                                }
                            })
                            .collect();
                        variants = Some((variants_impls, variants_span));
                    }
                };
                Ok((TraitItem::Const(trait_item), variants))
            }
            TraitImplItem::Type(type_fn_item) => {
                let mut trait_item = TraitItemType {
                    attrs: type_fn_item.attrs.clone(),
                    type_token: type_fn_item.type_token.clone(),
                    ident: type_fn_item.ident.clone(),
                    generics: type_fn_item.generics.clone(),
                    colon_token: Default::default(),
                    bounds: type_fn_item.bounds.clone(),
                    default: None,
                    semi_token: Default::default(),
                };
                let mut variants = None;
                match type_fn_item.expr {
                    TypeLevelExpr::Expr(ty) => {
                        // TODO: Don't do this if we know variants, as it is unstable.
                        trait_item.default = Some((Default::default(), ty));
                    }
                    TypeLevelExpr::Match(match_expr) => {
                        let Type::Path(TypePath { qself: None, path }) = &match_expr.ty else {
                            panic!("todo: type fn not canonicalized (expected path)");
                        };
                        assert!(
                            path.is_ident(SELF_TYPE_NAME),
                            "todo: type fn not canonicalized (expected Self)"
                        );
                        let item_context =
                            GenericsContext::WithGenerics(&type_fn_item.generics, &context);
                        let variants_span = match_expr.span();
                        let variants_impls = match_expr
                            .arms
                            .into_iter()
                            .map(|arm| {
                                let body_context = GenericsContext::WithGenerics(
                                    &arm.variant_generics,
                                    &item_context,
                                );
                                let ty = self.convert_type_level_expr_type(
                                    arm.body,
                                    &body_context,
                                    &type_fn_item.bounds,
                                )?;
                                let impl_item = ImplItemType {
                                    attrs: type_fn_item.attrs.clone(),
                                    vis: Visibility::Inherited,
                                    defaultness: None,
                                    type_token: type_fn_item.type_token.clone(),
                                    ident: type_fn_item.ident.clone(),
                                    generics: type_fn_item.generics.clone(),
                                    eq_token: Default::default(),
                                    ty,
                                    semi_token: Default::default(),
                                };
                                Ok(OutputTraitVariant {
                                    variant: TraitVariant {
                                        attrs: Vec::new(),
                                        ident: arm.variant_ident,
                                        generics: arm.variant_generics,
                                    },
                                    impl_items: vec![ImplItem::Type(impl_item)],
                                })
                            })
                            .collect::<Result<_>>()?;
                        variants = Some((variants_impls, variants_span));
                    }
                };
                Ok((TraitItem::Type(trait_item), variants))
            }
        }
    }

    pub fn convert_type_level_expr_type(
        &mut self,
        expr: TypeLevelExpr<Type>,
        context: &GenericsContext,
        return_bounds: &TypeParamBounds,
    ) -> Result<Type> {
        match expr {
            TypeLevelExpr::Expr(expr) => Ok(expr),
            TypeLevelExpr::Match(match_expr) => {
                self.convert_type_level_match_expr_type(match_expr, context, return_bounds)
            }
        }
    }

    pub fn convert_type_level_match_expr_type(
        &mut self,
        mut match_expr: TypeLevelExprMatch<TypeLevelExpr<Type>>,
        context: &GenericsContext,
        return_bounds: &TypeParamBounds,
    ) -> Result<Type> {
        let ty = match_expr.ty.clone();
        let Some(match_ident) = Self::get_type_ident(&ty) else {
            return Err(Error::new(
                ty.span(),
                "type matching is currently only supported on variables",
            ));
        };
        let (match_param, generics, arguments) =
            isolate_type_param(&mut match_expr, context, match_ident)?;
        let Some(TypeParamBound::Trait(trait_bound)) = match_param.bounds.first() else {
            return Err(Error::new(
                ty.span(),
                "no appropriate type bound for matching found",
            ));
        };
        let Some(trait_ident) = trait_bound.path.get_ident() else {
            return Err(Error::new(
                ty.span(),
                "matching is currently only supported for plain traits",
            ));
        };
        let trait_def_item = self.trait_def_item(trait_ident)?;
        let impl_context = trait_def_item.impl_context();
        let type_fn_ident = Ident::new(
            &format!("__{}", trait_def_item.next_internal_item_idx),
            match_ident.span(),
        );
        trait_def_item.next_internal_item_idx += 1;
        let (trait_item, variants) = self.create_trait_item(
            TraitImplItem::Type(TraitImplItemType {
                attrs: Vec::new(),
                vis: Visibility::Inherited,
                type_token: Default::default(),
                ident: type_fn_ident.clone(),
                generics,
                bounds: return_bounds.clone(),
                expr: TypeLevelExpr::Match(match_expr),
            }),
            &impl_context,
        )?;
        let trait_def_item = self.trait_def_item(trait_ident)?;
        trait_def_item.add_item(trait_item, variants)?;
        let mut segments = trait_bound.path.segments.clone();
        segments.push(PathSegment {
            ident: type_fn_ident,
            arguments,
        });
        Ok(Type::Path(TypePath {
            qself: Some(QSelf {
                lt_token: Default::default(),
                ty: Box::new(ty),
                position: segments.len() - 1,
                as_token: Some(Default::default()),
                gt_token: Default::default(),
            }),
            path: Path {
                leading_colon: trait_bound.path.leading_colon.clone(),
                segments,
            },
        }))
    }

    fn get_type_ident(ty: &Type) -> Option<&Ident> {
        let Type::Path(type_path) = ty else {
            return None;
        };
        if type_path.qself.is_some() {
            return None;
        }
        type_path.path.get_ident()
    }
}

impl ToTokens for OutputMetaItemList<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for item in &self.0 {
            match item {
                OutputMetaItem::TraitDef(enum_item) => enum_item.to_tokens(tokens),
                OutputMetaItem::Type(type_fn_item) => type_fn_item.to_tokens(tokens),
            }
        }
    }
}

pub enum OutputMetaItem<'a> {
    TraitDef(OutputItemTraitDef<'a>),
    Type(ItemType),
}

pub struct OutputItemTraitDef<'a> {
    pub trait_item: &'a ItemTraitDef,
    pub variants: Option<Vec<OutputTraitVariant>>,
    pub impl_items: Vec<TraitItem>,
    pub next_internal_item_idx: usize,
}

impl<'a> OutputItemTraitDef<'a> {
    pub fn add_item(
        &mut self,
        item: TraitItem,
        variants: Option<(Vec<OutputTraitVariant>, Span)>,
    ) -> Result<()> {
        self.impl_items.push(item);
        if let Some((variants, variants_span)) = variants {
            if let Some(existing_variants) = &mut self.variants {
                let expected_len = existing_variants.len();
                if variants.len() != expected_len {
                    return Err(Error::new(
                        variants_span,
                        format!("expected exactly {expected_len} variants"),
                    ));
                }
                for (existing_variant, variant) in
                    existing_variants.iter_mut().zip(variants.into_iter())
                {
                    if existing_variant.variant.ident != variant.variant.ident {
                        return Err(Error::new(
                            variant.variant.ident.span(),
                            format!("expected variant {}", &existing_variant.variant.ident),
                        ));
                    }
                    // TODO: check generics
                    existing_variant
                        .impl_items
                        .reserve(variant.impl_items.len());
                    for mut impl_item in variant.impl_items {
                        // TODO: prevent implementation from referencing enum item generics instead
                        // of arm generics
                        impl_item.substitute_all_params(
                            &variant.variant.generics,
                            &existing_variant.variant.generics,
                        )?;
                        existing_variant.impl_items.push(impl_item);
                    }
                }
            } else {
                self.variants = Some(variants);
            }
        }
        Ok(())
    }

    pub fn impl_context(&self) -> GenericsContext<'a> {
        let mut bounds = Punctuated::new();
        bounds.push(TypeParamBound::Trait(TraitBound {
            paren_token: None,
            modifier: TraitBoundModifier::None,
            lifetimes: None,
            path: self.trait_item.ident.clone().into(),
        }));
        GenericsContext::WithSelf(
            Cow::Owned(self_type_param(Span::call_site(), bounds)),
            &GenericsContext::Empty,
        )
    }
}

impl ToTokens for OutputItemTraitDef<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let trait_generics = self.trait_item.generics.extract_generics();
        let trait_generics_vec: Vec<TokenStream> = trait_generics
            .params
            .iter()
            .map(ToTokens::to_token_stream)
            .collect();
        let trait_item = ItemTrait {
            attrs: self.trait_item.attrs.clone(),
            vis: self.trait_item.vis.clone(),
            unsafety: None,
            auto_token: None,
            restriction: None,
            trait_token: self.trait_item.trait_token.clone(),
            ident: self.trait_item.ident.clone(),
            generics: trait_generics.clone(),
            colon_token: None,
            supertraits: parse_quote!(Sized),
            brace_token: Default::default(),
            items: self.impl_items.clone(),
        };
        trait_item.to_tokens(tokens);
        let mut macro_params = quote!($_Name:ident, $($_ref_path:ident::)*, );
        let mut macro_body = TokenStream::new();
        for trait_param in &self.trait_item.generics.params {
            if let MetaGenericParam::TypeBound(type_bound_param) = trait_param {
                let ident = &type_bound_param.ident;
                macro_params.append_all(quote!(($($#ident:tt)+), ));
            }
        }
        if let Some(variants) = &self.variants {
            for output_variant in variants {
                let variant = &output_variant.variant;
                let ident = &variant.ident;
                let mut variant_generics = variant.generics.clone();
                self.trait_item
                    .generics
                    .eliminate_in_generics(&mut variant_generics);
                let mut macro_generic_params = Vec::new();
                let mut impl_generic_params = trait_generics_vec.clone();
                let mut impl_generic_args = Vec::new();
                let param_prefix = format!("{ident}_");
                for param in &variant_generics.params {
                    match param {
                        GenericParam::Lifetime(LifetimeParam {
                            attrs: _,
                            lifetime,
                            colon_token,
                            bounds,
                        }) => {
                            let ident = ident_with_prefix(&lifetime.ident, &param_prefix);
                            macro_generic_params.push(quote!($#ident:lifetime));
                            impl_generic_params.push(quote!($#ident #colon_token #bounds));
                            impl_generic_args.push(quote!($#ident));
                        }
                        GenericParam::Type(TypeParam {
                            attrs: _,
                            ident,
                            colon_token,
                            bounds,
                            eq_token,
                            default,
                        }) => {
                            let ident = ident_with_prefix(ident, &param_prefix);
                            macro_generic_params.push(quote!($#ident:ident));
                            // TODO: substitute own trait with $_Name in bounds
                            impl_generic_params
                                .push(quote!($#ident #colon_token #bounds #eq_token #default));
                            impl_generic_args.push(quote!($#ident));
                        }
                        GenericParam::Const(ConstParam {
                            attrs: _,
                            const_token,
                            ident,
                            colon_token,
                            ty,
                            eq_token,
                            default,
                        }) => {
                            let ident = ident_with_prefix(ident, &param_prefix);
                            macro_generic_params.push(quote!($#ident:ident));
                            impl_generic_params.push(
                                quote!(#const_token $#ident #colon_token #ty #eq_token #default),
                            );
                            impl_generic_args.push(quote!($#ident));
                        }
                    }
                }
                let mut macro_generics = TokenStream::new();
                if !macro_generic_params.is_empty() {
                    macro_generics.append_all(variant.generics.lt_token);
                    macro_generics.append_separated(macro_generic_params, <Token![,]>::default());
                    macro_generics.append_all(quote!($(,)?));
                    macro_generics.append_all(variant.generics.gt_token);
                }
                let body_ident = ident_with_suffix(ident, "__Body");
                macro_params.append_all(quote!(#ident #macro_generics => $#body_ident:tt));
                let mut impl_generics = TokenStream::new();
                if !impl_generic_params.is_empty() {
                    impl_generics.append_all(
                        variant
                            .generics
                            .lt_token
                            .or(self.trait_item.generics.lt_token),
                    );
                    impl_generics.append_separated(impl_generic_params, <Token![,]>::default());
                    impl_generics.append_all(
                        variant
                            .generics
                            .gt_token
                            .or(self.trait_item.generics.gt_token),
                    );
                }
                let mut impl_args = TokenStream::new();
                if !impl_generic_args.is_empty() {
                    impl_args.append_all(variant.generics.lt_token);
                    impl_args.append_separated(impl_generic_args, <Token![,]>::default());
                    impl_args.append_all(variant.generics.gt_token);
                }
                macro_body.append_all(
                    quote!(impl #impl_generics $_Name for $($_ref_path::)*#ident #impl_args $#body_ident),
                );
            }
        } else {
            // TODO
        }
        let macro_ident = ident_with_prefix(&self.trait_item.ident, "__trait_impl__");
        // TODO: Add support for trait aliases where variants are not known.
        tokens.append_all(quote!(macro_rules! #macro_ident {
            (#macro_params) => { #macro_body }
        }));

        let ident = &self.trait_item.ident;

        let mut impl_args = quote!(#ident, , );
        for trait_param in &self.trait_item.generics.params {
            if let MetaGenericParam::TypeBound(type_bound_param) = trait_param {
                let bounds = &type_bound_param.bounds;
                impl_args.append_all(quote!((#bounds), ));
            }
        }

        if let Some(variants) = &self.variants {
            for output_variant in variants {
                let variant = &output_variant.variant;
                let mut variant_generics = variant.generics.clone();
                self.trait_item
                    .generics
                    .eliminate_in_generics(&mut variant_generics);
                let struct_item = ItemStruct {
                    attrs: variant.attrs.clone(),
                    vis: self.trait_item.vis.clone(),
                    struct_token: Default::default(),
                    ident: variant.ident.clone(),
                    generics: variant_generics,
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: phantom_types(&variant.generics),
                    }),
                    semi_token: Some(Default::default()),
                };
                struct_item.to_tokens(tokens);

                variant.ident.to_tokens(&mut impl_args);
                let mut generic_params = Vec::new();
                for param in &variant.generics.params {
                    match param {
                        GenericParam::Lifetime(lifetime_param) => {
                            generic_params.push(lifetime_param.lifetime.to_token_stream());
                        }
                        GenericParam::Type(type_param) => {
                            generic_params.push(type_param.ident.to_token_stream());
                        }
                        GenericParam::Const(const_param) => {
                            generic_params.push(const_param.ident.to_token_stream());
                        }
                    }
                }
                if !generic_params.is_empty() {
                    impl_args.append_all(variant.generics.lt_token);
                    impl_args.append_separated(generic_params, <Token![,]>::default());
                    impl_args.append_all(variant.generics.gt_token);
                }
                <Token![=>]>::default().to_tokens(&mut impl_args);

                token::Brace::default().surround(&mut impl_args, |body| {
                    for impl_item in &output_variant.impl_items {
                        impl_item.to_tokens(body);
                    }
                });
            }
        } else {
            // TODO
        }

        tokens.append_all(quote!(#macro_ident!(#impl_args);));
    }
}

pub struct OutputTraitVariant {
    pub variant: TraitVariant,
    pub impl_items: Vec<ImplItem>,
}

fn phantom_types(generics: &Generics) -> Punctuated<Field, Token![,]> {
    let mut types = Punctuated::new();
    types.push(parse_quote!(()));
    for param in &generics.params {
        match param {
            GenericParam::Type(type_param) => {
                let name = &type_param.ident;
                types.push(parse_quote!(core::marker::PhantomData<#name>));
            }
            _ => {}
        }
    }
    types
}
