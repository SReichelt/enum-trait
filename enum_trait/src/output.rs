use std::{borrow::Cow, mem::take};

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
        variants_known: bool,
    ) -> Result<(
        TraitItem,
        Option<(Vec<(Option<TraitVariant>, ImplItem)>, Span)>,
    )> {
        match item {
            TraitImplItem::Type(type_item) => {
                let mut trait_item = TraitItemType {
                    attrs: Self::trait_item_attrs(type_item.attrs.clone(), &type_item.vis),
                    type_token: type_item.type_token.clone(),
                    ident: type_item.ident.clone(),
                    generics: type_item.generics.clone(),
                    colon_token: Default::default(),
                    bounds: type_item.bounds.clone(),
                    default: None,
                    semi_token: Default::default(),
                };
                let item_context = GenericsContext::WithGenerics(&type_item.generics, &context);
                let mut variants = None;
                let mut expr = Some(type_item.ty);
                if let Some(match_expr) = Self::get_self_match(&mut expr) {
                    let variants_span = match_expr.span();
                    let variants_impls = match_expr
                        .arms
                        .into_iter()
                        .map(|mut arm| {
                            rename_conflicting_params(
                                &mut arm.variant_generics,
                                |param| Ok(param_context_name_conflict(param, &item_context)),
                                |subst| arm.body.substitute_impl(subst),
                            )?;
                            let body_context =
                                GenericsContext::WithGenerics(&arm.variant_generics, &item_context);
                            let ty = self.convert_type_level_expr_type(
                                arm.body,
                                &body_context,
                                &type_item.bounds,
                            )?;
                            let impl_item = ImplItemType {
                                attrs: Self::code_item_attrs(type_item.attrs.clone()),
                                vis: Visibility::Inherited,
                                defaultness: None,
                                type_token: type_item.type_token.clone(),
                                ident: type_item.ident.clone(),
                                generics: type_item.generics.clone(),
                                eq_token: Default::default(),
                                ty,
                                semi_token: Default::default(),
                            };
                            Ok((
                                Some(TraitVariant {
                                    attrs: Vec::new(),
                                    ident: arm.variant_ident,
                                    generics: arm.variant_generics,
                                }),
                                ImplItem::Type(impl_item),
                            ))
                        })
                        .collect::<Result<_>>()?;
                    variants = Some((variants_impls, variants_span));
                } else {
                    let ty = self.convert_type_level_expr_type(
                        expr.unwrap(),
                        &item_context,
                        &type_item.bounds,
                    )?;
                    if variants_known {
                        // Prefer individual impls over default in trait because the latter is
                        // currently unstable.
                        let impl_item = ImplItemType {
                            attrs: Self::code_item_attrs(type_item.attrs.clone()),
                            vis: Visibility::Inherited,
                            defaultness: None,
                            type_token: type_item.type_token.clone(),
                            ident: type_item.ident.clone(),
                            generics: type_item.generics.clone(),
                            eq_token: Default::default(),
                            ty: ty.clone(),
                            semi_token: Default::default(),
                        };
                        variants = Some((vec![(None, ImplItem::Type(impl_item))], ty.span()));
                    } else {
                        trait_item.default = Some((Default::default(), ty));
                    }
                }
                Ok((TraitItem::Type(trait_item), variants))
            }

            TraitImplItem::Const(const_item) => {
                let mut trait_item = TraitItemConst {
                    attrs: Self::trait_item_attrs(const_item.attrs.clone(), &const_item.vis),
                    const_token: const_item.const_token.clone(),
                    ident: const_item.ident.clone(),
                    generics: Default::default(),
                    colon_token: Default::default(),
                    ty: const_item.ty.clone(),
                    default: None,
                    semi_token: Default::default(),
                };
                let mut variants = None;
                let mut expr = Some(const_item.expr);
                if let Some(match_expr) = Self::get_self_match(&mut expr) {
                    let variants_span = match_expr.span();
                    let variants_impls = match_expr
                        .arms
                        .into_iter()
                        .map(|mut arm| {
                            rename_conflicting_params(
                                &mut arm.variant_generics,
                                |param| Ok(param_context_name_conflict(param, &context)),
                                |subst| arm.body.substitute_impl(subst),
                            )?;
                            let body_context =
                                GenericsContext::WithGenerics(&arm.variant_generics, &context);
                            let expr = self.convert_type_level_expr_const(
                                arm.body,
                                &body_context,
                                &const_item.ty,
                            )?;
                            let impl_item = ImplItemConst {
                                attrs: Self::code_item_attrs(const_item.attrs.clone()),
                                vis: Visibility::Inherited,
                                defaultness: None,
                                const_token: const_item.const_token.clone(),
                                ident: const_item.ident.clone(),
                                generics: Default::default(),
                                colon_token: Default::default(),
                                ty: const_item.ty.clone(),
                                eq_token: Default::default(),
                                expr,
                                semi_token: Default::default(),
                            };
                            Ok((
                                Some(TraitVariant {
                                    attrs: Vec::new(),
                                    ident: arm.variant_ident,
                                    generics: arm.variant_generics,
                                }),
                                ImplItem::Const(impl_item),
                            ))
                        })
                        .collect::<Result<_>>()?;
                    variants = Some((variants_impls, variants_span));
                } else {
                    let expr =
                        self.convert_type_level_expr_const(expr.unwrap(), context, &const_item.ty)?;
                    trait_item.default = Some((Default::default(), expr));
                }
                Ok((TraitItem::Const(trait_item), variants))
            }

            TraitImplItem::Fn(fn_item) => {
                let mut trait_item = TraitItemFn {
                    attrs: Self::trait_item_attrs(fn_item.attrs.clone(), &fn_item.vis),
                    sig: fn_item.sig.clone(),
                    default: None,
                    semi_token: Default::default(),
                };
                let item_context = GenericsContext::WithGenerics(&fn_item.sig.generics, &context);
                let mut variants = None;
                let mut expr = Some(fn_item.expr);
                if let Some(match_expr) = Self::get_self_match(&mut expr) {
                    let variants_span = match_expr.span();
                    let variants_impls = match_expr
                        .arms
                        .into_iter()
                        .map(|mut arm| {
                            rename_conflicting_params(
                                &mut arm.variant_generics,
                                |param| Ok(param_context_name_conflict(param, &item_context)),
                                |subst| arm.body.substitute_impl(subst),
                            )?;
                            let body_context =
                                GenericsContext::WithGenerics(&arm.variant_generics, &item_context);
                            let expr = self.convert_type_level_expr_fn(
                                arm.body,
                                &body_context,
                                &fn_item.sig,
                            )?;
                            let impl_item = ImplItemFn {
                                attrs: Self::code_item_attrs(fn_item.attrs.clone()),
                                vis: Visibility::Inherited,
                                defaultness: None,
                                sig: fn_item.sig.clone(),
                                block: Block {
                                    brace_token: Default::default(),
                                    stmts: vec![Stmt::Expr(expr, None)],
                                },
                            };
                            Ok((
                                Some(TraitVariant {
                                    attrs: Vec::new(),
                                    ident: arm.variant_ident,
                                    generics: arm.variant_generics,
                                }),
                                ImplItem::Fn(impl_item),
                            ))
                        })
                        .collect::<Result<_>>()?;
                    variants = Some((variants_impls, variants_span));
                } else {
                    let expr =
                        self.convert_type_level_expr_fn(expr.unwrap(), context, &fn_item.sig)?;
                    trait_item.default = Some(Block {
                        brace_token: Default::default(),
                        stmts: vec![Stmt::Expr(expr, None)],
                    });
                    trait_item.semi_token = None;
                }
                Ok((TraitItem::Fn(trait_item), variants))
            }
        }
    }

    fn get_self_match<E>(
        expr: &mut Option<TypeLevelExpr<E>>,
    ) -> Option<TypeLevelExprMatch<TypeLevelExpr<E>>> {
        let Some(TypeLevelExpr::Match(match_expr)) = expr else {
            return None;
        };
        let Type::Path(TypePath { qself: None, path }) = &match_expr.ty else {
            return None;
        };
        if !path.is_ident(SELF_TYPE_NAME) {
            return None;
        }
        let Some(TypeLevelExpr::Match(match_expr)) = take(expr) else {
            unreachable!();
        };
        Some(match_expr)
    }

    pub fn convert_type_level_expr_type(
        &mut self,
        expr: TypeLevelExpr<Type>,
        context: &GenericsContext,
        bounds: &TypeParamBounds,
    ) -> Result<Type> {
        self.convert_type_level_expr(
            expr,
            bounds.clone(),
            context,
            |ident, generics, expr, bounds| {
                Ok(TraitImplItem::Type(TraitImplItemType {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    type_token: Default::default(),
                    ident,
                    generics,
                    bounds,
                    ty: expr,
                }))
            },
            |qself, path| Ok(Type::Path(TypePath { qself, path })),
        )
    }

    pub fn convert_type_level_expr_const(
        &mut self,
        expr: TypeLevelExpr<Expr>,
        context: &GenericsContext,
        ty: &Type,
    ) -> Result<Expr> {
        self.convert_type_level_expr(
            expr,
            ty.clone(),
            context,
            |ident, generics, expr, ty| {
                if !generics.params.is_empty() {
                    return Err(Error::new(
                        expr.span(),
                        "translation of this match expression would require constants with generic parameters"),
                    );
                }
                Ok(TraitImplItem::Const(TraitImplItemConst {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    const_token: Default::default(),
                    ident,
                    ty,
                    expr,
                }))
            },
            |qself, path| {
                Ok(Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                }))
            },
        )
    }

    pub fn convert_type_level_expr_fn(
        &mut self,
        expr: TypeLevelExpr<Expr>,
        context: &GenericsContext,
        sig: &Signature,
    ) -> Result<Expr> {
        self.convert_type_level_expr(
            expr,
            sig.clone(),
            context,
            |ident, generics, expr, sig| {
                Ok(TraitImplItem::Fn(TraitImplItemFn {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    sig: Signature {
                        ident,
                        generics,
                        ..sig
                    },
                    expr,
                }))
            },
            |qself, mut path| {
                if let PathArguments::AngleBracketed(path_args) =
                    &mut path.segments.last_mut().unwrap().arguments
                {
                    path_args.colon2_token = Some(Default::default());
                }
                let mut args = Punctuated::new();
                for arg in &sig.inputs {
                    let FnArg::Typed(arg) = arg else {
                        return Err(Error::new(arg.span(), "unexpected self parameter"));
                    };
                    let Pat::Ident(ident) = arg.pat.as_ref() else {
                        return Err(Error::new(
                            arg.pat.span(),
                            "patterns in parameters are currently not supported",
                        ));
                    };
                    args.push(Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: ident.ident.clone().into(),
                    }));
                }
                Ok(Expr::Call(ExprCall {
                    attrs: Vec::new(),
                    func: Box::new(Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself,
                        path,
                    })),
                    paren_token: Default::default(),
                    args,
                }))
            },
        )
    }

    pub fn convert_type_level_expr<E: Substitutable, X: Substitutable>(
        &mut self,
        expr: TypeLevelExpr<E>,
        extra: X,
        context: &GenericsContext,
        create_trait_impl_item: impl FnOnce(
            Ident,
            Generics,
            TypeLevelExpr<E>,
            X,
        ) -> Result<TraitImplItem>,
        create_path_expr: impl FnOnce(Option<QSelf>, Path) -> Result<E>,
    ) -> Result<E> {
        match expr {
            TypeLevelExpr::Expr(expr) => Ok(expr),
            TypeLevelExpr::Match(match_expr) => {
                let (qself, path) = self.convert_type_level_match_expr(
                    match_expr,
                    extra,
                    context,
                    create_trait_impl_item,
                )?;
                create_path_expr(qself, path)
            }
        }
    }

    pub fn convert_type_level_match_expr<E: Substitutable, X: Substitutable>(
        &mut self,
        match_expr: TypeLevelExprMatch<TypeLevelExpr<E>>,
        extra: X,
        context: &GenericsContext,
        create_trait_impl_item: impl FnOnce(
            Ident,
            Generics,
            TypeLevelExpr<E>,
            X,
        ) -> Result<TraitImplItem>,
    ) -> Result<(Option<QSelf>, Path)> {
        let ty = match_expr.ty.clone();
        let Some(match_ident) = Self::get_type_ident(&ty) else {
            return Err(Error::new(
                ty.span(),
                "type matching is currently only supported on variables",
            ));
        };
        let mut expr = (match_expr, extra);
        let (match_param, generics, arguments) =
            isolate_type_param(&mut expr, context, match_ident)?;
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
        let trait_item_ident = Ident::new(
            &format!("__{}", trait_def_item.next_internal_item_idx),
            match_ident.span(),
        );
        trait_def_item.next_internal_item_idx += 1;
        let variants_known = trait_def_item.variants.is_some();
        let (trait_item, variants) = self.create_trait_item(
            create_trait_impl_item(
                trait_item_ident.clone(),
                generics,
                TypeLevelExpr::Match(expr.0),
                expr.1,
            )?,
            &impl_context,
            variants_known,
        )?;
        let trait_def_item = self.trait_def_item(trait_ident)?;
        trait_def_item.add_item(trait_item, variants)?;
        let mut segments = trait_bound.path.segments.clone();
        segments.push(PathSegment {
            ident: trait_item_ident,
            arguments,
        });
        Ok((
            Some(QSelf {
                lt_token: Default::default(),
                ty: Box::new(ty),
                position: segments.len() - 1,
                as_token: Some(Default::default()),
                gt_token: Default::default(),
            }),
            Path {
                leading_colon: trait_bound.path.leading_colon.clone(),
                segments,
            },
        ))
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

    fn trait_item_attrs(mut attrs: Vec<Attribute>, vis: &Visibility) -> Vec<Attribute> {
        if !matches!(vis, Visibility::Public(_)) {
            // Hack: We declare private trait items as `deprecated` and use `#[allow(deprecated)]`
            // internally.
            attrs.push(parse_quote!(#[deprecated = "private"]));
            attrs.push(parse_quote!(#[doc(hidden)]));
        }
        attrs
    }

    pub fn code_item_attrs(mut attrs: Vec<Attribute>) -> Vec<Attribute> {
        attrs.push(parse_quote!(#[allow(deprecated)]));
        attrs
    }
}

impl ToTokens for OutputMetaItemList<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for item in &self.0 {
            match item {
                OutputMetaItem::TraitDef(trait_item) => trait_item.to_tokens(tokens),
                OutputMetaItem::Item(item) => item.to_tokens(tokens),
            }
        }
    }
}

pub enum OutputMetaItem<'a> {
    TraitDef(OutputItemTraitDef<'a>),
    Item(Item),
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
        variants: Option<(Vec<(Option<TraitVariant>, ImplItem)>, Span)>,
    ) -> Result<()> {
        self.impl_items.push(item);
        if let Some((variants, variants_span)) = variants {
            if let Some(existing_variants) = &mut self.variants {
                let mut variant_iter = variants.into_iter();
                for existing_variant in existing_variants {
                    let Some((variant, mut impl_item)) = variant_iter.next() else {
                        return Err(Error::new(
                            variants_span,
                            format!(
                                "too few variants; expected {}",
                                &existing_variant.variant.ident
                            ),
                        ));
                    };
                    if let Some(variant) = variant {
                        if existing_variant.variant.ident != variant.ident {
                            return Err(Error::new(
                                variant.ident.span(),
                                format!("expected variant {}", &existing_variant.variant.ident),
                            ));
                        }
                        let mut expected_generics = existing_variant.variant.generics.clone();
                        rename_all_params(&mut expected_generics, &variant.generics)?;
                        check_token_equality(&variant.generics, &expected_generics)?;
                        impl_item.substitute_all_params(
                            &variant.generics,
                            &existing_variant.variant.generics,
                        )?;
                    } else {
                        assert!(variant_iter.next().is_none());
                        variant_iter = vec![(None, impl_item.clone())].into_iter();
                    }
                    existing_variant.impl_items.push(impl_item);
                }
            } else {
                self.variants = Some(
                    variants
                        .into_iter()
                        .map(|(variant, impl_item)| OutputTraitVariant {
                            variant: variant.unwrap(),
                            impl_items: vec![impl_item],
                        })
                        .collect(),
                );
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

    fn output_contents(&self, tokens: &mut TokenStream) {
        match &self.trait_item.contents {
            TraitContents::Enum { variants } => {
                for variant in variants {
                    self.output_variant_def(variant, tokens);
                }
            }
        }
    }

    fn output_variant_def(&self, variant: &TraitVariant, tokens: &mut TokenStream) {
        let mut variant_generics = variant.generics.clone();
        self.trait_item
            .generics
            .eliminate_in_generics(&mut variant_generics);

        let phantom_types = phantom_types(&variant_generics);
        let struct_item = ItemStruct {
            attrs: variant.attrs.clone(),
            vis: self.trait_item.vis.clone(),
            struct_token: Default::default(),
            ident: variant.ident.clone(),
            generics: variant_generics.clone(),
            fields: Fields::Unnamed(FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: phantom_types,
            }),
            semi_token: Some(Default::default()),
        };
        struct_item.to_tokens(tokens);

        Self::output_variant_dummy_impls(&variant.ident, &variant_generics, tokens);
    }

    fn output_variant_dummy_impls(
        variant_ident: &Ident,
        variant_generics: &Generics,
        tokens: &mut TokenStream,
    ) {
        // Even though no instances of the variant types should ever be constructed, we output dummy
        // impls for standard traits, as otherwise `derive` doesn't work for types with enum trait
        // parameters.

        let variant_args = generic_args(variant_generics);

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::clone::Clone),
            quote! {
                fn clone(&self) -> Self {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::marker::Copy),
            TokenStream::new(),
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::default::Default),
            quote! {
                fn default() -> Self {
                    panic!("marker type is not intended to be constructed")
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::cmp::PartialEq),
            quote! {
                fn eq(&self, _other: &Self) -> bool {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::cmp::Eq),
            TokenStream::new(),
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::cmp::PartialOrd),
            quote! {
                fn partial_cmp(&self, _other: &Self) -> Option<core::cmp::Ordering> {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::cmp::Ord),
            quote! {
                fn cmp(&self, _other: &Self) -> core::cmp::Ordering {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::hash::Hash),
            quote! {
                fn hash<H: core::hash::Hasher>(&self, _state: &mut H) {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(core::fmt::Debug),
            quote! {
                fn fmt(&self, _f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    unreachable!()
                }
            },
            tokens,
        );
    }

    fn output_variant_dummy_impl(
        variant_ident: &Ident,
        variant_generics: &Generics,
        variant_args: &PathArguments,
        trait_path: TokenStream,
        impl_contents: TokenStream,
        tokens: &mut TokenStream,
    ) {
        tokens.append_all(quote! {
            #[doc(hidden)]
            impl #variant_generics #trait_path for #variant_ident #variant_args {
                #impl_contents
            }
        });
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
        let mut supertraits: Punctuated<TypeParamBound, Token![+]>;
        match &self.trait_item.contents {
            TraitContents::Enum { variants } => {
                supertraits = parse_quote!(
                    core::marker::Sized
                        + core::clone::Clone
                        + core::marker::Copy
                        + core::default::Default
                        + core::cmp::PartialEq
                        + core::cmp::Eq
                        + core::cmp::PartialOrd
                        + core::cmp::Ord
                        + core::hash::Hash
                        + core::fmt::Debug
                );
                if variants.iter().all(|variant| {
                    variant.generics.params.iter().all(|param| {
                        if let GenericParam::Type(type_param) = param {
                            type_param.bounds.iter().any(|bound| {
                                if let TypeParamBound::Trait(trait_bound) = bound {
                                    trait_bound.path.is_ident(&self.trait_item.ident)
                                } else {
                                    false
                                }
                            })
                        } else {
                            false
                        }
                    })
                }) {
                    supertraits.push(TypeParamBound::Lifetime(parse_quote!('static)));
                }
            }
        }
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
            supertraits,
            brace_token: Default::default(),
            items: self.impl_items.clone(),
        };
        trait_item.to_tokens(tokens);

        self.output_contents(tokens);

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
        let ident = &self.trait_item.ident;
        let macro_ident = ident_with_prefix(ident, "__trait_impl__");
        // TODO: Add support for trait aliases where variants are not known.
        tokens.append_all(quote!(macro_rules! #macro_ident {
            (#macro_params) => { #macro_body }
        }));

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
