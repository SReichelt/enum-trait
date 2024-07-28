use std::{borrow::Cow, collections::HashMap, mem::take};

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{punctuated::Punctuated, spanned::Spanned, visit_mut::VisitMut, *};

use crate::{expr::*, generics::*, helpers::*, item::*, subst::*};

pub struct OutputMetaItemList<'a>(pub Vec<OutputMetaItem<'a>>);

impl<'a> OutputMetaItemList<'a> {
    pub fn new() -> Self {
        OutputMetaItemList(Vec::new())
    }

    pub fn trait_def_item(&mut self, ident: &Ident) -> Result<&mut OutputItemTraitDef<'a>> {
        let Some(trait_def_item) = self.0.iter_mut().find_map(|output_item| {
            if let OutputMetaItem::TraitDef(trait_def_item) = output_item {
                if &trait_def_item.trait_def.ident == ident {
                    return Some(trait_def_item);
                }
            }
            None
        }) else {
            return Err(Error::new_spanned(
                &ident,
                format!(
                    "trait `{ident}` not defined in this meta block (may need to define an alias)"
                ),
            ));
        };
        Ok(trait_def_item)
    }

    pub fn create_trait_item(
        &mut self,
        part_ident: &mut Option<Ident>,
        item: TraitImplItem,
        context: &GenericsContext,
        trait_def: &ItemTraitDef,
        variants_known: bool,
    ) -> Result<OutputTraitItemDesc> {
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
                RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                    .visit_trait_item_type_mut(&mut trait_item);
                let item_context = GenericsContext::WithGenerics(&trait_item.generics, &context);
                let mut expr = Some(type_item.ty);
                let mut dependent_idents = Vec::new();
                let mut variants = Self::try_implement_variants(
                    &mut expr,
                    &item_context,
                    trait_def,
                    &mut dependent_idents,
                    |body, body_context, _| {
                        let mut ty = self.convert_type_level_expr_type(
                            &type_item.attrs,
                            part_ident,
                            body,
                            &body_context,
                            &type_item.bounds,
                        )?;
                        RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                            .visit_type_mut(&mut ty);
                        Ok(ImplItem::Type(ImplItemType {
                            attrs: Self::code_item_attrs(type_item.attrs.clone()),
                            vis: Visibility::Inherited,
                            defaultness: None,
                            type_token: trait_item.type_token.clone(),
                            ident: trait_item.ident.clone(),
                            generics: trait_item.generics.clone(),
                            eq_token: Default::default(),
                            ty,
                            semi_token: Default::default(),
                        }))
                    },
                )?;
                if variants.is_none() {
                    let mut ty = self.convert_type_level_expr_type(
                        &type_item.attrs,
                        part_ident,
                        expr.unwrap(),
                        &item_context,
                        &type_item.bounds,
                    )?;
                    RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                        .visit_type_mut(&mut ty);
                    if variants_known {
                        let span = ty.span();
                        // Prefer individual impls over default in trait because the latter is
                        // currently unstable.
                        let impl_item = ImplItemType {
                            attrs: Self::code_item_attrs(type_item.attrs.clone()),
                            vis: Visibility::Inherited,
                            defaultness: None,
                            type_token: trait_item.type_token.clone(),
                            ident: trait_item.ident.clone(),
                            generics: trait_item.generics.clone(),
                            eq_token: Default::default(),
                            ty,
                            semi_token: Default::default(),
                        };
                        variants = Some((vec![(None, ImplItem::Type(impl_item))], span));
                    } else {
                        trait_item.default = Some((Default::default(), ty));
                    }
                }
                trait_def.collect_dependencies_in_generics(
                    &type_item.generics,
                    part_ident,
                    &mut dependent_idents,
                );
                trait_def.collect_dependencies_in_bounds(
                    &type_item.bounds,
                    part_ident,
                    &mut dependent_idents,
                );
                Ok(OutputTraitItemDesc {
                    item: TraitItem::Type(trait_item),
                    variants,
                    dependent_idents,
                })
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
                RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                    .visit_trait_item_const_mut(&mut trait_item);
                let mut expr = Some(const_item.expr);
                let mut dependent_idents = Vec::new();
                let variants = Self::try_implement_variants(
                    &mut expr,
                    context,
                    trait_def,
                    &mut dependent_idents,
                    |body, body_context, substitutions| {
                        let mut expr = self.convert_type_level_expr_const(
                            &const_item.attrs,
                            part_ident,
                            body,
                            &body_context,
                            &const_item.ty,
                        )?;
                        RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                            .visit_expr_mut(&mut expr);
                        let mut ty = trait_item.ty.clone();
                        for (subst_param, subst_arg) in substitutions {
                            ty.substitute(subst_param, ParamSubstArg::Arg(subst_arg))?;
                        }
                        Ok(ImplItem::Const(ImplItemConst {
                            attrs: Self::code_item_attrs(const_item.attrs.clone()),
                            vis: Visibility::Inherited,
                            defaultness: None,
                            const_token: trait_item.const_token.clone(),
                            ident: trait_item.ident.clone(),
                            generics: trait_item.generics.clone(),
                            colon_token: Default::default(),
                            ty,
                            eq_token: Default::default(),
                            expr,
                            semi_token: Default::default(),
                        }))
                    },
                )?;
                if variants.is_none() {
                    let mut expr = self.convert_type_level_expr_const(
                        &const_item.attrs,
                        part_ident,
                        expr.unwrap(),
                        context,
                        &const_item.ty,
                    )?;
                    RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                        .visit_expr_mut(&mut expr);
                    trait_item.default = Some((Default::default(), expr));
                }
                Ok(OutputTraitItemDesc {
                    item: TraitItem::Const(trait_item),
                    variants,
                    dependent_idents,
                })
            }

            TraitImplItem::Fn(fn_item) => {
                let mut trait_item = TraitItemFn {
                    attrs: Self::trait_item_attrs(fn_item.attrs.clone(), &fn_item.vis),
                    sig: fn_item.sig.clone(),
                    default: None,
                    semi_token: Default::default(),
                };
                RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                    .visit_trait_item_fn_mut(&mut trait_item);
                let item_context =
                    GenericsContext::WithGenerics(&trait_item.sig.generics, &context);
                let mut expr = Some(fn_item.expr);
                let mut dependent_idents = Vec::new();
                let variants = Self::try_implement_variants(
                    &mut expr,
                    &item_context,
                    trait_def,
                    &mut dependent_idents,
                    |body, body_context, substitutions| {
                        let mut expr = self.convert_type_level_expr_fn(
                            &fn_item.attrs,
                            part_ident,
                            body,
                            &body_context,
                            &fn_item.sig,
                        )?;
                        RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                            .visit_expr_mut(&mut expr);
                        let mut sig = trait_item.sig.clone();
                        for (subst_param, subst_arg) in substitutions {
                            sig.substitute(subst_param, ParamSubstArg::Arg(subst_arg))?;
                        }
                        Ok(ImplItem::Fn(ImplItemFn {
                            attrs: Self::code_item_attrs(fn_item.attrs.clone()),
                            vis: Visibility::Inherited,
                            defaultness: None,
                            sig,
                            block: Block {
                                brace_token: Default::default(),
                                stmts: vec![Stmt::Expr(expr, None)],
                            },
                        }))
                    },
                )?;
                if variants.is_none() {
                    let mut expr = self.convert_type_level_expr_fn(
                        &fn_item.attrs,
                        part_ident,
                        expr.unwrap(),
                        context,
                        &fn_item.sig,
                    )?;
                    RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                        .visit_expr_mut(&mut expr);
                    trait_item.default = Some(Block {
                        brace_token: Default::default(),
                        stmts: vec![Stmt::Expr(expr, None)],
                    });
                    trait_item.semi_token = None;
                }
                trait_def.collect_dependencies_in_generics(
                    &fn_item.sig.generics,
                    part_ident,
                    &mut dependent_idents,
                );
                Ok(OutputTraitItemDesc {
                    item: TraitItem::Fn(trait_item),
                    variants,
                    dependent_idents,
                })
            }
        }
    }

    fn get_self_match<E>(
        expr: &mut Option<TypeLevelExpr<E>>,
    ) -> Option<TypeLevelExprMatch<TypeLevelExpr<E>>> {
        let Some(TypeLevelExpr::Match(match_expr)) = expr else {
            return None;
        };
        if !type_is_ident(match_expr.types.last().unwrap(), SELF_TYPE_NAME) {
            return None;
        }
        let Some(TypeLevelExpr::Match(match_expr)) = take(expr) else {
            unreachable!();
        };
        Some(match_expr)
    }

    fn try_implement_variants<E: Substitutable + ToTokens>(
        expr: &mut Option<TypeLevelExpr<E>>,
        context: &GenericsContext,
        trait_def: &ItemTraitDef,
        dependent_idents: &mut Vec<Ident>,
        f: impl FnMut(
            TypeLevelExpr<E>,
            &GenericsContext,
            &[(&GenericParam, GenericArgument)],
        ) -> Result<ImplItem>,
    ) -> Result<Option<(Vec<(Option<ImplVariant>, ImplItem)>, Span)>> {
        if let Some(match_expr) = Self::get_self_match(expr) {
            let variants_span = match_expr.span();
            let variants_impls =
                Self::implement_variants(match_expr, context, trait_def, dependent_idents, f)?;
            Ok(Some((variants_impls, variants_span)))
        } else {
            Ok(None)
        }
    }

    fn implement_variants<E: Substitutable>(
        match_expr: TypeLevelExprMatch<E>,
        context: &GenericsContext,
        trait_def: &ItemTraitDef,
        dependent_idents: &mut Vec<Ident>,
        mut f: impl FnMut(E, &GenericsContext, &[(&GenericParam, GenericArgument)]) -> Result<ImplItem>,
    ) -> Result<Vec<(Option<ImplVariant>, ImplItem)>> {
        let mut free_params: Vec<GenericParam> = trait_def
            .generics
            .extract_generics()
            .params
            .into_iter()
            .collect();
        let mut matched_params = Vec::new();
        let types_len = match_expr.types.len();
        let mut ty_iter = match_expr.types.iter();
        let mut ty = ty_iter.next().unwrap();
        while let Some(next_ty) = ty_iter.next() {
            let Some(match_ident) = get_type_ident(&ty) else {
                return Err(Error::new(
                    ty.span(),
                    "type matching is currently only supported on variables",
                ));
            };
            let Some(generic_idx) = free_params.iter().position(|param| {
                if let GenericParam::Type(type_param) = param {
                    &type_param.ident == match_ident
                } else {
                    false
                }
            }) else {
                let free_param_idents = free_params
                    .iter()
                    .filter_map(|param| {
                        if let GenericParam::Type(type_param) = param {
                            Some(format!("`{}`", type_param.ident))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                return Err(Error::new(
                    ty.span(),
                    format!("combined matching is currently only supported for trait arguments (`{match_ident}` not found in [{free_param_idents}])")));
            };
            matched_params.push(free_params.remove(generic_idx));
            ty = next_ty;
        }
        match_expr
            .arms
            .into_iter()
            .map(|mut arm| {
                if arm.selectors.len() != types_len {
                    return Err(Error::new(
                        arm.selectors.span(),
                        format!("exactly {types_len} selector(s) expected"),
                    ));
                }
                let mut impl_generic_params = Punctuated::new();
                let mut trait_args = Punctuated::new();
                for param in &free_params {
                    impl_generic_params.push(param.clone());
                    trait_args.push(generic_param_arg(param, None));
                }
                let mut substitutions = Vec::new();
                let mut selector_iter = arm.selectors.into_iter();
                let mut selector = selector_iter.next().unwrap();
                let mut matched_param_iter = matched_params.iter();
                while let Some(next_selector) = selector_iter.next() {
                    let matched_param = matched_param_iter.next().unwrap();
                    match selector {
                        TypeLevelArmSelector::Specific { ident, generics } => {
                            trait_def.collect_dependencies_in_generics(
                                &generics,
                                &mut None,
                                dependent_idents,
                            );
                            let arg = Self::create_arm_arg(&ident, &generics);
                            for param in &generics.params {
                                let mut param = param.clone();
                                RemoveTypeBoundParamsFromPathArguments(&trait_def.generics)
                                    .visit_generic_param_mut(&mut param);
                                impl_generic_params.push(param);
                            }
                            trait_args.push(arg.clone());
                            substitutions.push((matched_param, arg));
                        }
                        TypeLevelArmSelector::Default { .. } => {
                            impl_generic_params.push(matched_param.clone());
                            trait_args.push(generic_param_arg(matched_param, None));
                        }
                    }
                    selector = next_selector;
                }
                match selector {
                    TypeLevelArmSelector::Specific {
                        ident,
                        mut generics,
                    } => {
                        rename_conflicting_params(
                            &mut generics,
                            |param| Ok(param_context_name_conflict(param, &context)),
                            |subst| arm.body.substitute_impl(subst),
                        )?;
                        let body_context = GenericsContext::WithGenerics(&generics, &context);
                        let impl_item = f(arm.body, &body_context, &substitutions)?;
                        Ok((
                            Some(ImplVariant {
                                impl_generics: build_generics(impl_generic_params),
                                trait_args: build_path_arguments(trait_args),
                                variant: TraitVariant {
                                    attrs: Vec::new(),
                                    ident,
                                    generics,
                                },
                            }),
                            impl_item,
                        ))
                    }
                    TypeLevelArmSelector::Default { underscore_token } => Err(Error::new(
                        underscore_token.span(),
                        "default selector only supported for trait arguments",
                    )),
                }
            })
            .collect::<Result<_>>()
    }

    fn create_arm_arg(ident: &Ident, generics: &Generics) -> GenericArgument {
        let segment = PathSegment {
            ident: ident.clone(),
            arguments: generic_args(&generics),
        };
        GenericArgument::Type(Type::Path(TypePath {
            qself: None,
            path: segment.into(),
        }))
    }

    pub fn convert_type_level_expr_type(
        &mut self,
        attrs: &Vec<Attribute>,
        part_ident: &Option<Ident>,
        expr: TypeLevelExpr<Type>,
        context: &GenericsContext,
        bounds: &TypeParamBounds,
    ) -> Result<Type> {
        self.convert_type_level_expr(
            part_ident,
            expr,
            bounds.clone(),
            context,
            |ident, generics, expr, bounds| {
                Ok(TraitImplItem::Type(TraitImplItemType {
                    attrs: attrs.clone(),
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
        attrs: &Vec<Attribute>,
        part_ident: &Option<Ident>,
        expr: TypeLevelExpr<Expr>,
        context: &GenericsContext,
        ty: &Type,
    ) -> Result<Expr> {
        self.convert_type_level_expr(
            part_ident,
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
                    attrs: attrs.clone(),
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
        attrs: &Vec<Attribute>,
        part_ident: &Option<Ident>,
        expr: TypeLevelExpr<Expr>,
        context: &GenericsContext,
        sig: &Signature,
    ) -> Result<Expr> {
        self.convert_type_level_expr(
            part_ident,
            expr,
            sig.clone(),
            context,
            |ident, generics, expr, sig| {
                Ok(TraitImplItem::Fn(TraitImplItemFn {
                    attrs: attrs.clone(),
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
        part_ident: &Option<Ident>,
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
                    part_ident,
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
        part_ident: &Option<Ident>,
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
        let ty = match_expr.types.last().unwrap().clone();
        let Some(match_ident) = get_type_ident(&ty) else {
            return Err(Error::new(
                ty.span(),
                "type matching is currently only supported on variables",
            ));
        };
        let mut expr = (match_expr, extra);
        let (match_param, mut extracted_params) =
            isolate_type_param(&mut expr, context, match_ident)?;
        let Some(TypeParamBound::Trait(trait_bound)) = match_param.bounds.first() else {
            return Err(Error::new(
                ty.span(),
                "no appropriate type bound for matching found",
            ));
        };
        if trait_bound.path.leading_colon.is_some() || trait_bound.path.segments.len() != 1 {
            return Err(Error::new(
                ty.span(),
                "matching is not supported on externally-defined traits; define an alias within this block",
            ));
        };
        let trait_segment = trait_bound.path.segments.last().unwrap();
        let trait_ident = &trait_segment.ident;
        let trait_def_item = self.trait_def_item(trait_ident)?;
        Self::eliminate_exact_trait_args(
            trait_def_item,
            &trait_segment.arguments,
            &mut expr,
            &mut extracted_params,
        )?;
        let impl_context = trait_def_item.impl_context();
        let trait_def = trait_def_item.trait_def;
        let trait_item_ident = Ident::new(
            &format!("__{}", trait_def_item.next_internal_item_idx),
            Span::call_site(),
        );
        trait_def_item.next_internal_item_idx += 1;
        let variants_known = trait_def_item.variants.is_some();
        let (params, args) = extracted_params.into_iter().unzip();
        let impl_item = create_trait_impl_item(
            trait_item_ident.clone(),
            build_generics(params),
            TypeLevelExpr::Match(expr.0),
            expr.1,
        )?;
        let mut part_ident = part_ident.clone();
        let trait_item_desc = self.create_trait_item(
            &mut part_ident,
            impl_item,
            &impl_context,
            trait_def,
            variants_known,
        )?;
        let trait_def_item = self.trait_def_item(trait_ident)?;
        trait_def_item.add_item(&part_ident, trait_item_desc)?;
        let mut segments = trait_bound.path.segments.clone();
        segments.push(PathSegment {
            ident: trait_item_ident,
            arguments: build_path_arguments(args),
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

    fn eliminate_exact_trait_args(
        trait_def_item: &OutputItemTraitDef,
        arguments: &PathArguments,
        expr: &mut impl Substitutable,
        extracted_params: &mut Vec<(GenericParam, GenericArgument)>,
    ) -> Result<()> {
        if let PathArguments::AngleBracketed(args) = arguments {
            for (param, arg) in trait_def_item
                .extracted_generics
                .params
                .iter()
                .zip(args.args.iter())
            {
                if let GenericArgument::Type(arg_ty) = arg {
                    if let Some(arg_ident) = get_type_ident(arg_ty) {
                        if let Some(extracted_param_idx) =
                            extracted_params.iter().position(|(_, extracted_arg)| {
                                if let GenericArgument::Type(extracted_arg_ty) = extracted_arg {
                                    type_is_ident(extracted_arg_ty, arg_ident)
                                } else {
                                    false
                                }
                            })
                        {
                            let (extracted_param, _) = extracted_params.remove(extracted_param_idx);
                            expr.substitute(&extracted_param, ParamSubstArg::Param(param))?;
                        }
                    }
                }
            }
        }
        Ok(())
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

pub struct OutputTraitItemDesc {
    item: TraitItem,
    variants: Option<(Vec<(Option<ImplVariant>, ImplItem)>, Span)>,
    dependent_idents: Vec<Ident>,
}

pub struct OutputItemTraitDef<'a> {
    pub trait_def: &'a ItemTraitDef,
    pub extracted_generics: Generics,
    pub variants: Option<Vec<OutputImplVariant>>,
    pub impl_items: ImplPartList<TraitItem>,
    pub dependent_idents: Vec<Ident>,
    pub next_internal_item_idx: usize,
}

impl<'a> OutputItemTraitDef<'a> {
    pub fn add_item(
        &mut self,
        part_ident: &Option<Ident>,
        item: OutputTraitItemDesc,
    ) -> Result<()> {
        self.impl_items.add_item(part_ident, item.item);

        if let Some((variants, variants_span)) = item.variants {
            if self.variants.is_none() {
                self.variants = Some(
                    variants
                        .iter()
                        .map(|(orig_variant, _)| {
                            // `unwrap` because `create_trait_item` only outputs `None` if
                            // `variants_known` is `true`.
                            let orig_variant = orig_variant.as_ref().unwrap();
                            let mut variant = orig_variant.clone();
                            add_underscores_to_all_params(&mut variant.impl_generics)?;
                            variant.trait_args.substitute_all_params(
                                &orig_variant.impl_generics,
                                &variant.impl_generics,
                            )?;
                            variant.variant.generics.substitute_all_params(
                                &orig_variant.impl_generics,
                                &variant.impl_generics,
                            )?;
                            add_underscores_to_all_params(&mut variant.variant.generics)?;
                            Ok(OutputImplVariant {
                                variant,
                                impl_items: ImplPartList::new(),
                            })
                        })
                        .collect::<Result<_>>()?,
                );
            }
            let existing_variants = self.variants.as_mut().unwrap();
            let mut variant_iter = variants.into_iter();
            for existing_variant in existing_variants {
                let existing_trait_variant = &existing_variant.variant.variant;
                let Some((variant, mut impl_item)) = variant_iter.next() else {
                    return Err(Error::new(
                        variants_span,
                        format!(
                            "too few variants; expected `{}`",
                            &existing_trait_variant.ident
                        ),
                    ));
                };
                if let Some(variant) = variant {
                    let trait_variant = &variant.variant;
                    if existing_trait_variant.ident != trait_variant.ident {
                        return Err(Error::new(
                            trait_variant.ident.span(),
                            format!("expected variant `{}`", &existing_trait_variant.ident),
                        ));
                    }
                    let mut expected_impl_generics = existing_variant.variant.impl_generics.clone();
                    rename_all_params(&mut expected_impl_generics, &variant.impl_generics)?;
                    check_token_equality(&variant.impl_generics, &expected_impl_generics)?;
                    let mut variant_generics = trait_variant.generics.clone();
                    variant_generics.substitute_all_params(
                        &variant.impl_generics,
                        &existing_variant.variant.impl_generics,
                    )?;
                    impl_item.substitute_all_params(
                        &variant.impl_generics,
                        &existing_variant.variant.impl_generics,
                    )?;
                    let mut expected_trait_args = existing_variant.variant.trait_args.clone();
                    expected_trait_args.substitute_all_params(
                        &existing_variant.variant.impl_generics,
                        &variant.impl_generics,
                    )?;
                    check_token_equality(&variant.trait_args, &expected_trait_args)?;
                    let mut expected_generics = existing_trait_variant.generics.clone();
                    rename_all_params(&mut expected_generics, &variant_generics)?;
                    check_token_equality(&variant_generics, &expected_generics)?;
                    impl_item.substitute_all_params(
                        &variant_generics,
                        &existing_trait_variant.generics,
                    )?;
                } else {
                    assert!(variant_iter.next().is_none());
                    variant_iter = vec![(None, impl_item.clone())].into_iter();
                }
                existing_variant.impl_items.add_item(part_ident, impl_item);
            }
            if let Some((Some(variant), _)) = variant_iter.next() {
                return Err(Error::new(
                    variant.variant.ident.span(),
                    "superfluous variant",
                ));
            }
        }

        for ident in item.dependent_idents {
            if !self.dependent_idents.contains(&ident) {
                self.dependent_idents.push(ident);
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
            path: self.trait_def.ident.clone().into(),
        }));
        GenericsContext::WithSelf(
            Cow::Owned(self_type_param(None, bounds)),
            &GenericsContext::Empty,
        )
    }

    fn trait_body_macro_ident(ident: &Ident) -> Ident {
        ident_with_prefix(ident, "__trait_body__", false)
    }

    fn impl_macro_ident(ident: &Ident) -> Ident {
        ident_with_prefix(ident, "__trait_impl__", false)
    }

    fn impl_body_macro_ident(ident: &Ident) -> Ident {
        ident_with_prefix(ident, "__trait_impl_body__", false)
    }

    fn output_contents(&self, tokens: &mut TokenStream) {
        if let TraitContents::Enum { variants } = &self.trait_def.contents {
            for variant in variants {
                self.output_variant_def(variant, tokens);
            }
        }
    }

    fn output_variant_def(&self, variant: &TraitVariant, tokens: &mut TokenStream) {
        let mut variant_generics = variant.generics.clone();
        RemoveTypeBoundParamsFromPathArguments(&self.trait_def.generics)
            .visit_generics_mut(&mut variant_generics);
        self.trait_def
            .generics
            .erase_in_generics(&mut variant_generics);

        let phantom_types = phantom_types(&variant_generics);
        let struct_item = ItemStruct {
            attrs: variant.attrs.clone(),
            vis: self.trait_def.vis.clone(),
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
            quote!(::core::clone::Clone),
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
            quote!(::core::marker::Copy),
            TokenStream::new(),
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(::core::default::Default),
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
            quote!(::core::cmp::PartialEq),
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
            quote!(::core::cmp::Eq),
            TokenStream::new(),
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(::core::cmp::PartialOrd),
            quote! {
                fn partial_cmp(&self, _other: &Self) -> Option<::core::cmp::Ordering> {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(::core::cmp::Ord),
            quote! {
                fn cmp(&self, _other: &Self) -> ::core::cmp::Ordering {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(::core::hash::Hash),
            quote! {
                fn hash<H: ::core::hash::Hasher>(&self, _state: &mut H) {
                    unreachable!()
                }
            },
            tokens,
        );

        Self::output_variant_dummy_impl(
            variant_ident,
            variant_generics,
            &variant_args,
            quote!(::core::fmt::Debug),
            quote! {
                fn fmt(&self, _f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
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
        tokens.extend(quote! {
            #[doc(hidden)]
            impl #variant_generics #trait_path for #variant_ident #variant_args {
                #impl_contents
            }
        });
    }

    fn output_ref_path(path: &Path, tokens: &mut TokenStream) {
        path.leading_colon.to_tokens(tokens);
        for segment_pair in path.segments.pairs() {
            if segment_pair.punct().is_some() {
                segment_pair.to_tokens(tokens);
            }
        }
    }
}

impl ToTokens for OutputItemTraitDef<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Specifies whether this trait has independent `impl` items, as opposed to having them
        // defined automatically via the implementation macro of the trait we are aliasing.
        let independent_impls = match &self.trait_def.contents {
            TraitContents::Enum { .. } => true,
            TraitContents::Alias { path } => {
                self.trait_def.generics.where_clause.is_some()
                    || path.arguments.has_complex_type_arg()
            }
        };

        let trait_ident = &self.trait_def.ident;
        let name_param_ident = Ident::new("_Name", Span::call_site());
        let name_param = quote!($#name_param_ident);
        let ref_path_param_ident = Ident::new("_ref_path", Span::call_site());
        let ref_path_param = quote!($#ref_path_param_ident);
        let mut macro_args_base = HashMap::from([
            (
                name_param_ident.clone(),
                MacroArg::ident(trait_ident.clone()),
            ),
            (ref_path_param_ident.clone(), MacroArg::Multi(Vec::new())),
        ]);
        let mut macro_default_type_bound_params = TokenStream::new();
        let mut macro_default_type_bound_args = TokenStream::new();
        for trait_param in &self.trait_def.generics.params {
            if let MetaGenericParam::TypeBound(type_bound_param) = trait_param {
                let ident = &type_bound_param.ident;
                macro_default_type_bound_params.extend(quote!(($($#ident:tt)+), ));
                macro_default_type_bound_args.extend(quote!(($($#ident)+), ));
                macro_args_base.insert(
                    ident.clone(),
                    MacroArg::multi_tokens(&type_bound_param.bounds),
                );
            }
        }
        let mut macro_params_base = quote!(#name_param:ident, $(#ref_path_param:ident::)*, #macro_default_type_bound_params);
        let mut macro_default_args_base =
            quote!(#name_param, $(#ref_path_param::)*, #macro_default_type_bound_args);
        for ident in &self.dependent_idents {
            macro_params_base.extend(quote!(trait #ident = ($($#ident:tt)+), ));
            macro_default_args_base.extend(quote!(trait #ident = ($($#ident)+), ));
            macro_args_base.insert(ident.clone(), MacroArg::multi_tokens(ident));
        }

        let generalize = |mut tokens| {
            for trait_param in &self.trait_def.generics.params {
                if let MetaGenericParam::TypeBound(type_bound_param) = trait_param {
                    let ident = &type_bound_param.ident;
                    tokens = replace_tokens(tokens, ident, &quote!($($#ident)+));
                }
            }
            for ident in &self.dependent_idents {
                tokens = replace_tokens(tokens, ident, &quote!($($#ident)+));
            }
            replace_tokens(tokens, trait_ident, &name_param)
        };

        let mut macro_parent_type_bound_args = macro_default_type_bound_args;
        if let TraitContents::Alias { path } = &self.trait_def.contents {
            for arg in &path.arguments.args {
                if let MetaGenericArgument::TraitAlias(alias_arg) = arg {
                    let ident = &alias_arg.ident;
                    let value = generalize(alias_arg.value.to_token_stream());
                    macro_parent_type_bound_args.extend(quote!(trait #ident = (#value), ));
                }
            }
        }

        let trait_body_macro_ident = Self::trait_body_macro_ident(trait_ident);
        let mut macro_contents = TokenStream::new();
        let mut trait_items = Vec::new();
        for part in &self.impl_items.0 {
            let mut macro_body = TokenStream::new();
            if let Some(part_ident) = &part.ident {
                if part_ident == SELF_TYPE_NAME && !independent_impls {
                    if let TraitContents::Alias { path } = &self.trait_def.contents {
                        let mut path = path.extract_path();
                        if let Some(segment) = path.segments.last_mut() {
                            segment.ident = Self::trait_body_macro_ident(&segment.ident);
                            segment.arguments = PathArguments::None;
                        }
                        let mut ref_path = TokenStream::new();
                        Self::output_ref_path(&path, &mut ref_path);
                        macro_body.extend(quote!(
                            #path!(#part_ident, #name_param, #ref_path, #macro_parent_type_bound_args);
                        ));
                    }
                }
            }
            let mut impl_items = TokenStream::new();
            for impl_item in &part.items {
                impl_item.to_tokens(&mut impl_items);
            }
            macro_body.extend(generalize(impl_items));
            if let Some(part_ident) = &part.ident {
                macro_contents.extend(quote!(
                    (#part_ident, #macro_params_base) => { #macro_body };
                ));
            }
            // Here, we would like to invoke the macro which we have just constructed (similarly to
            // the invocation we output when defining a trait alias). Unfortunately, Rust Analyzer
            // currently doesn't seem to expand such invocations transparently enough, so that IDE
            // navigation fails. Therefore, we expand the macro ourselves, essentially duplicating
            // its contents in our output.
            let expanded_macro = expand_macro_body(macro_body, &macro_args_base);
            trait_items.push(TraitItem::Verbatim(expanded_macro));
        }
        tokens.extend(quote! {
            #[macro_export]
            macro_rules! #trait_body_macro_ident {
                #macro_contents
            }
            pub use #trait_body_macro_ident;
        });

        let mut supertraits: Punctuated<TypeParamBound, Token![+]>;
        match &self.trait_def.contents {
            TraitContents::Enum { variants } => {
                supertraits = parse_quote!(
                    ::core::marker::Sized
                        + ::core::clone::Clone
                        + ::core::marker::Copy
                        + ::core::default::Default
                        + ::core::cmp::PartialEq
                        + ::core::cmp::Eq
                        + ::core::cmp::PartialOrd
                        + ::core::cmp::Ord
                        + ::core::hash::Hash
                        + ::core::fmt::Debug
                );
                if variants.iter().all(|variant| {
                    variant.generics.params.iter().all(|param| {
                        if let GenericParam::Type(type_param) = param {
                            type_param.bounds.iter().any(|bound| {
                                if let TypeParamBound::Trait(trait_bound) = bound {
                                    trait_bound.path.is_ident(&self.trait_def.ident)
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
            TraitContents::Alias { path } => {
                let mut path = path.extract_path();
                RemoveTypeBoundParamsFromPathArguments(&self.trait_def.generics)
                    .visit_path_mut(&mut path);
                supertraits = Punctuated::new();
                supertraits.push(TypeParamBound::Trait(TraitBound {
                    paren_token: None,
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path,
                }));
            }
        }
        let trait_item = ItemTrait {
            attrs: self.trait_def.attrs.clone(),
            vis: self.trait_def.vis.clone(),
            unsafety: None,
            auto_token: None,
            restriction: None,
            trait_token: self.trait_def.trait_token.clone(),
            ident: self.trait_def.ident.clone(),
            generics: self.extracted_generics.clone(),
            colon_token: None,
            supertraits,
            brace_token: Default::default(),
            items: trait_items,
        };
        trait_item.to_tokens(tokens);

        self.output_contents(tokens);

        let mut macro_variant_params = TokenStream::new();
        let mut macro_variant_args = TokenStream::new();
        let mut full_macro_variant_args = TokenStream::new();
        let mut macro_variant_default_args = TokenStream::new();
        let mut macro_body = TokenStream::new();
        let mut full_macro_body = TokenStream::new();
        let impl_body_macro_ident = Self::impl_body_macro_ident(trait_ident);
        let mut impl_body_macro_contents = TokenStream::new();
        let part_param_ident = Ident::new("_Part", Span::call_site());
        let part_param = quote!($#part_param_ident);
        let mut macro_args = macro_args_base;
        if let Some(variants) = &self.variants {
            for (variant_idx, output_variant) in variants.iter().enumerate() {
                let mut full_variant_impl_body = TokenStream::new();
                for part in &output_variant.impl_items.0 {
                    let mut impl_items = TokenStream::new();
                    for impl_item in &part.items {
                        impl_item.to_tokens(&mut impl_items);
                    }
                    let impl_body_macro_body = generalize(impl_items);
                    if let Some(part_ident) = &part.ident {
                        impl_body_macro_contents.extend(quote!(
                            ([#part_ident $(, $($_OtherPart:tt)*)?], #variant_idx, #macro_params_base) => {
                                #impl_body_macro_body
                                $(#ref_path_param::)*#impl_body_macro_ident!([$($($_OtherPart)*)?], #variant_idx, #macro_default_args_base);
                            };
                        ));
                    }
                    full_variant_impl_body.extend(impl_body_macro_body);
                }

                let variant = &output_variant.variant.variant;
                let mut macro_generic_params = Vec::new();
                let mut macro_generic_args = Vec::new();
                let mut macro_generic_default_args = Vec::new();
                let mut impl_generic_params: Vec<TokenStream> = output_variant
                    .variant
                    .impl_generics
                    .params
                    .iter()
                    .map(|param| generalize(param.to_token_stream()))
                    .collect();
                let mut impl_generic_args = Vec::new();
                let param_prefix = format!("Var_{variant_idx}_");
                for param in &variant.generics.params {
                    match param {
                        GenericParam::Lifetime(LifetimeParam {
                            attrs: _,
                            lifetime,
                            colon_token,
                            bounds,
                        }) => {
                            let macro_param_ident =
                                ident_with_prefix(&lifetime.ident, &param_prefix, true);
                            let macro_param = quote!($#macro_param_ident);
                            macro_generic_params.push(quote!(#macro_param:lifetime));
                            macro_generic_args.push(quote!(#macro_param));
                            macro_generic_default_args.push(lifetime.to_token_stream());
                            impl_generic_params.push(quote!(#macro_param #colon_token #bounds));
                            impl_generic_args.push(quote!(#macro_param));
                            macro_args.insert(macro_param_ident, MacroArg::single(&lifetime));
                        }
                        GenericParam::Type(TypeParam {
                            attrs: _,
                            ident,
                            colon_token,
                            bounds,
                            eq_token,
                            default,
                        }) => {
                            let macro_param_ident = ident_with_prefix(ident, &param_prefix, true);
                            let macro_param = quote!($#macro_param_ident);
                            macro_generic_params.push(quote!(#macro_param:ident));
                            macro_generic_args.push(quote!(#macro_param));
                            macro_generic_default_args.push(ident.to_token_stream());
                            let generalized_bounds = generalize(bounds.to_token_stream());
                            impl_generic_params.push(
                                quote!(#macro_param #colon_token #generalized_bounds #eq_token #default),
                            );
                            impl_generic_args.push(quote!(#macro_param));
                            macro_args.insert(macro_param_ident, MacroArg::ident(ident.clone()));
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
                            let macro_param_ident = ident_with_prefix(ident, &param_prefix, true);
                            let macro_param = quote!($#macro_param_ident);
                            macro_generic_params.push(quote!(#macro_param:ident));
                            macro_generic_args.push(quote!(#macro_param));
                            macro_generic_default_args.push(ident.to_token_stream());
                            impl_generic_params.push(
                                quote!(#const_token #macro_param #colon_token #ty #eq_token #default),
                            );
                            impl_generic_args.push(quote!(#macro_param));
                            macro_args.insert(macro_param_ident, MacroArg::ident(ident.clone()));
                        }
                    }
                }
                let mut macro_generics = TokenStream::new();
                let mut macro_arg_generics = TokenStream::new();
                let mut macro_default_arg_generics = TokenStream::new();
                if !macro_generic_params.is_empty() {
                    variant.generics.lt_token.to_tokens(&mut macro_generics);
                    macro_generics.append_separated(macro_generic_params, <Token![,]>::default());
                    macro_generics.extend(quote!($(,)?));
                    variant.generics.gt_token.to_tokens(&mut macro_generics);
                    variant.generics.lt_token.to_tokens(&mut macro_arg_generics);
                    macro_arg_generics.append_separated(macro_generic_args, <Token![,]>::default());
                    variant.generics.gt_token.to_tokens(&mut macro_arg_generics);
                    variant
                        .generics
                        .lt_token
                        .to_tokens(&mut macro_default_arg_generics);
                    macro_default_arg_generics
                        .append_separated(macro_generic_default_args, <Token![,]>::default());
                    variant
                        .generics
                        .gt_token
                        .to_tokens(&mut macro_default_arg_generics);
                }
                let body_param_ident =
                    Ident::new(&format!("{param_prefix}_Body"), Span::call_site());
                let body_param = quote!($#body_param_ident);
                let variant_impl_body = quote! {
                    $(#ref_path_param::)*#impl_body_macro_ident!([$(#part_param)*], #variant_idx, #macro_default_args_base);
                    $(#body_param)*
                };
                let variant_ident = &variant.ident;
                macro_variant_params.extend(quote!(#variant_ident #macro_generics => {
                    $(#body_param:tt)*
                }));
                macro_variant_args.extend(quote!(#variant_ident #macro_arg_generics => {
                    #variant_impl_body
                }));
                full_macro_variant_args.extend(quote!(#variant_ident #macro_arg_generics => {
                    #full_variant_impl_body
                }));
                macro_variant_default_args
                    .extend(quote!(#variant_ident #macro_default_arg_generics => {}));
                let mut impl_generics = TokenStream::new();
                if !impl_generic_params.is_empty() {
                    variant
                        .generics
                        .lt_token
                        .or(self.trait_def.generics.lt_token)
                        .to_tokens(&mut impl_generics);
                    impl_generics.append_separated(impl_generic_params, <Token![,]>::default());
                    variant
                        .generics
                        .gt_token
                        .or(self.trait_def.generics.gt_token)
                        .to_tokens(&mut impl_generics);
                }
                let trait_generic_args = &output_variant.variant.trait_args;
                let mut impl_args = TokenStream::new();
                if !impl_generic_args.is_empty() {
                    variant.generics.lt_token.to_tokens(&mut impl_args);
                    impl_args.append_separated(impl_generic_args, <Token![,]>::default());
                    variant.generics.gt_token.to_tokens(&mut impl_args);
                }
                if independent_impls {
                    let impl_signature = quote!(
                        impl #impl_generics #name_param #trait_generic_args for $(#ref_path_param::)*#variant_ident #impl_args
                    );
                    macro_body.extend(quote! {
                        #impl_signature {
                            #variant_impl_body
                        }
                    });
                    full_macro_body.extend(quote! {
                        #impl_signature {
                            #full_variant_impl_body
                        }
                    });
                }
            }
        }
        tokens.extend(quote! {
            #[macro_export]
            macro_rules! #impl_body_macro_ident {
                #impl_body_macro_contents
                ([], $_VariantIdx:literal, #macro_params_base) => {};
            }
            pub use #impl_body_macro_ident;
        });
        if !independent_impls {
            if let TraitContents::Alias { path } = &self.trait_def.contents {
                let mut path = path.extract_path();
                if let Some(segment) = path.segments.last_mut() {
                    segment.ident = Self::impl_macro_ident(&segment.ident);
                    segment.arguments = PathArguments::None;
                }
                let mut ref_path = TokenStream::new();
                Self::output_ref_path(&path, &mut ref_path);
                macro_body.extend(quote!(
                    #path!([Self], #name_param, #ref_path, #macro_parent_type_bound_args #macro_variant_args);
                ));
                full_macro_body.extend(quote!(
                    #path!([Self], #name_param, #ref_path, #macro_parent_type_bound_args #full_macro_variant_args);
                ));
            }
        }
        let impl_macro_ident = Self::impl_macro_ident(trait_ident);
        let impl_macro_params_base = quote!([$(#part_param:tt)*], #macro_params_base);
        let impl_macro_default_args_base = quote!([$(#part_param)*], #macro_default_args_base);
        let macro_default_matcher = if macro_variant_params.is_empty() {
            TokenStream::new()
        } else {
            quote!((#impl_macro_params_base) => {
                $(#ref_path_param::)*#impl_macro_ident!(#impl_macro_default_args_base #macro_variant_default_args);
            };)
        };
        tokens.extend(quote! {
            #[macro_export]
            macro_rules! #impl_macro_ident {
                #macro_default_matcher
                (#impl_macro_params_base #macro_variant_params) => {
                    #macro_body
                };
            }
            pub use #impl_macro_ident;
        });

        // Here, we would like to invoke the macro which we have just constructed (similarly to
        // the invocation we output when defining a trait alias). Unfortunately, Rust Analyzer
        // currently doesn't seem to expand such invocations transparently enough, so that IDE
        // navigation fails. Therefore, we expand the macro ourselves, essentially duplicating
        // its contents in our output.
        tokens.extend(expand_macro_body(full_macro_body, &macro_args));
    }
}

#[derive(Clone)]
pub struct ImplVariant {
    pub impl_generics: Generics,
    pub trait_args: PathArguments,
    pub variant: TraitVariant,
}

pub struct OutputImplVariant {
    pub variant: ImplVariant,
    pub impl_items: ImplPartList<ImplItem>,
}

fn phantom_types(generics: &Generics) -> Punctuated<Field, Token![,]> {
    let mut types = Punctuated::new();
    types.push(parse_quote!(()));
    for param in &generics.params {
        match param {
            GenericParam::Type(type_param) => {
                let name = &type_param.ident;
                types.push(parse_quote!(::core::marker::PhantomData<#name>));
            }
            _ => {}
        }
    }
    types
}

pub struct ImplPartList<T>(Vec<ImplPart<T>>);

impl<T> ImplPartList<T> {
    pub fn new() -> Self {
        ImplPartList(vec![
            ImplPart {
                ident: None,
                items: Vec::new(),
            },
            ImplPart {
                ident: Some(self_type_ident(None)),
                items: Vec::new(),
            },
        ])
    }

    fn add_item(&mut self, part_ident: &Option<Ident>, item: T) {
        let part = if let Some(part) = self.0.iter_mut().find(|part| &part.ident == part_ident) {
            part
        } else {
            self.0.push(ImplPart {
                ident: part_ident.clone(),
                items: Vec::new(),
            });
            self.0.last_mut().unwrap()
        };
        part.items.push(item);
    }
}

struct ImplPart<T> {
    ident: Option<Ident>,
    items: Vec<T>,
}
