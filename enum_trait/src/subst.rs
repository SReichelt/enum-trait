use std::{borrow::Cow, mem::replace};
use syn::{punctuated::Punctuated, spanned::Spanned, visit_mut::*, *};

use crate::{expr::*, generics::*};

pub struct ParamSubst<'a, 'b> {
    pub param: &'a GenericParam,
    pub arg: ParamSubstArg<'b>,
    pub result: Result<bool>,
}

#[derive(Clone, Copy)]
pub enum ParamSubstArg<'a> {
    TestOnly,
    Param(&'a GenericParam),
    Arg(&'a GenericArgument),
}

impl VisitMut for ParamSubst<'_, '_> {
    fn visit_lifetime_mut(&mut self, i: &mut Lifetime) {
        if let GenericParam::Lifetime(lifetime_param) = self.param {
            if i.ident == lifetime_param.lifetime.ident {
                return self.subst(i, self.arg.get_lifetime());
            }
        }
        visit_lifetime_mut(self, i)
    }

    fn visit_expr_mut(&mut self, i: &mut Expr) {
        if let GenericParam::Const(const_param) = self.param {
            let ident = &const_param.ident;
            if let Expr::Path(expr_path) = &i {
                if expr_path.qself.is_none() && expr_path.path.is_ident(ident) {
                    return self.subst(i, self.arg.get_expr());
                }
            } else if !matches!(self.arg, ParamSubstArg::TestOnly) {
                if let Some(arg) = self.substituted(self.arg.get_expr()) {
                    let ty = &const_param.ty;
                    *i = parse_quote!({
                        let #ident: #ty = #arg;
                        #i
                    });
                }
                return;
            }
        }
        visit_expr_mut(self, i)
    }

    fn visit_expr_path_mut(&mut self, i: &mut ExprPath) {
        if self.subst_in_path(&mut i.qself, &mut i.path) {
            return;
        }
        visit_expr_path_mut(self, i)
    }

    fn visit_type_mut(&mut self, i: &mut Type) {
        if let GenericParam::Type(type_param) = self.param {
            if let Type::Path(type_path) = &i {
                if type_path.qself.is_none() && type_path.path.is_ident(&type_param.ident) {
                    return self.subst(i, self.arg.get_type());
                }
            }
        }
        visit_type_mut(self, i)
    }

    fn visit_type_path_mut(&mut self, i: &mut TypePath) {
        if self.subst_in_path(&mut i.qself, &mut i.path) {
            return;
        }
        visit_type_path_mut(self, i)
    }

    fn visit_lifetime_param_mut(&mut self, i: &mut LifetimeParam) {
        // Need to override this to prevent `visit_lifetime_mut` from being called on the param.
        for attr in &mut i.attrs {
            self.visit_attribute_mut(attr);
        }
        for lifetime in &mut i.bounds {
            self.visit_lifetime_mut(lifetime);
        }
    }

    fn visit_generic_argument_mut(&mut self, i: &mut GenericArgument) {
        if let GenericArgument::Type(ty) = &i {
            if let GenericParam::Const(const_param) = self.param {
                if let Type::Path(type_path) = ty {
                    if type_path.qself.is_none() && type_path.path.is_ident(&const_param.ident) {
                        if let Some(arg) = self.substituted(self.arg.get_expr()) {
                            *i = GenericArgument::Const(arg);
                        }
                        return;
                    }
                }
            }
        }
        visit_generic_argument_mut(self, i)
    }
}

impl<'a, 'b> ParamSubst<'a, 'b> {
    fn new(param: &'a GenericParam, arg: ParamSubstArg<'b>) -> Self {
        ParamSubst {
            param,
            arg,
            result: Ok(false),
        }
    }

    fn substituted<T>(&mut self, arg: Result<Option<T>>) -> Option<T> {
        match arg {
            Ok(arg) => {
                if let Ok(result) = &mut self.result {
                    *result = true;
                }
                arg
            }
            Err(error) => {
                if self.result.is_ok() {
                    self.result = Err(error);
                }
                None
            }
        }
    }

    fn subst<T>(&mut self, node: &mut T, arg: Result<Option<T>>) {
        if let Some(arg) = self.substituted(arg) {
            *node = arg;
        }
    }

    fn subst_with_generics(&mut self, generics: &mut Generics, mut f: impl FnMut(&mut ParamSubst)) {
        // Substitute only if none of our generic parameters shadow `param`.
        if !param_generics_name_conflict(&self.param, generics) {
            match rename_conflicting_params(
                generics,
                |param| match &self.arg {
                    ParamSubstArg::TestOnly => Ok(false),
                    ParamSubstArg::Param(arg) => Ok(param_name_conflict(param, arg)),
                    ParamSubstArg::Arg(arg) => (*arg).clone().references_param(param),
                },
                &mut f,
            ) {
                Ok(true) => {
                    if let Ok(result) = &mut self.result {
                        *result = true;
                    }
                }
                Ok(false) => {}
                Err(error) => {
                    if self.result.is_ok() {
                        self.result = Err(error);
                    }
                }
            }
            self.visit_generics_mut(generics);
            f(self);
        }
    }

    fn subst_in_path(&mut self, qself: &mut Option<QSelf>, path: &mut Path) -> bool {
        if qself.is_none() && path.leading_colon.is_none() {
            if let GenericParam::Type(type_param) = self.param {
                if let Some(first) = path.segments.first_mut() {
                    if first.arguments.is_empty() && first.ident == type_param.ident {
                        match self.arg {
                            ParamSubstArg::TestOnly => {}
                            ParamSubstArg::Param(arg) => {
                                if let GenericParam::Type(arg) = arg {
                                    first.ident = arg.ident.clone();
                                } else if self.result.is_ok() {
                                    self.result =
                                        Err(Error::new(arg.span(), "non-type arg for type param"));
                                }
                            }
                            ParamSubstArg::Arg(arg) => {
                                if let GenericArgument::Type(arg) = arg {
                                    *qself = Some(QSelf {
                                        lt_token: Default::default(),
                                        ty: Box::new(arg.clone()),
                                        position: 0,
                                        as_token: None,
                                        gt_token: Default::default(),
                                    });
                                    path.leading_colon = Some(Default::default());
                                    let old_segments =
                                        replace(&mut path.segments, Punctuated::new());
                                    for pair in old_segments.into_pairs().skip(1) {
                                        let (segment, punct) = pair.into_tuple();
                                        path.segments.push_value(segment);
                                        if let Some(punct) = punct {
                                            path.segments.push_punct(punct);
                                        }
                                    }
                                    if path.segments.is_empty() {
                                        self.result = Err(Error::new(
                                            arg.span(),
                                            "cannot replace single-segment path with type arg",
                                        ));
                                    }
                                } else if self.result.is_ok() {
                                    self.result =
                                        Err(Error::new(arg.span(), "non-type arg for type param"));
                                }
                            }
                        }
                        if let Ok(result) = &mut self.result {
                            *result = true;
                        }
                        return true;
                    }
                }
            }
        }
        false
    }
}

impl ParamSubstArg<'_> {
    fn get_lifetime(&self) -> Result<Option<Lifetime>> {
        match self {
            ParamSubstArg::TestOnly => Ok(None),
            ParamSubstArg::Param(arg) => {
                if let GenericParam::Lifetime(arg) = arg {
                    Ok(Some(arg.lifetime.clone()))
                } else {
                    Err(Error::new(
                        arg.span(),
                        "non-lifetime arg for lifetime param",
                    ))
                }
            }
            ParamSubstArg::Arg(arg) => {
                if let GenericArgument::Lifetime(arg) = arg {
                    Ok(Some(arg.clone()))
                } else {
                    Err(Error::new(
                        arg.span(),
                        "non-lifetime arg for lifetime param",
                    ))
                }
            }
        }
    }

    fn get_expr(&self) -> Result<Option<Expr>> {
        match self {
            ParamSubstArg::TestOnly => Ok(None),
            ParamSubstArg::Param(arg) => {
                if let GenericParam::Const(arg) = arg {
                    Ok(Some(Expr::Path(ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: arg.ident.clone().into(),
                    })))
                } else {
                    Err(Error::new(arg.span(), "non-const arg for const param"))
                }
            }
            ParamSubstArg::Arg(arg) => {
                if let GenericArgument::Const(arg) = arg {
                    Ok(Some(arg.clone()))
                } else {
                    Err(Error::new(arg.span(), "non-const arg for const param"))
                }
            }
        }
    }

    fn get_type(&self) -> Result<Option<Type>> {
        match self {
            ParamSubstArg::TestOnly => Ok(None),
            ParamSubstArg::Param(arg) => {
                if let GenericParam::Type(arg) = arg {
                    Ok(Some(Type::Path(TypePath {
                        qself: None,
                        path: arg.ident.clone().into(),
                    })))
                } else {
                    Err(Error::new(arg.span(), "non-type arg for type param"))
                }
            }
            ParamSubstArg::Arg(arg) => {
                if let GenericArgument::Type(arg) = arg {
                    Ok(Some(arg.clone()))
                } else {
                    Err(Error::new(arg.span(), "non-type arg for type param"))
                }
            }
        }
    }
}

pub trait Substitutable: Sized {
    fn substitute_impl(&mut self, subst: &mut ParamSubst);

    fn substitute(&mut self, param: &GenericParam, arg: ParamSubstArg) -> Result<bool> {
        let mut subst = ParamSubst::new(param, arg);
        self.substitute_impl(&mut subst);
        subst.result
    }

    fn substitute_all(
        &mut self,
        generics: &Generics,
        args: &AngleBracketedGenericArguments,
    ) -> Result<bool> {
        let mut substituted = false;
        let mut args_iter = args.args.iter();
        for param in &generics.params {
            let Some(arg) = args_iter.next() else {
                return Err(Error::new(args.span(), "too few arguments"));
            };
            substituted |= self.substitute(param, ParamSubstArg::Arg(arg))?;
        }
        if let Some(arg) = args_iter.next() {
            return Err(Error::new(arg.span(), "superfluous argument"));
        }
        Ok(substituted)
    }

    fn substitute_all_params(&mut self, generics: &Generics, args: &Generics) -> Result<bool> {
        let mut substituted = false;
        let mut args_iter = args.params.iter();
        for param in &generics.params {
            let Some(arg) = args_iter.next() else {
                return Err(Error::new(args.span(), "too few parameters"));
            };
            substituted |= self.substitute(param, ParamSubstArg::Param(arg))?;
        }
        if let Some(arg) = args_iter.next() {
            return Err(Error::new(arg.span(), "superfluous parameter"));
        }
        Ok(substituted)
    }

    fn references_param(&mut self, param: &GenericParam) -> Result<bool> {
        let mut subst = ParamSubst::new(param, ParamSubstArg::TestOnly);
        self.substitute_impl(&mut subst);
        subst.result
    }
}

impl Substitutable for Expr {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.visit_expr_mut(self);
    }
}

impl Substitutable for Type {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.visit_type_mut(self);
    }
}

impl Substitutable for GenericArgument {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.visit_generic_argument_mut(self);
    }
}

impl<E: Substitutable> Substitutable for TypeLevelExpr<E> {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        match self {
            TypeLevelExpr::Expr(expr) => expr.substitute_impl(subst),
            TypeLevelExpr::Match(match_expr) => match_expr.substitute_impl(subst),
        }
    }
}

impl<E: Substitutable> Substitutable for TypeLevelExprMatch<E> {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        self.ty.substitute_impl(subst);
        for arm in &mut self.arms {
            arm.substitute_impl(subst);
        }
    }
}

impl<E: Substitutable> Substitutable for TypeLevelArm<E> {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.subst_with_generics(&mut self.variant_generics, |subst| {
            self.body.substitute_impl(subst)
        })
    }
}

impl Substitutable for ImplItem {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        match self {
            ImplItem::Const(item_const) => item_const.substitute_impl(subst),
            ImplItem::Fn(item_fn) => item_fn.substitute_impl(subst),
            ImplItem::Type(item_type) => item_type.substitute_impl(subst),
            _ => panic!("don't know how to substitute in impl item"),
        }
    }
}

impl Substitutable for ImplItemConst {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.subst_with_generics(&mut self.generics, |subst| {
            subst.visit_type_mut(&mut self.ty);
            subst.visit_expr_mut(&mut self.expr);
        })
    }
}

impl Substitutable for ImplItemFn {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.subst_with_generics(&mut self.sig.generics, |subst| {
            for mut el in Punctuated::pairs_mut(&mut self.sig.inputs) {
                let it = el.value_mut();
                subst.visit_fn_arg_mut(it);
            }
            if let Some(it) = &mut self.sig.variadic {
                subst.visit_variadic_mut(it);
            }
            subst.visit_return_type_mut(&mut self.sig.output);
            subst.visit_block_mut(&mut self.block);
        })
    }
}

impl Substitutable for ImplItemType {
    fn substitute_impl(&mut self, subst: &mut ParamSubst) {
        subst.subst_with_generics(&mut self.generics, |subst| {
            subst.visit_type_mut(&mut self.ty);
        })
    }
}

fn param_name_conflict(param1: &GenericParam, param2: &GenericParam) -> bool {
    match (param1, param2) {
        (GenericParam::Lifetime(lifetime_param1), GenericParam::Lifetime(lifetime_param2)) => {
            lifetime_param1.lifetime == lifetime_param2.lifetime
        }
        (GenericParam::Type(type_param1), GenericParam::Type(type_param2)) => {
            type_param1.ident == type_param2.ident
        }
        (GenericParam::Type(type_param1), GenericParam::Const(const_param2)) => {
            type_param1.ident == const_param2.ident
        }
        (GenericParam::Const(const_param1), GenericParam::Type(type_param2)) => {
            const_param1.ident == type_param2.ident
        }
        (GenericParam::Const(const_param1), GenericParam::Const(const_param2)) => {
            const_param1.ident == const_param2.ident
        }
        _ => false,
    }
}

fn param_generics_name_conflict(param: &GenericParam, generics: &Generics) -> bool {
    generics
        .params
        .iter()
        .any(|generic_param| param_name_conflict(param, generic_param))
}

fn param_context_name_conflict(param: &GenericParam, mut context: &GenericsContext) -> bool {
    loop {
        match context {
            GenericsContext::Empty => return false,
            GenericsContext::WithSelf(self_param, next_context) => {
                if param_name_conflict(param, self_param) {
                    return true;
                }
                context = next_context;
            }
            GenericsContext::WithGenerics(generics, next_context) => {
                if param_generics_name_conflict(param, generics) {
                    return true;
                }
                context = next_context;
            }
        }
    }
}

pub fn rename_conflicting_param(
    param: &mut GenericParam,
    mut conflicting: impl FnMut(&GenericParam) -> Result<bool>,
    mut substitute: impl FnMut(&mut ParamSubst),
) -> Result<bool> {
    if conflicting(param)? {
        let old_param = param.clone();
        loop {
            match param {
                GenericParam::Lifetime(lifetime_param) => {
                    add_underscore_suffix(&mut lifetime_param.lifetime.ident)
                }
                GenericParam::Type(type_param) => add_underscore_suffix(&mut type_param.ident),
                GenericParam::Const(const_param) => add_underscore_suffix(&mut const_param.ident),
            }
            if !conflicting(param)? {
                break;
            }
        }
        let mut subst = ParamSubst::new(&old_param, ParamSubstArg::Param(param));
        substitute(&mut subst);
        subst.result?;
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn rename_conflicting_params(
    generics: &mut Generics,
    mut conflicting: impl FnMut(&GenericParam) -> Result<bool>,
    mut substitute: impl FnMut(&mut ParamSubst),
) -> Result<bool> {
    let mut renamed = false;
    for param in &mut generics.params {
        renamed |= rename_conflicting_param(param, &mut conflicting, &mut substitute)?;
    }
    Ok(renamed)
}

fn add_underscore_suffix(ident: &mut Ident) {
    *ident = ident_with_suffix(ident, "_")
}

fn build_indirection_impl<'a, E: Substitutable, R>(
    expr: &mut E,
    context: &'a GenericsContext<'a>,
    shadowing_params: &GenericsContext,
    on_empty: impl FnOnce(&mut E) -> Result<R>,
    mut on_generics: impl FnMut(
        &mut E,
        &'a Generics,
        &'a GenericsContext<'a>,
        &GenericsContext,
    ) -> Result<
        Option<(
            R,
            Punctuated<GenericParam, Token![,]>,
            Punctuated<GenericArgument, Token![,]>,
        )>,
    >,
) -> Result<(
    R,
    Punctuated<GenericParam, Token![,]>,
    Punctuated<GenericArgument, Token![,]>,
)> {
    match context {
        GenericsContext::Empty => {
            let result = on_empty(expr)?;
            Ok((result, Punctuated::new(), Punctuated::new()))
        }
        GenericsContext::WithSelf(param, next_context) => {
            let next_shadowing_params =
                GenericsContext::WithSelf(Cow::Borrowed(param), shadowing_params);
            let (result, mut params, mut args) = build_indirection_impl(
                expr,
                next_context,
                &next_shadowing_params,
                on_empty,
                on_generics,
            )?;
            add_param_indirection(expr, param, &mut params, &mut args, &next_shadowing_params)?;
            Ok((result, params, args))
        }
        GenericsContext::WithGenerics(generics, next_context) => {
            if let Some(result) = on_generics(expr, generics, next_context, shadowing_params)? {
                return Ok(result);
            }
            let next_shadowing_params = GenericsContext::WithGenerics(generics, shadowing_params);
            let (result, mut params, mut args) = build_indirection_impl(
                expr,
                next_context,
                &next_shadowing_params,
                on_empty,
                on_generics,
            )?;
            for param in &generics.params {
                add_param_indirection(expr, param, &mut params, &mut args, shadowing_params)?;
            }
            Ok((result, params, args))
        }
    }
}

fn add_param_indirection(
    expr: &mut impl Substitutable,
    param: &GenericParam,
    params: &mut Punctuated<GenericParam, Token![,]>,
    args: &mut Punctuated<GenericArgument, Token![,]>,
    shadowing_params: &GenericsContext,
) -> Result<()> {
    if expr.references_param(param)? {
        let mut param = param.clone();
        let arg = generic_param_arg(&param);
        rename_conflicting_param(
            &mut param,
            |param| Ok(param_context_name_conflict(param, shadowing_params)),
            |subst| expr.substitute_impl(subst),
        )?;
        params.push(param);
        args.push(arg);
    }
    Ok(())
}

pub fn isolate_type_param<'a>(
    expr: &mut impl Substitutable,
    context: &'a GenericsContext<'a>,
    param_ident: &Ident,
) -> Result<(&'a TypeParam, Generics, PathArguments)> {
    let (type_param, params, args) = build_indirection_impl(
        expr,
        context,
        &GenericsContext::Empty,
        |_| {
            Err(Error::new(
                param_ident.span(),
                format!("type param '{param_ident}' not found"),
            ))
        },
        |expr, generics, next_context, shadowing_params| {
            let next_shadowing_params = GenericsContext::WithGenerics(generics, shadowing_params);
            for (param_idx, param) in generics.params.iter().enumerate() {
                match param {
                    GenericParam::Lifetime(_) => {}
                    GenericParam::Type(type_param) => {
                        if &type_param.ident == param_ident {
                            let (_, mut params, mut args) = build_indirection_impl(
                                expr,
                                next_context,
                                &next_shadowing_params,
                                |_| Ok(()),
                                |_, _, _, _| Ok(None),
                            )?;
                            for (param2_idx, param2) in generics.params.iter().enumerate() {
                                if param2_idx != param_idx {
                                    add_param_indirection(
                                        expr,
                                        param2,
                                        &mut params,
                                        &mut args,
                                        shadowing_params,
                                    )?;
                                }
                            }
                            expr.substitute(
                                param,
                                ParamSubstArg::Param(&self_type_param(
                                    param.span(),
                                    type_param.bounds.clone(),
                                )),
                            )?;
                            return Ok(Some((type_param, params, args)));
                        }
                    }
                    GenericParam::Const(const_param) => {
                        if &const_param.ident == param_ident {
                            return Err(Error::new(
                                param_ident.span(),
                                format!(
                                    "type param expected, but '{param_ident}' is a const param"
                                ),
                            ));
                        }
                    }
                }
            }
            Ok(None)
        },
    )?;
    let (lt_token, gt_token) = if params.is_empty() {
        (None, None)
    } else {
        (Some(Default::default()), Some(Default::default()))
    };
    let generics = Generics {
        lt_token,
        params,
        gt_token,
        where_clause: None,
    };
    let arguments = if args.is_empty() {
        PathArguments::None
    } else {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: Default::default(),
            args,
            gt_token: Default::default(),
        })
    };
    Ok((type_param, generics, arguments))
}

#[cfg(test)]
#[rustfmt::skip::macros(parse_quote)]
mod tests {
    use quote::ToTokens;

    use super::*;

    #[test]
    fn lifetime_param_subst_param() {
        let param = GenericParam::Lifetime(parse_quote!('a));
        let arg = GenericParam::Lifetime(parse_quote!('x));
        let arg = ParamSubstArg::Param(&arg);
        let assert_subst_type = |mut ty: Type, result: Type| {
            ty.substitute(&param, arg).unwrap();
            assert_eq_tokens(&ty, &result);
        };
        assert_subst_type(parse_quote!(A<'a>), parse_quote!(A<'x>));
        assert_subst_type(parse_quote!(A<'b>), parse_quote!(A<'b>));
        let assert_subst_type_level_expr =
            |mut ty: TypeLevelExpr<Type>, result: TypeLevelExpr<Type>| {
                ty.substitute(&param, arg).unwrap();
                assert_eq_tokens(&ty, &result);
            };
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C => A, D => B, E => F<'b, 'a, A> }),
            parse_quote!(match <B> { C => A, D => B, E => F<'b, 'x, A> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<'b, 'a, D> => F<'a> }),
            parse_quote!(match <B> { C<'b, 'a, D> => F<'a> }),
        );
    }

    #[test]
    fn type_param_subst_param() {
        let param = GenericParam::Type(parse_quote!(A));
        let arg = GenericParam::Type(parse_quote!(X));
        let arg = ParamSubstArg::Param(&arg);
        let assert_subst_type = |mut ty: Type, result: Type| {
            ty.substitute(&param, arg).unwrap();
            assert_eq_tokens(&ty, &result);
        };
        assert_subst_type(parse_quote!(A), parse_quote!(X));
        assert_subst_type(parse_quote!(B), parse_quote!(B));
        assert_subst_type(parse_quote!(A::C), parse_quote!(X::C));
        assert_subst_type(parse_quote!(<A>::C), parse_quote!(<X>::C));
        assert_subst_type(parse_quote!(B::C), parse_quote!(B::C));
        assert_subst_type(parse_quote!(B::A), parse_quote!(B::A));
        assert_subst_type(
            parse_quote!(F<A,B,G<A::C::D>,E>),
            parse_quote!(F<X,B,G<X::C::D>,E>),
        );
        let assert_subst_expr = |mut expr: Expr, result: Expr| {
            expr.substitute(&param, arg).unwrap();
            assert_eq_tokens(&expr, &result);
        };
        assert_subst_expr(parse_quote!(A::C), parse_quote!(X::C));
        assert_subst_expr(parse_quote!(B::C), parse_quote!(B::C));
        let assert_subst_type_level_expr =
            |mut ty: TypeLevelExpr<Type>, result: TypeLevelExpr<Type>| {
                ty.substitute(&param, arg).unwrap();
                assert_eq_tokens(&ty, &result);
            };
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C => A, D => B, E => F<A> }),
            parse_quote!(match <B> { C => X, D => B, E => F<X> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> {
                C => match <C> {
                    D => A,
                },
            }),
            parse_quote!(match <B> {
                C => match <C> {
                    D => X,
                },
            }),
        );
        assert_subst_type_level_expr(parse_quote!(match <A> {}), parse_quote!(match <X> {}));
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, E: F<A>> => A }),
            parse_quote!(match <B> { C<D, E: F<X>> => X }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, A, E: F<A>> => A }),
            parse_quote!(match <B> { C<D, A, E: F<A>> => A }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, X> => (A, X) }),
            parse_quote!(match <B> { C<D, X_> => (X, X_) }),
        );
    }

    #[test]
    fn type_param_subst_tuple() {
        let param = GenericParam::Type(parse_quote!(A));
        let arg = GenericArgument::Type(parse_quote!((X, Y)));
        let arg = ParamSubstArg::Arg(&arg);
        let assert_subst_type = |mut ty: Type, result: Type| {
            ty.substitute(&param, arg).unwrap();
            assert_eq_tokens(&ty, &result);
        };
        assert_subst_type(parse_quote!(A), parse_quote!((X, Y)));
        assert_subst_type(parse_quote!(B), parse_quote!(B));
        assert_subst_type(parse_quote!(::A), parse_quote!(::A));
        assert_subst_type(parse_quote!(A::C), parse_quote!(<(X, Y)>::C));
        assert_subst_type(parse_quote!(<A>::C), parse_quote!(<(X, Y)>::C));
        assert_subst_type(parse_quote!(B::C), parse_quote!(B::C));
        assert_subst_type(parse_quote!(B::A), parse_quote!(B::A));
        assert_subst_type(
            parse_quote!(F<A,B,G<A::C::D>,E>),
            parse_quote!(F<(X, Y),B,G<<(X, Y)>::C::D>,E>),
        );
        let assert_subst_expr = |mut expr: Expr, result: Expr| {
            expr.substitute(&param, arg).unwrap();
            assert_eq_tokens(&expr, &result);
        };
        assert_subst_expr(parse_quote!(A::C), parse_quote!(<(X, Y)>::C));
        assert_subst_expr(parse_quote!(B::C), parse_quote!(B::C));
        let assert_subst_type_level_expr =
            |mut ty: TypeLevelExpr<Type>, result: TypeLevelExpr<Type>| {
                ty.substitute(&param, arg).unwrap();
                assert_eq_tokens(&ty, &result);
            };
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C => A, D => B, E => F<A> }),
            parse_quote!(match <B> { C => (X, Y), D => B, E => F<(X, Y)> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> {
                C => match <C> {
                    D => A,
                },
            }),
            parse_quote!(match <B> {
                C => match <C> {
                    D => (X, Y),
                },
            }),
        );
        assert_subst_type_level_expr(parse_quote!(match <A> {}), parse_quote!(match <(X, Y)> {}));
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, E: F<A>> => A }),
            parse_quote!(match <B> { C<D, E: F<(X, Y)>> => (X, Y) }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, A, E: F<A>> => A }),
            parse_quote!(match <B> { C<D, A, E: F<A>> => A }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, X> => (A, X) }),
            parse_quote!(match <B> { C<D, X_> => ((X, Y), X_) }),
        );
    }

    #[test]
    fn expr_param_subst_param() {
        let param = GenericParam::Const(parse_quote!(const A: T));
        let arg = GenericParam::Const(parse_quote!(const X: T));
        let arg = ParamSubstArg::Param(&arg);
        let assert_subst_expr = |mut expr: Expr, result: Expr| {
            expr.substitute(&param, arg).unwrap();
            assert_eq_tokens(&expr, &result);
        };
        assert_subst_expr(parse_quote!(A), parse_quote!(X));
        assert_subst_expr(parse_quote!(B), parse_quote!(B));
        assert_subst_expr(parse_quote!(::A), parse_quote!(::A));
        assert_subst_expr(parse_quote!(A::B), parse_quote!(A::B));
        assert_subst_expr(parse_quote!(B::A), parse_quote!(B::A));
        assert_subst_expr(
            parse_quote!(f(A, B(A(C)))),
            parse_quote!({
                let A: T = X;
                f(A, B(A(C)))
            }),
        );
        assert_subst_expr(
            parse_quote!(A(|A| 2 * A)),
            parse_quote!({
                let A: T = X;
                A(|A| 2 * A)
            }),
        );
        let assert_subst_type = |mut ty: Type, result: Type| {
            ty.substitute(&param, arg).unwrap();
            assert_eq_tokens(&ty, &result);
        };
        assert_subst_type(parse_quote!(F<A>), parse_quote!(F<X>));
        let assert_subst_type_level_expr =
            |mut expr: TypeLevelExpr<Expr>, result: TypeLevelExpr<Expr>| {
                expr.substitute(&param, arg).unwrap();
                assert_eq_tokens(&expr, &result);
            };
        assert_subst_type_level_expr(
            parse_quote!(match <B> {
                C => A,
                D => B,
                E => f(A),
            }),
            parse_quote!(match <B> {
                C => X,
                D => B,
                E => {
                    let A: T = X;
                    f(A)
                },
            }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> {
                C => match <C> {
                    D => A,
                },
            }),
            parse_quote!(match <B> {
                C => match <C> {
                    D => X,
                },
            }),
        );
        assert_subst_type_level_expr(parse_quote!(match <F<A>> {}), parse_quote!(match <F<X>> {}));
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, A, E: F<A>> => f::<A> }),
            parse_quote!(match <B> { C<D, A, E: F<A>> => f::<A> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, E: F<A>> => f::<A> }),
            parse_quote!(match <B> { C<D, E: F<X>> => f::<X> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, X> => f::<A, X> }),
            parse_quote!(match <B> { C<D, X_> => f::<X, X_> }),
        );
    }

    #[test]
    fn expr_param_subst_op() {
        let param = GenericParam::Const(parse_quote!(const A: T));
        let arg = GenericArgument::Const(parse_quote!(X + 42));
        let arg = ParamSubstArg::Arg(&arg);
        let assert_subst_expr = |mut expr: Expr, result: Expr| {
            expr.substitute(&param, arg).unwrap();
            assert_eq_tokens(&expr, &result);
        };
        assert_subst_expr(parse_quote!(A), parse_quote!(X + 42));
        assert_subst_expr(parse_quote!(B), parse_quote!(B));
        assert_subst_expr(parse_quote!(::A), parse_quote!(::A));
        assert_subst_expr(parse_quote!(A::B), parse_quote!(A::B));
        assert_subst_expr(parse_quote!(B::A), parse_quote!(B::A));
        assert_subst_expr(
            parse_quote!(f(A, B(A(C)))),
            parse_quote!({
                let A: T = X + 42;
                f(A, B(A(C)))
            }),
        );
        assert_subst_expr(
            parse_quote!(A(|A| 2 * A)),
            parse_quote!({
                let A: T = X + 42;
                A(|A| 2 * A)
            }),
        );
        let assert_subst_type = |mut ty: Type, result: Type| {
            ty.substitute(&param, arg).unwrap();
            assert_eq_tokens(&ty, &result);
        };
        assert_subst_type(parse_quote!(F<A>), parse_quote!(F<{ X + 42 }>));
        let assert_subst_type_level_expr =
            |mut expr: TypeLevelExpr<Expr>, result: TypeLevelExpr<Expr>| {
                expr.substitute(&param, arg).unwrap();
                assert_eq_tokens(&expr, &result);
            };
        assert_subst_type_level_expr(
            parse_quote!(match <B> {
                C => A,
                D => B,
                E => f(A),
            }),
            parse_quote!(match <B> {
                C => X + 42,
                D => B,
                E => {
                    let A: T = X + 42;
                    f(A)
                },
            }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> {
                C => match <C> {
                    D => A,
                },
            }),
            parse_quote!(match <B> {
                C => match <C> {
                    D => X + 42,
                },
            }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <F<A>> {}),
            parse_quote!(match <F<{ X + 42 }>> {}),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, A, E: F<A>> => f::<A> }),
            parse_quote!(match <B> { C<D, A, E: F<A>> => f::<A> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, E: F<A>> => f::<A> }),
            parse_quote!(match <B> { C<D, E: F<{ X + 42 }>> => f::<{ X + 42 }> }),
        );
        assert_subst_type_level_expr(
            parse_quote!(match <B> { C<D, X> => f::<A, X> }),
            parse_quote!(match <B> { C<D, X_> => f::<{ X + 42 }, X_> }),
        );
    }

    fn assert_eq_tokens<T: ToTokens>(t1: &T, t2: &T) {
        assert_eq!(
            t1.to_token_stream().to_string(),
            t2.to_token_stream().to_string(),
        );
    }
}
