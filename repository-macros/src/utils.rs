use proc_macro::Ident;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse::Parse;
use syn::spanned::Spanned;
use syn::Item;

macro_rules! delegate_single_fn_impl {
    (of $trait_path:path; fn $fn_name:ident($($x:tt)*) $(-> $ret_ty:ty)?;
    ) => {};
    (of $trait_path:path; fn $fn_name:ident($($x:tt)*) $(-> $ret_ty:ty)?;
     $ty_path:path => $delegate_expr:expr $(,)?
    ) => {
        impl $trait_path for $ty_path {
            fn $fn_name($($x)*) $(-> $ret_ty)? {
                $delegate_expr
            }
        }
    };
    (of $trait_path:path; fn $fn_name:ident($($x:tt)*) $(-> $ret_ty:ty)?;
       $ty_path:path => $delegate_expr:expr, $($tail:tt)*
    ) => {
        impl $trait_path for $ty_path {
            fn $fn_name($($x)*) $(-> $ret_ty)? {
                $delegate_expr
            }
        }
        delegate_single_fn_impl!(
            of $trait_path; fn $fn_name($($x)*) $(-> $ret_ty)?;
            $($tail)*
        );
    };

    (inherent; $v:vis fn $fn_name:ident($($x:tt)*) $(-> $ret_ty:ty)?;
    ) => {};
    (inherent; $v:vis fn $fn_name:ident($($x:tt)*) $(-> $ret_ty:ty)?;
     $ty_path:path => $delegate_expr:expr $(,)?
    ) => {
        impl $ty_path {
            $v fn $fn_name($($x)*) $(-> $ret_ty)? {
                $delegate_expr
            }
        }
    };
    (inherent; $v:vis fn $fn_name:ident($($x:tt)*) $(-> $ret_ty:ty)?;
       $ty_path:path => $delegate_expr:expr, $($tail:tt)*
    ) => {
        impl $ty_path {
            $v fn $fn_name($($x)*) $(-> $ret_ty)? {
                $delegate_expr
            }
        }
        delegate_single_fn_impl!(
            inherent; $v fn $fn_name($($x)*) $(-> $ret_ty)?;
            $($tail)*
        );
    };
}

pub(crate) trait RetainOrTake<T> {
    type Drain<'a>
    where
        Self: 'a;
    fn retain_or_take<F>(&mut self, f: F) -> Self::Drain<'_>
    where
        F: FnMut(&mut T) -> bool;
}

impl<T> RetainOrTake<T> for Vec<T> {
    type Drain<'a>  = std::vec::Drain<'a, T> where Self: 'a;

    fn retain_or_take<F>(&mut self, mut f: F) -> Self::Drain<'_>
    where
        F: FnMut(&mut T) -> bool,
    {
        let len = self.len();
        // let [mut write_start, mut write_pos, mut read_pos, len] = [0, 0, 0, self.len()];
        if len == 0 {
            return self.drain(..);
        }
        let [mut write_start, mut write_len] = [0, 0];
        let mut retain_these = f(&mut self[0]);
        let mut read_pos = 0;
        let mut read_end = read_pos + 1;
        loop {
            while read_end < len && f(&mut self[read_end]) == retain_these {
                read_end += 1;
            }

            if retain_these {
                // move these before write_start
                self[write_start..read_end].rotate_left(write_len);
                write_start = read_end - write_len;
            } else {
                // merge this into write_pos
                write_len = read_end - write_start;
            }

            if read_end == len {
                break;
            }
            retain_these = !retain_these;
            read_pos = read_end;
            read_end = read_pos + 1;
        }

        self.drain(write_start..)
    }
}

pub(crate) trait PushAndGetWithIndex {
    type Target;
    fn push_and_get_with_index(&mut self, v: Self::Target) -> (usize, &mut Self::Target);
}

impl<T> PushAndGetWithIndex for Vec<T> {
    type Target = T;
    fn push_and_get_with_index(&mut self, v: Self::Target) -> (usize, &mut Self::Target) {
        let idx = self.len();
        self.push(v);
        (idx, &mut self[idx])
    }
}

pub(crate) fn has_str_meta_in_args(args: &[GeneralizedMeta], v: &str) -> bool {
    args.into_iter().any(|nested_meta| match nested_meta {
        GeneralizedMeta::Path(p) => p.is_ident(v),
        _ => false,
    })
}

macro_rules! ensure_in_macro {
    ($cond:expr, $span:expr, $msg:expr) => {
        if !$cond {
            return TokenStream::from(syn::Error::new($span, $msg).to_compile_error());
        }
    };
}

macro_rules! unwrap_result_in_macro {
    ($value:expr) => {
        match $value {
            Ok(v) => v,
            Err(e) => return TokenStream::from(e.to_compile_error()),
        }
    };
}

macro_rules! validate_name_in_macro {
    ($i:expr) => {
        ensure_in_macro!(
            !$i.ident_text().contains('_'),
            $i.span(),
            "cannot use underscore in name"
        );
    };
}

macro_rules! ident_from_combining {
    ($span:expr => $a:expr, $b:expr) => {
        syn::Ident::new_raw(
            &format!(
                "{}_{}",
                crate::utils::IdentText::ident_text(&$a),
                crate::utils::IdentText::ident_text(&$b)
            ),
            $span,
        )
    };
    ($span:expr => $a:expr, $b:expr, $c:expr) => {
        syn::Ident::new_raw(
            &format!(
                "{}_{}_{}",
                crate::utils::IdentText::ident_text(&$a),
                crate::utils::IdentText::ident_text(&$b),
                crate::utils::IdentText::ident_text(&$c)
            ),
            $span,
        )
    };
}

#[derive(Debug)]
pub(crate) enum GeneralizedMeta {
    Lit(syn::Lit),

    Path(syn::Path),

    List(GeneralizedMetaList),

    NamePath(MetaNamePathValue),

    NameLit(MetaNameLitValue),
}

impl Parse for GeneralizedMeta {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Lit) && !(input.peek(syn::LitBool) && input.peek2(syn::Token![=])) {
            input.parse().map(GeneralizedMeta::Lit)
        } else if let Ok(path) = input.call(Self::parse_meta_path) {
            Self::parse_after_path(path, input)
        } else {
            Err(input.error("expected identifier or literal"))
        }
    }
}

impl GeneralizedMeta {
    pub(crate) fn parse_attribute(attr: &syn::Attribute) -> syn::Result<Self> {
        fn clone_ident_segment(segment: &syn::PathSegment) -> syn::PathSegment {
            syn::PathSegment {
                ident: segment.ident.clone(),
                arguments: syn::PathArguments::None,
            }
        }
        let path = syn::Path {
            leading_colon: attr
                .path
                .leading_colon
                .as_ref()
                .map(|colon| syn::Token![::](colon.spans)),
            segments: attr
                .path
                .segments
                .pairs()
                .map(|pair| match pair {
                    syn::punctuated::Pair::Punctuated(seg, punct) => {
                        syn::punctuated::Pair::Punctuated(
                            clone_ident_segment(seg),
                            syn::Token![::](punct.spans),
                        )
                    }
                    syn::punctuated::Pair::End(seg) => {
                        syn::punctuated::Pair::End(clone_ident_segment(seg))
                    }
                })
                .collect(),
        };

        let parser = |input: syn::parse::ParseStream| Self::parse_after_path(path, input);
        syn::parse::Parser::parse2(parser, attr.tokens.clone())
    }

    fn parse_meta_path(input: syn::parse::ParseStream) -> syn::Result<syn::Path> {
        use syn::ext::IdentExt;
        Ok(syn::Path {
            leading_colon: input.parse()?,
            segments: {
                let mut segments = syn::punctuated::Punctuated::new();
                while input.peek(syn::Ident::peek_any) {
                    let ident = syn::Ident::parse_any(input)?;
                    segments.push_value(syn::PathSegment::from(ident));
                    if !input.peek(syn::Token![::]) {
                        break;
                    }
                    let punct = input.parse()?;
                    segments.push_punct(punct);
                }
                if segments.is_empty() {
                    return Err(input.error("expected path"));
                } else if segments.trailing_punct() {
                    return Err(input.error("expected path segment"));
                }
                segments
            },
        })
    }

    fn parse_after_path(path: syn::Path, input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Paren) {
            let content;
            Ok(GeneralizedMetaList {
                path,
                paren_token: syn::parenthesized!(content in input),
                nested: content.parse_terminated(GeneralizedMeta::parse)?,
            })
            .map(GeneralizedMeta::List)
        } else if input.peek(syn::Token![=]) {
            let eq_token = input.parse()?;
            if input.peek(syn::Lit) && !(input.peek(syn::LitBool) && input.peek2(syn::Token![=])) {
                Ok(MetaNameLitValue {
                    path,
                    eq_token,
                    lit: input.parse()?,
                })
                .map(GeneralizedMeta::NameLit)
            } else {
                let value_path = input.call(Self::parse_meta_path)?;
                Ok(MetaNamePathValue {
                    path,
                    eq_token,
                    value: value_path,
                })
                .map(GeneralizedMeta::NamePath)
            }
        } else {
            Ok(GeneralizedMeta::Path(path))
        }
    }
}

impl GeneralizedMeta {
    pub(crate) fn get_ident_value(&self) -> syn::Result<syn::Ident> {
        match self {
            GeneralizedMeta::NamePath(MetaNamePathValue { path, value, .. }) => {
                if let Some(value) = value.get_ident() {
                    Ok(value.clone())
                } else {
                    Err(syn::Error::new(
                        path.span(),
                        "should specify an ident for attribute",
                    ))
                }
            }
            GeneralizedMeta::Path(path)
            | GeneralizedMeta::List(GeneralizedMetaList { path, .. })
            | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. }) => Err(syn::Error::new(
                path.span(),
                "should specify an ident for attribute",
            )),
            GeneralizedMeta::Lit(lit) => Err(syn::Error::new(
                lit.span(),
                "should specify an ident for attribute",
            )),
        }
    }

    pub(crate) fn get_ident_values(&self) -> syn::Result<Vec<syn::Ident>> {
        match self {
            GeneralizedMeta::NamePath(MetaNamePathValue { path, value, .. }) => {
                if let Some(value) = value.get_ident() {
                    Ok(vec![value.clone()])
                } else {
                    Err(syn::Error::new(
                        path.span(),
                        "should specify an ident for attribute",
                    ))
                }
            }
            GeneralizedMeta::List(GeneralizedMetaList { path, nested, .. }) => nested
                .iter()
                .map(|meta| match meta {
                    GeneralizedMeta::Path(path) => {
                        if let Some(ident) = path.get_ident() {
                            Ok(ident.clone())
                        } else {
                            Err(syn::Error::new(
                                path.span(),
                                "should specify an ident for attribute",
                            ))
                        }
                    }
                    GeneralizedMeta::List(GeneralizedMetaList { path, .. })
                    | GeneralizedMeta::NamePath(MetaNamePathValue { path, .. })
                    | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. }) => Err(
                        syn::Error::new(path.span(), "should specify an ident for attribute"),
                    ),
                    GeneralizedMeta::Lit(lit) => Err(syn::Error::new(
                        lit.span(),
                        "should specify an ident for attribute",
                    )),
                })
                .collect(),
            GeneralizedMeta::Path(path)
            | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. }) => Err(syn::Error::new(
                path.span(),
                "should specify an ident for attribute",
            )),
            GeneralizedMeta::Lit(lit) => Err(syn::Error::new(
                lit.span(),
                "should specify an ident for attribute",
            )),
        }
    }

    pub(crate) fn get_path_values(&self) -> syn::Result<Vec<syn::Path>> {
        match self {
            GeneralizedMeta::NamePath(MetaNamePathValue { path, value, .. }) => {
                Ok(vec![value.clone()])
            }
            GeneralizedMeta::List(GeneralizedMetaList { path, nested, .. }) => nested
                .iter()
                .map(|meta| match meta {
                    GeneralizedMeta::Path(path) => Ok(path.clone()),
                    GeneralizedMeta::List(GeneralizedMetaList { path, .. })
                    | GeneralizedMeta::NamePath(MetaNamePathValue { path, .. })
                    | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. }) => Err(
                        syn::Error::new(path.span(), "should specify a path for attribute"),
                    ),
                    GeneralizedMeta::Lit(lit) => Err(syn::Error::new(
                        lit.span(),
                        "should specify a path for attribute",
                    )),
                })
                .collect(),
            GeneralizedMeta::Path(path)
            | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. }) => Err(syn::Error::new(
                path.span(),
                "should specify a path for attribute",
            )),
            GeneralizedMeta::Lit(lit) => Err(syn::Error::new(
                lit.span(),
                "should specify a path for attribute",
            )),
        }
    }
}

#[derive(Debug)]
pub(crate) struct GeneralizedMetaList {
    pub path: syn::Path,
    pub paren_token: syn::token::Paren,
    pub nested: syn::punctuated::Punctuated<GeneralizedMeta, syn::Token![,]>,
}

#[derive(Debug)]
pub(crate) struct MetaNamePathValue {
    pub(crate) path: syn::Path,
    pub(crate) eq_token: syn::Token![=],
    pub(crate) value: syn::Path,
}

type MetaNameLitValue = syn::MetaNameValue;

#[derive(Debug)]
pub(crate) struct AttributeArgs(Vec<GeneralizedMeta>);

impl AttributeArgs {
    pub(crate) fn get_ident_with_name(&self, name: &str) -> syn::Result<Option<syn::Ident>> {
        let mut result = None;
        let mut found = false;
        for meta in self.iter() {
            match meta {
                GeneralizedMeta::NamePath(MetaNamePathValue { path, value, .. })
                    if path.is_ident(name) =>
                {
                    if let Some(value) = value.get_ident() {
                        if found {
                            return Err(syn::Error::new(
                                path.span(),
                                "duplicate attribute specified",
                            ));
                        } else {
                            result = Some(value.clone());
                            found = true;
                        }
                    } else {
                        return Err(syn::Error::new(
                            path.span(),
                            "should specify an ident for attribute",
                        ));
                    }
                }
                GeneralizedMeta::Path(path)
                | GeneralizedMeta::List(GeneralizedMetaList { path, .. })
                | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. })
                    if path.is_ident(name) =>
                {
                    return Err(syn::Error::new(
                        path.span(),
                        "should specify an ident for attribute",
                    ))
                }
                _ => continue,
            }
        }
        Ok(result)
    }

    pub(crate) fn get_path_with_name(&self, name: &str) -> syn::Result<Option<syn::Path>> {
        let mut result = None;
        let mut found = false;
        for meta in self.iter() {
            match meta {
                GeneralizedMeta::NamePath(MetaNamePathValue { path, value, .. })
                    if path.is_ident(name) =>
                {
                    if found {
                        return Err(syn::Error::new(
                            path.span(),
                            "duplicate attribute specified",
                        ));
                    } else {
                        result = Some(value.clone());
                        found = true;
                    }
                }
                GeneralizedMeta::Path(path)
                | GeneralizedMeta::List(GeneralizedMetaList { path, .. })
                | GeneralizedMeta::NameLit(MetaNameLitValue { path, .. })
                    if path.is_ident(name) =>
                {
                    return Err(syn::Error::new(
                        path.span(),
                        "should specify a path for attribute",
                    ))
                }
                _ => continue,
            }
        }
        Ok(result)
    }
}

impl std::ops::Deref for AttributeArgs {
    type Target = Vec<GeneralizedMeta>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl syn::parse::Parse for AttributeArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut metas = Vec::new();

        loop {
            if input.is_empty() {
                break;
            }
            let value = input.parse()?;
            metas.push(value);
            if input.is_empty() {
                break;
            }
            input.parse::<syn::Token![,]>()?;
        }

        Ok(AttributeArgs(metas))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct IdentPair {
    pub(crate) snake_case: String,
    pub(crate) camel_case: String,
    pub(crate) span: proc_macro2::Span,
}

impl IdentPair {
    pub(crate) fn from_snake_case_ident(i: syn::Ident) -> Self {
        use heck::ToUpperCamelCase;
        let snake_case = i.ident_text();
        let camel_case = snake_case.to_upper_camel_case();
        IdentPair {
            snake_case,
            camel_case,
            span: i.span(),
        }
    }

    pub(crate) fn from_camel_case_ident(i: syn::Ident) -> Self {
        use heck::ToSnakeCase;
        let camel_case = i.ident_text();
        let snake_case = camel_case.to_snake_case();
        IdentPair {
            snake_case,
            camel_case,
            span: i.span(),
        }
    }

    pub(crate) fn span(&self) -> proc_macro2::Span {
        self.span.clone()
    }

    pub(crate) fn snake_case_ident(&self) -> syn::Ident {
        syn::Ident::new_raw(&self.snake_case, self.span)
    }

    pub(crate) fn camel_case_ident(&self) -> syn::Ident {
        syn::Ident::new_raw(&self.camel_case, self.span)
    }

    pub(crate) fn eq_camel_case_ident(&self, i: &syn::Ident) -> bool {
        self.camel_case == i.ident_text()
    }
}

pub(crate) trait IdentText {
    fn ident_text(&self) -> String;
}

impl IdentText for syn::Ident {
    fn ident_text(&self) -> String {
        let s = self.to_string();
        if let Some(s) = s.strip_prefix("r#") {
            s.to_owned()
        } else {
            s
        }
    }
}

impl IdentText for str {
    fn ident_text(&self) -> String {
        self.to_owned()
    }
}

impl IdentText for String {
    fn ident_text(&self) -> String {
        self.clone()
    }
}

impl<T> IdentText for &'_ T
where
    T: IdentText + ?Sized,
{
    fn ident_text(&self) -> String {
        (**self).ident_text()
    }
}

impl<T> IdentText for &'_ mut T
where
    T: IdentText + ?Sized,
{
    fn ident_text(&self) -> String {
        (**self).ident_text()
    }
}

pub(crate) trait TyWrap {
    fn wrap_ty_with_shared_ref(self) -> Self;
    fn wrap_ty_with_option(self) -> Self;
    fn wrap_ty_with_result(self, error_ty: Self) -> Self;
}

impl TyWrap for syn::Type {
    fn wrap_ty_with_shared_ref(self) -> Self {
        syn::Type::Reference(syn::TypeReference {
            and_token: Default::default(),
            lifetime: Default::default(),
            mutability: None,
            elem: Box::new(self),
        })
    }

    fn wrap_ty_with_option(self) -> Self {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter(vec![syn::PathSegment {
                    ident: syn::Ident::new("Option", self.span()),
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            colon2_token: Default::default(),
                            lt_token: Default::default(),
                            args: syn::punctuated::Punctuated::from_iter(vec![
                                syn::GenericArgument::Type(self),
                            ]),
                            gt_token: Default::default(),
                        },
                    ),
                }]),
            },
        })
    }

    fn wrap_ty_with_result(self, error_ty: Self) -> Self {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter(vec![syn::PathSegment {
                    ident: syn::Ident::new("Result", self.span()),
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            colon2_token: Default::default(),
                            lt_token: Default::default(),
                            args: syn::punctuated::Punctuated::from_iter(vec![
                                syn::GenericArgument::Type(self),
                                syn::GenericArgument::Type(error_ty),
                            ]),
                            gt_token: Default::default(),
                        },
                    ),
                }]),
            },
        })
    }
}

pub(crate) enum Either<L, R> {
    Left(L),
    Right(R),
}

pub(crate) trait StreamingIterator {
    type Item<'a>
    where
        Self: 'a;
    fn next(&mut self) -> Option<Self::Item<'_>>;
}
