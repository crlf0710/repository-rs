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

#[derive(Clone, Copy)]
pub(crate) enum ItemAdtRef<'a> {
    Struct(&'a syn::ItemStruct),
    Enum(&'a syn::ItemEnum),
    Union(&'a syn::ItemUnion),
}

pub(crate) enum ItemAdtMutRef<'a> {
    Struct(&'a mut syn::ItemStruct),
    Enum(&'a mut syn::ItemEnum),
    Union(&'a mut syn::ItemUnion),
}

impl ItemAdtMutRef<'_> {
    fn borrow(&self) -> ItemAdtRef<'_> {
        match self {
            ItemAdtMutRef::Struct(i) => ItemAdtRef::Struct(i),
            ItemAdtMutRef::Enum(i) => ItemAdtRef::Enum(i),
            ItemAdtMutRef::Union(i) => ItemAdtRef::Union(i),
        }
    }
}

mod adt;

pub(crate) use adt::{ItemStructOnly, ItemStructOrEnum, ItemStructOrEnumOrUnion};

impl quote::ToTokens for ItemAdtRef<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            ItemAdtRef::Struct(i) => i.to_tokens(tokens),
            ItemAdtRef::Enum(i) => i.to_tokens(tokens),
            ItemAdtRef::Union(i) => i.to_tokens(tokens),
        }
    }
}

delegate_single_fn_impl! {
    of quote::ToTokens;
    fn to_tokens(&self, tokens: &mut TokenStream2);
    ItemAdtMutRef<'_> => self.borrow().to_tokens(tokens),
    ItemStructOnly => self.borrow().to_tokens(tokens),
    ItemStructOrEnum => self.borrow().to_tokens(tokens),
    ItemStructOrEnumOrUnion => self.borrow().to_tokens(tokens),
}

impl ItemAdtRef<'_> {
    pub(crate) fn name(&self) -> syn::Ident {
        match self {
            ItemAdtRef::Struct(syn::ItemStruct { ident, .. })
            | ItemAdtRef::Enum(syn::ItemEnum { ident, .. })
            | ItemAdtRef::Union(syn::ItemUnion { ident, .. }) => ident.clone(),
        }
    }

    pub(crate) fn vis(&self) -> syn::Visibility {
        match self {
            ItemAdtRef::Struct(syn::ItemStruct { vis, .. })
            | ItemAdtRef::Enum(syn::ItemEnum { vis, .. })
            | ItemAdtRef::Union(syn::ItemUnion { vis, .. }) => vis.clone(),
        }
    }
}

delegate_single_fn_impl! {
    inherent;
    pub(crate) fn name(&self) -> syn::Ident;
    ItemAdtMutRef<'_> => self.borrow().name(),
    ItemStructOnly => self.borrow().name(),
    ItemStructOrEnum => self.borrow().name(),
    ItemStructOrEnumOrUnion => self.borrow().name(),
}

delegate_single_fn_impl! {
    inherent;
    pub(crate) fn vis(&self) -> syn::Visibility;
    ItemAdtMutRef<'_> => self.borrow().vis(),
    ItemStructOnly => self.borrow().vis(),
    ItemStructOrEnum => self.borrow().vis(),
    ItemStructOrEnumOrUnion => self.borrow().vis(),
}

impl<'a> ItemAdtMutRef<'a> {
    pub(crate) fn take_attr_with_ident_name(
        self,
        ident_name: &str,
    ) -> impl Iterator<Item = syn::Attribute> + 'a {
        match self {
            ItemAdtMutRef::Struct(syn::ItemStruct { attrs, .. })
            | ItemAdtMutRef::Enum(syn::ItemEnum { attrs, .. })
            | ItemAdtMutRef::Union(syn::ItemUnion { attrs, .. }) => {
                attrs.retain_or_take(|attr| !attr.path.is_ident(ident_name))
            }
        }
    }

    pub(crate) fn append_attrs(self, new_attrs: impl IntoIterator<Item = syn::Attribute>) {
        match self {
            ItemAdtMutRef::Struct(syn::ItemStruct { attrs, .. })
            | ItemAdtMutRef::Enum(syn::ItemEnum { attrs, .. })
            | ItemAdtMutRef::Union(syn::ItemUnion { attrs, .. }) => {
                attrs.extend(new_attrs.into_iter())
            }
        }
    }
}

delegate_single_fn_impl! {
    inherent;
    pub(crate) fn take_attr_with_ident_name(&mut self, ident_name: &str) -> impl Iterator<Item = syn::Attribute> + '_;
    ItemStructOnly => self.borrow_mut().take_attr_with_ident_name(ident_name),
    ItemStructOrEnum => self.borrow_mut().take_attr_with_ident_name(ident_name),
    ItemStructOrEnumOrUnion => self.borrow_mut ().take_attr_with_ident_name(ident_name),
}

#[derive(Debug)]
pub(crate) struct GeneralizedField {
    pub(crate) field: syn::Field,
    pub(crate) accessor: Option<IdentPair>,
    pub(crate) indexes_in_applicable_variants: Vec<usize>,
    pub(crate) flags: GeneralizedFieldFlags,
}

#[derive(Debug)]
pub(crate) enum GeneralizedFieldFlags {
    None,
    Getter,
}

impl GeneralizedField {
    pub(crate) fn new_from_syn_field(
        field: &mut syn::Field,
        index_in_first_applicable_variant: usize,
        in_union: bool,
    ) -> syn::Result<Self> {
        let accessor = field
            .attrs
            .retain_or_take(|attr| !attr.path.is_ident("accessor"))
            .map(|x| GeneralizedMeta::parse_attribute(&x).and_then(|x| x.get_ident_value()))
            .collect::<syn::Result<Vec<_>>>()?;
        let accessor = match &accessor[..] {
            [] => None,
            [x] => Some(x.clone()),
            [x, y, ..] => return Err(syn::Error::new(y.span(), "multiple accessor specified")),
        };
        let field = GeneralizedField {
            field: field.clone(),
            indexes_in_applicable_variants: vec![index_in_first_applicable_variant],
            accessor: accessor.map(IdentPair::from_snake_case_ident),
            flags: if !in_union {
                GeneralizedFieldFlags::Getter
            } else {
                GeneralizedFieldFlags::None
            },
        };
        Ok(field)
    }

    pub(crate) fn vis(&self) -> syn::Visibility {
        self.field.vis.clone()
    }

    pub(crate) fn has_getter(&self) -> bool {
        match self.flags {
            GeneralizedFieldFlags::Getter => true,
            GeneralizedFieldFlags::None => false,
        }
    }
}

trait RetainOrTake<T> {
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
            !$i.to_string().contains('_'),
            $i.span(),
            "cannot use underscore in name"
        );
    };
}

macro_rules! ident_from_combining {
    ($span:expr => $a:expr, $b:expr) => {
        syn::Ident::new(&format!("{}_{}", $a, $b), $span)
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

#[derive(Debug)]
pub(crate) struct IdentPair {
    pub(crate) snake_case: String,
    pub(crate) camel_case: String,
    pub(crate) span: proc_macro2::Span,
}

impl IdentPair {
    pub(crate) fn from_snake_case_ident(i: syn::Ident) -> Self {
        use heck::ToUpperCamelCase;
        let snake_case = i.to_string();
        let camel_case = snake_case.to_upper_camel_case();
        IdentPair {
            snake_case,
            camel_case,
            span: i.span(),
        }
    }

    pub(crate) fn from_camel_case_ident(i: syn::Ident) -> Self {
        use heck::ToSnakeCase;
        let camel_case = i.to_string();
        let snake_case = camel_case.to_snake_case();
        IdentPair {
            snake_case,
            camel_case,
            span: i.span(),
        }
    }

    pub(crate) fn snake_case_ident(&self) -> syn::Ident {
        syn::Ident::new_raw(&self.snake_case, self.span)
    }
}
