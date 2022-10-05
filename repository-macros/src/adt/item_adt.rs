use super::{ItemAdtMutRef, ItemAdtRef};
use syn::parse::Parse;

#[derive(Clone)]
pub(crate) enum ItemStructOnly {
    Struct(syn::ItemStruct),
}

#[derive(Clone)]
pub(crate) enum ItemStructOrEnum {
    Struct(syn::ItemStruct),
    Enum(syn::ItemEnum),
}

#[derive(Clone)]
pub(crate) enum ItemStructOrEnumOrUnion {
    Struct(syn::ItemStruct),
    Enum(syn::ItemEnum),
    Union(syn::ItemUnion),
}

impl Parse for ItemStructOnly {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item = syn::Item::parse(input)?;
        match item {
            syn::Item::Struct(i) => Ok(ItemStructOnly::Struct(i)),
            _ => Err(syn::Error::new(input.span(), "not a struct")),
        }
    }
}

impl Parse for ItemStructOrEnum {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item = syn::Item::parse(input)?;
        match item {
            syn::Item::Struct(i) => Ok(ItemStructOrEnum::Struct(i)),
            syn::Item::Enum(i) => Ok(ItemStructOrEnum::Enum(i)),
            _ => Err(syn::Error::new(input.span(), "not a struct or enum")),
        }
    }
}

impl Parse for ItemStructOrEnumOrUnion {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item = syn::Item::parse(input)?;
        match item {
            syn::Item::Struct(i) => Ok(ItemStructOrEnumOrUnion::Struct(i)),
            syn::Item::Enum(i) => Ok(ItemStructOrEnumOrUnion::Enum(i)),
            syn::Item::Union(i) => Ok(ItemStructOrEnumOrUnion::Union(i)),
            _ => Err(syn::Error::new(
                input.span(),
                "not a struct or enum or union",
            )),
        }
    }
}

impl ItemStructOnly {
    pub(crate) fn borrow(&self) -> ItemAdtRef<'_> {
        match self {
            ItemStructOnly::Struct(i) => ItemAdtRef::Struct(i),
        }
    }
    pub(crate) fn borrow_mut(&mut self) -> ItemAdtMutRef<'_> {
        match self {
            ItemStructOnly::Struct(i) => ItemAdtMutRef::Struct(i),
        }
    }
}

impl ItemStructOrEnum {
    pub(crate) fn borrow(&self) -> ItemAdtRef<'_> {
        match self {
            ItemStructOrEnum::Struct(i) => ItemAdtRef::Struct(i),
            ItemStructOrEnum::Enum(i) => ItemAdtRef::Enum(i),
        }
    }
    pub(crate) fn borrow_mut(&mut self) -> ItemAdtMutRef<'_> {
        match self {
            ItemStructOrEnum::Struct(i) => ItemAdtMutRef::Struct(i),
            ItemStructOrEnum::Enum(i) => ItemAdtMutRef::Enum(i),
        }
    }
}

impl ItemStructOrEnumOrUnion {
    pub(crate) fn borrow(&self) -> ItemAdtRef<'_> {
        match self {
            ItemStructOrEnumOrUnion::Struct(i) => ItemAdtRef::Struct(i),
            ItemStructOrEnumOrUnion::Enum(i) => ItemAdtRef::Enum(i),
            ItemStructOrEnumOrUnion::Union(i) => ItemAdtRef::Union(i),
        }
    }
    pub(crate) fn borrow_mut(&mut self) -> ItemAdtMutRef<'_> {
        match self {
            ItemStructOrEnumOrUnion::Struct(i) => ItemAdtMutRef::Struct(i),
            ItemStructOrEnumOrUnion::Enum(i) => ItemAdtMutRef::Enum(i),
            ItemStructOrEnumOrUnion::Union(i) => ItemAdtMutRef::Union(i),
        }
    }
}

impl ItemStructOnly {
    pub(crate) fn append_or_replace_field(&mut self, field_name: syn::Ident, field_ty: syn::Path) {
        let field_ty = syn::Type::Path(syn::TypePath {
            qself: None,
            path: field_ty,
        });
        let vis_crate = syn::Visibility::Restricted(syn::VisRestricted {
            pub_token: Default::default(),
            paren_token: Default::default(),
            in_token: None,
            path: Box::new(syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter(vec![syn::PathSegment::from(
                    <syn::Token![crate]>::default(),
                )]),
            }),
        });
        let new_field = syn::Field {
            attrs: Default::default(),
            vis: vis_crate,
            ident: Some(field_name),
            colon_token: Default::default(),
            ty: field_ty,
        };
        match self {
            ItemStructOnly::Struct(syn::ItemStruct { fields, .. }) => match fields {
                syn::Fields::Unit => {
                    *fields = syn::Fields::Named(syn::FieldsNamed {
                        brace_token: Default::default(),
                        named: syn::punctuated::Punctuated::from_iter(Some(new_field)),
                    });
                }
                syn::Fields::Named(fields) => {
                    fields.named.push(new_field);
                }
                syn::Fields::Unnamed(_) => unimplemented!(),
            },
        }
    }
}

impl ItemStructOrEnumOrUnion {
    pub(crate) fn clone_with_name(&self, new_name: syn::Ident) -> Self {
        let mut result = self.clone();
        match &mut result {
            ItemStructOrEnumOrUnion::Struct(syn::ItemStruct { ident, .. })
            | ItemStructOrEnumOrUnion::Enum(syn::ItemEnum { ident, .. })
            | ItemStructOrEnumOrUnion::Union(syn::ItemUnion { ident, .. }) => *ident = new_name,
        }
        result
    }
}
