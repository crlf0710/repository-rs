use crate::utils;
use crate::utils::IdentPair;
use crate::utils::RetainOrTake;
use crate::utils::StreamingIterator;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

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

mod item_adt;

pub(crate) use item_adt::{ItemStructOnly, ItemStructOrEnum, ItemStructOrEnumOrUnion};

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

impl<'a> ItemAdtRef<'a> {
    pub(crate) fn name(self) -> syn::Ident {
        match self {
            ItemAdtRef::Struct(syn::ItemStruct { ident, .. })
            | ItemAdtRef::Enum(syn::ItemEnum { ident, .. })
            | ItemAdtRef::Union(syn::ItemUnion { ident, .. }) => ident.clone(),
        }
    }

    pub(crate) fn vis(self) -> syn::Visibility {
        match self {
            ItemAdtRef::Struct(syn::ItemStruct { vis, .. })
            | ItemAdtRef::Enum(syn::ItemEnum { vis, .. })
            | ItemAdtRef::Union(syn::ItemUnion { vis, .. }) => vis.clone(),
        }
    }

    pub(crate) fn attrs(self) -> &'a Vec<syn::Attribute> {
        match self {
            ItemAdtRef::Struct(syn::ItemStruct { attrs, .. })
            | ItemAdtRef::Enum(syn::ItemEnum { attrs, .. })
            | ItemAdtRef::Union(syn::ItemUnion { attrs, .. }) => attrs,
        }
    }
}

impl<'a> ItemAdtMutRef<'a> {
    pub(crate) fn attrs_mut(self) -> &'a mut Vec<syn::Attribute> {
        match self {
            ItemAdtMutRef::Struct(syn::ItemStruct { attrs, .. })
            | ItemAdtMutRef::Enum(syn::ItemEnum { attrs, .. })
            | ItemAdtMutRef::Union(syn::ItemUnion { attrs, .. }) => attrs,
        }
    }

    pub(crate) fn generalized_variants_iter_mut(self) -> GeneralizedVariantIterMut<'a> {
        match self {
            ItemAdtMutRef::Struct(syn::ItemStruct {
                attrs,
                vis,
                ident,
                fields,
                ..
            }) => GeneralizedVariantIterMut {
                end_of_iterator: false,
                idx: 0,
                outer_attributes: attrs,
                outer_ident: ident,
                outer_vis: vis,
                fields: Some(fields),
                iter_mut: None,
            },
            ItemAdtMutRef::Enum(syn::ItemEnum {
                attrs,
                vis,
                ident,
                variants,
                ..
            }) => GeneralizedVariantIterMut {
                end_of_iterator: false,
                idx: 0,
                outer_attributes: attrs,
                outer_ident: ident,
                outer_vis: vis,
                fields: None,
                iter_mut: Some(utils::Either::Left(variants.iter_mut())),
            },
            ItemAdtMutRef::Union(syn::ItemUnion {
                attrs,
                vis,
                ident,
                fields,
                ..
            }) => GeneralizedVariantIterMut {
                end_of_iterator: false,
                idx: 0,
                outer_attributes: attrs,
                outer_ident: ident,
                outer_vis: vis,
                fields: None,
                iter_mut: Some(utils::Either::Right(fields.named.iter_mut())),
            },
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
    pub(crate) fn attrs(&self) -> &Vec<syn::Attribute>;
    ItemAdtMutRef<'_> => self.borrow().attrs(),
    ItemStructOnly => self.borrow().attrs(),
    ItemStructOrEnum => self.borrow().attrs(),
    ItemStructOrEnumOrUnion => self.borrow().attrs(),
}

delegate_single_fn_impl! {
    inherent;
    pub(crate) fn attrs_mut(&mut self) -> &mut Vec<syn::Attribute>;
    ItemStructOnly => self.borrow_mut().attrs_mut(),
    ItemStructOrEnum => self.borrow_mut().attrs_mut(),
    ItemStructOrEnumOrUnion => self.borrow_mut ().attrs_mut(),
}

delegate_single_fn_impl! {
    inherent;
    pub(crate) fn vis(&self) -> syn::Visibility;
    ItemAdtMutRef<'_> => self.borrow().vis(),
    ItemStructOnly => self.borrow().vis(),
    ItemStructOrEnum => self.borrow().vis(),
    ItemStructOrEnumOrUnion => self.borrow().vis(),
}

pub(crate) fn attrs_take_with_ident_name<'a>(
    attrs: &'a mut Vec<syn::Attribute>,
    ident_name: &str,
) -> impl Iterator<Item = syn::Attribute> + 'a {
    attrs.retain_or_take(|attr| !attr.path.is_ident(ident_name))
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum GeneralizedVariantKind {
    StructWhole,
    EnumVariant,
    UnionField,
}

#[derive(Debug)]
pub(crate) struct GeneralizedVariantMut<'a> {
    kind: GeneralizedVariantKind,
    index: usize,
    outer_attributes: &'a mut Vec<syn::Attribute>,
    outer_ident: &'a mut syn::Ident,
    outer_vis: &'a mut syn::Visibility,
    inner_attributes: Option<&'a mut Vec<syn::Attribute>>,
    inner_ident: Option<&'a mut syn::Ident>,
    inner_vis: Option<&'a mut syn::Visibility>,
    struct_or_enumvariant_fields: Option<&'a mut syn::Fields>,
    enumvariant_discriminant: Option<&'a mut (syn::Token![=], syn::Expr)>,
    union_single_ty: Option<&'a mut syn::Type>,
}

impl GeneralizedVariantMut<'_> {
    pub(crate) fn variant_name(&self) -> Option<IdentPair> {
        let ident = self.inner_ident.as_ref().map(|x| (**x).clone())?;
        let ident = match self.kind {
            GeneralizedVariantKind::StructWhole => unreachable!(),
            GeneralizedVariantKind::EnumVariant => IdentPair::from_camel_case_ident(ident),
            GeneralizedVariantKind::UnionField => IdentPair::from_snake_case_ident(ident),
        };
        Some(ident)
    }

    pub(crate) fn variant_ctor_path(&self) -> syn::Path {
        match self.kind {
            GeneralizedVariantKind::StructWhole => self.outer_ident.clone().into(),
            GeneralizedVariantKind::UnionField => self.outer_ident.clone().into(),
            GeneralizedVariantKind::EnumVariant => syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter(vec![
                    syn::PathSegment {
                        ident: self.outer_ident.clone(),
                        arguments: syn::PathArguments::None,
                    },
                    syn::PathSegment {
                        ident: self.inner_ident.as_ref().map(|x| (**x).clone()).unwrap(),
                        arguments: syn::PathArguments::None,
                    },
                ]),
            },
        }
    }

    pub(crate) fn variant_ctor_style(&self) -> AdtVariantCtorStyle {
        if let Some(fields) = &self.struct_or_enumvariant_fields {
            match fields {
                syn::Fields::Named(_) => AdtVariantCtorStyle::BraceNamed,
                syn::Fields::Unnamed(_) => AdtVariantCtorStyle::ParenUnamed,
                syn::Fields::Unit => AdtVariantCtorStyle::Unit,
            }
        } else {
            AdtVariantCtorStyle::BraceNamed
        }
    }
}

pub(crate) struct GeneralizedVariantIterMut<'a> {
    end_of_iterator: bool,
    idx: usize,
    outer_attributes: &'a mut Vec<syn::Attribute>,
    outer_ident: &'a mut syn::Ident,
    outer_vis: &'a mut syn::Visibility,
    fields: Option<&'a mut syn::Fields>,
    iter_mut: Option<
        utils::Either<
            syn::punctuated::IterMut<'a, syn::Variant>,
            syn::punctuated::IterMut<'a, syn::Field>,
        >,
    >,
}

impl utils::StreamingIterator for GeneralizedVariantIterMut<'_> {
    type Item<'a> = GeneralizedVariantMut<'a> where Self: 'a;
    fn next(&mut self) -> Option<Self::Item<'_>> {
        if self.end_of_iterator {
            return None;
        }
        match &mut self.iter_mut {
            None => {
                let v = GeneralizedVariantMut {
                    kind: GeneralizedVariantKind::StructWhole,
                    index: self.idx,
                    outer_attributes: self.outer_attributes,
                    outer_ident: self.outer_ident,
                    outer_vis: self.outer_vis,
                    inner_attributes: None,
                    inner_ident: None,
                    inner_vis: None,
                    struct_or_enumvariant_fields: self.fields.as_mut().map(|x| &mut **x),
                    enumvariant_discriminant: None,
                    union_single_ty: None,
                };
                self.idx += 1;
                self.end_of_iterator = true;
                Some(v)
            }
            Some(utils::Either::Left(variant_iter_mut)) => {
                let Some(variant) = variant_iter_mut.next() else {
                    self.end_of_iterator = true;
                    return None;
                };
                let v = GeneralizedVariantMut {
                    kind: GeneralizedVariantKind::EnumVariant,
                    index: self.idx,
                    outer_attributes: self.outer_attributes,
                    outer_ident: self.outer_ident,
                    outer_vis: self.outer_vis,
                    inner_attributes: Some(&mut variant.attrs),
                    inner_ident: Some(&mut variant.ident),
                    inner_vis: None,
                    struct_or_enumvariant_fields: Some(&mut variant.fields),
                    enumvariant_discriminant: variant.discriminant.as_mut(),
                    union_single_ty: None,
                };
                self.idx += 1;
                Some(v)
            }
            Some(utils::Either::Right(field_iter_mut)) => {
                let Some(field) = field_iter_mut.next() else {
                    self.end_of_iterator = true;
                    return None;
                };
                let v = GeneralizedVariantMut {
                    kind: GeneralizedVariantKind::UnionField,
                    index: self.idx,
                    outer_attributes: self.outer_attributes,
                    outer_ident: self.outer_ident,
                    outer_vis: self.outer_vis,
                    inner_attributes: Some(&mut field.attrs),
                    inner_ident: field.ident.as_mut(),
                    inner_vis: Some(&mut field.vis),
                    struct_or_enumvariant_fields: None,
                    enumvariant_discriminant: None,
                    union_single_ty: Some(&mut field.ty),
                };
                self.idx += 1;
                Some(v)
            }
        }
    }
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
    RefGetter,
    Setter,
    GetterAndSetter,
    RefGetterAndSetter,
}

impl GeneralizedField {
    pub(crate) fn new_from_syn_field(
        field: &mut syn::Field,
        index_in_first_applicable_variant: usize,
        flags: GeneralizedFieldFlags,
    ) -> syn::Result<Self> {
        let accessor = field
            .attrs
            .retain_or_take(|attr| !attr.path.is_ident("accessor"))
            .map(|x| utils::GeneralizedMeta::parse_attribute(&x).and_then(|x| x.get_ident_value()))
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
            flags,
        };
        Ok(field)
    }

    pub(crate) fn vis(&self) -> syn::Visibility {
        self.field.vis.clone()
    }

    pub(crate) fn has_getter(&self) -> bool {
        match self.flags {
            GeneralizedFieldFlags::Getter | GeneralizedFieldFlags::GetterAndSetter => true,
            GeneralizedFieldFlags::None
            | GeneralizedFieldFlags::RefGetter
            | GeneralizedFieldFlags::Setter
            | GeneralizedFieldFlags::RefGetterAndSetter => false,
        }
    }

    pub(crate) fn has_ref_getter(&self) -> bool {
        match self.flags {
            GeneralizedFieldFlags::RefGetter | GeneralizedFieldFlags::RefGetterAndSetter => true,
            GeneralizedFieldFlags::None
            | GeneralizedFieldFlags::Getter
            | GeneralizedFieldFlags::Setter
            | GeneralizedFieldFlags::GetterAndSetter => false,
        }
    }

    pub(crate) fn has_setter(&self) -> bool {
        match self.flags {
            GeneralizedFieldFlags::Setter
            | GeneralizedFieldFlags::GetterAndSetter
            | GeneralizedFieldFlags::RefGetterAndSetter => true,
            GeneralizedFieldFlags::None
            | GeneralizedFieldFlags::RefGetter
            | GeneralizedFieldFlags::Getter => false,
        }
    }
}

#[derive(Debug)]
pub(crate) struct AdtVariantDefinition {
    pub(crate) variant_name: Option<utils::IdentPair>,
    pub(crate) ctor_path: syn::Path,
    pub(crate) ctor_style: AdtVariantCtorStyle,
    pub(crate) mandatory_components: Vec<usize>,
    pub(crate) optional_components: Vec<usize>,
}

impl AdtVariantDefinition {
    pub(crate) fn default_ctor_fn_name(&self, default_span: proc_macro2::Span) -> syn::Ident {
        if let Some(ident_pair) = &self.variant_name {
            ident_from_combining!(ident_pair.span() => "new", ident_pair.snake_case)
        } else {
            syn::Ident::new_raw("new", default_span)
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum AdtVariantCtorStyle {
    BraceNamed,
    ParenUnamed,
    Unit,
}

#[derive(Debug)]
pub(crate) struct AdtComponentDefinition {
    pub(crate) component_name: utils::IdentPair,
    pub(crate) is_inherent: bool,
    pub(crate) is_single_field_group: bool,
    pub(crate) always_mandatory: bool,
    pub(crate) applicable_variants: Vec<usize>,
    pub(crate) fields_named: bool,
    pub(crate) fields: Vec<GeneralizedField>,
    pub(crate) field_indexes_in_applicable_variants: Vec<Vec<usize>>,
}

#[derive(Clone, Copy)]
pub(crate) enum AdtKind {
    Interned,
    Entity,
}

pub(crate) fn build_fields_from_field_iter(
    ctor_style: AdtVariantCtorStyle,
    fields: impl Iterator<Item = syn::Field>,
) -> syn::Fields {
    match ctor_style {
        AdtVariantCtorStyle::BraceNamed => syn::Fields::Named(syn::FieldsNamed {
            brace_token: Default::default(),
            named: syn::punctuated::Punctuated::from_iter(fields),
        }),
        AdtVariantCtorStyle::ParenUnamed => syn::Fields::Unnamed(syn::FieldsUnnamed {
            paren_token: Default::default(),
            unnamed: syn::punctuated::Punctuated::from_iter(fields),
        }),
        AdtVariantCtorStyle::Unit => {
            assert!(fields.count() == 0);
            syn::Fields::Unit
        }
    }
}

pub(crate) fn component_storage_ty_and_build_expr(
    adt_kind: AdtKind,
    repo_crate_name: &syn::Ident,
    component_ident: &syn::Ident,
    kiosk_entry: &proc_macro2::TokenStream,
    is_inherent: bool,
    is_mandatory_in_all_variants: bool,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    match adt_kind {
        AdtKind::Interned => {
            assert!(is_inherent && is_mandatory_in_all_variants);
            (
                quote!(#repo_crate_name ::component_storage::DenseUniqStorageWithId<#component_ident>),
                quote!(#repo_crate_name ::component_storage::DenseUniqStorageWithId::new(#kiosk_entry)),
            )
        }
        AdtKind::Entity => {
            if is_inherent {
                assert!(is_mandatory_in_all_variants);
                (
                    quote!(#repo_crate_name ::component_storage::DenseStorageWithId<#component_ident>),
                    quote!(#repo_crate_name ::component_storage::DenseStorageWithId::new(#kiosk_entry)),
                )
            } else {
                if is_mandatory_in_all_variants {
                    (
                        quote!(#repo_crate_name ::component_storage::DenseStorage<#component_ident>),
                        quote!(#repo_crate_name ::component_storage::DenseStorage::new()),
                    )
                } else {
                    (
                        quote!(#repo_crate_name ::component_storage::SparseStorage<#component_ident>),
                        quote!(#repo_crate_name ::component_storage::SparseStorage::new()),
                    )
                }
            }
        }
    }
}

pub(crate) fn collect_adt_variants_and_components(
    item: ItemAdtMutRef<'_>,
    adt_kind: AdtKind,
    inherent_group_ident: &IdentPair,
) -> syn::Result<(Vec<AdtVariantDefinition>, Vec<AdtComponentDefinition>)> {
    use syn::spanned::Spanned;
    use utils::PushAndGetWithIndex;
    let item_name = item.name();
    let mut variant_defs = vec![];
    let mut comp_defs = vec![];
    const ENTITY_INHERENT_COMP_IDX: usize = 0;
    if matches!(adt_kind, AdtKind::Entity) {
        let (comp_def_idx, comp_def) = comp_defs.push_and_get_with_index(AdtComponentDefinition {
            component_name: inherent_group_ident.clone(),
            is_inherent: true,
            is_single_field_group: false,
            always_mandatory: true,
            applicable_variants: Vec::new(),
            fields_named: true,
            fields: Vec::new(),
            field_indexes_in_applicable_variants: Vec::new(),
        });
        assert_eq!(comp_def_idx, ENTITY_INHERENT_COMP_IDX);
    };
    let mut variant_iter_mut = item.generalized_variants_iter_mut();
    while let Some(mut variant) = variant_iter_mut.next() {
        if matches!(variant.kind, GeneralizedVariantKind::EnumVariant)
            && matches!(adt_kind, AdtKind::Entity)
            && variant.enumvariant_discriminant.is_some()
        {
            return Err(syn::Error::new(
                variant.enumvariant_discriminant.as_ref().unwrap().1.span(),
                "enumerate entity cannot specify discriminant",
            ));
        }
        let variant_name = variant.variant_name();
        let variant_ctor_path = variant.variant_ctor_path();
        let variant_ctor_style = variant.variant_ctor_style();

        let (variant_def_idx, variant_def) =
            variant_defs.push_and_get_with_index(AdtVariantDefinition {
                variant_name,
                ctor_path: variant_ctor_path,
                ctor_style: variant_ctor_style,
                mandatory_components: Vec::new(),
                optional_components: Vec::new(),
            });

        // associate the newly created variant with the inherent group.
        let mut comp_list_in_variant = Vec::new();
        let variant_fields_named = !matches!(variant_ctor_style, AdtVariantCtorStyle::ParenUnamed);

        let inherent_comp_def_idx = if matches!(adt_kind, AdtKind::Entity) {
            ENTITY_INHERENT_COMP_IDX
        } else {
            let (comp_def_idx, _) = comp_defs.push_and_get_with_index(AdtComponentDefinition {
                component_name: inherent_group_ident.clone(),
                is_inherent: true,
                is_single_field_group: false,
                always_mandatory: true,
                applicable_variants: vec![],
                fields_named: variant_fields_named,
                fields: Vec::new(),
                field_indexes_in_applicable_variants: vec![],
            });
            comp_def_idx
        };
        comp_list_in_variant.push(inherent_comp_def_idx);
        comp_defs[inherent_comp_def_idx]
            .applicable_variants
            .push(variant_def_idx);
        comp_defs[inherent_comp_def_idx]
            .field_indexes_in_applicable_variants
            .push(Vec::new());
        variant_def.mandatory_components.push(inherent_comp_def_idx);

        // add fields to the group.
        if let Some(fields) = &mut variant.struct_or_enumvariant_fields {
            let fields = &mut **fields;
            for (field_idx, field) in fields.iter_mut().enumerate() {
                let field_comp_def_idx;
                if !matches!(adt_kind, AdtKind::Entity) {
                    field_comp_def_idx = inherent_comp_def_idx;
                } else {
                    let inherent_attr = attrs_take_with_ident_name(&mut field.attrs, "inherent")
                        .collect::<Vec<_>>();
                    if !inherent_attr.is_empty() {
                        if matches!(variant.kind, GeneralizedVariantKind::StructWhole) {
                            field_comp_def_idx = inherent_comp_def_idx;
                        } else {
                            return Err(syn::Error::new(
                                inherent_attr[0].span(),
                                "#[inherent] is not allowd for enums",
                            ));
                        }
                    } else {
                        let group_attr = attrs_take_with_ident_name(&mut field.attrs, "group")
                            .collect::<Vec<_>>();
                        let is_single_field_group = group_attr.is_empty();
                        let mut existing_comp_idx = None;
                        let group_comp_name;
                        if !is_single_field_group {
                            let group_attr =
                                utils::GeneralizedMeta::parse_attribute(&group_attr[0])?;
                            let group_name = utils::IdentPair::from_snake_case_ident(
                                group_attr.get_ident_value()?,
                            );
                            group_comp_name = ident_from_combining!(group_name.span() => item_name, group_name.camel_case_ident());
                            for comp_idx_in_variant in comp_list_in_variant.iter().cloned() {
                                if !comp_defs[comp_idx_in_variant].is_single_field_group
                                    && comp_defs[comp_idx_in_variant]
                                        .component_name
                                        .eq_camel_case_ident(&group_comp_name)
                                {
                                    existing_comp_idx = Some(comp_idx_in_variant);
                                }
                            }
                        } else {
                            let field_name = if let Some(ident) = field.ident.as_ref() {
                                ident.clone()
                            } else {
                                syn::Ident::new_raw(&format!("field{field_idx}"), field.span())
                            };
                            let field_name = utils::IdentPair::from_snake_case_ident(field_name);
                            group_comp_name = ident_from_combining!(field_name.span() => item_name, "Field", field_name.camel_case_ident());
                        }
                        let comp_def_idx = if let Some(existing_comp_idx) = existing_comp_idx {
                            existing_comp_idx
                        } else {
                            let (comp_def_idx, _) =
                                comp_defs.push_and_get_with_index(AdtComponentDefinition {
                                    component_name: utils::IdentPair::from_camel_case_ident(
                                        group_comp_name,
                                    ),
                                    is_inherent: false,
                                    is_single_field_group,
                                    always_mandatory: false,
                                    applicable_variants: vec![],
                                    fields_named: variant_fields_named,
                                    fields: Vec::new(),
                                    field_indexes_in_applicable_variants: vec![],
                                });

                            comp_list_in_variant.push(comp_def_idx);
                            comp_defs[comp_def_idx]
                                .applicable_variants
                                .push(variant_def_idx);
                            comp_defs[comp_def_idx]
                                .field_indexes_in_applicable_variants
                                .push(Vec::new());
                            variant_def.mandatory_components.push(comp_def_idx);
                            comp_def_idx
                        };
                        field_comp_def_idx = comp_def_idx;
                    }
                }
                let ref_attr =
                    attrs_take_with_ident_name(&mut field.attrs, "by_ref").collect::<Vec<_>>();
                let use_ref_getter = !ref_attr.is_empty();
                let use_setter = matches!(adt_kind, AdtKind::Entity)
                    && !comp_defs[field_comp_def_idx].is_inherent;
                let flags = match (use_ref_getter, use_setter) {
                    (false, false) => GeneralizedFieldFlags::Getter,
                    (false, true) => GeneralizedFieldFlags::GetterAndSetter,
                    (true, false) => GeneralizedFieldFlags::RefGetter,
                    (true, true) => GeneralizedFieldFlags::RefGetterAndSetter,
                };
                comp_defs[field_comp_def_idx]
                    .fields
                    .push(GeneralizedField::new_from_syn_field(
                        field, field_idx, flags,
                    )?);
                assert_eq!(
                    1,
                    comp_defs[field_comp_def_idx]
                        .field_indexes_in_applicable_variants
                        .len()
                );
                comp_defs[field_comp_def_idx].field_indexes_in_applicable_variants[0]
                    .push(field_idx);
            }
        } else {
            // union case.
            assert!(!matches!(adt_kind, AdtKind::Entity));
            let field_comp_def_idx = inherent_comp_def_idx;
            let field_idx = 0;
            let flags = GeneralizedFieldFlags::None;
            let mut field = syn::Field {
                attrs: Default::default(),
                vis: variant.inner_vis.as_ref().map(|x| (**x).clone()).unwrap(),
                ident: None,
                colon_token: Default::default(),
                ty: variant
                    .union_single_ty
                    .as_ref()
                    .map(|x| (**x).clone())
                    .unwrap(),
            };

            comp_defs[field_comp_def_idx]
                .fields
                .push(GeneralizedField::new_from_syn_field(
                    &mut field, field_idx, flags,
                )?);
            assert_eq!(
                1,
                comp_defs[field_comp_def_idx]
                    .field_indexes_in_applicable_variants
                    .len()
            );
            comp_defs[field_comp_def_idx].field_indexes_in_applicable_variants[0].push(field_idx);
        }
    }

    // FIXME: Merge same name groups from different variants, or error
    let all_variants_count = variant_defs.len();
    'outer: for (comp_def_idx, comp_def) in comp_defs.iter_mut().enumerate() {
        if comp_def.applicable_variants.len() < all_variants_count {
            continue;
        }
        for applicable_variant in comp_def.applicable_variants.iter().cloned() {
            if !variant_defs[applicable_variant]
                .mandatory_components
                .contains(&comp_def_idx)
            {
                continue 'outer;
            }
        }
        comp_def.always_mandatory = true;
    }
    Ok((variant_defs, comp_defs))
}
