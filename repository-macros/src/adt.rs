use crate::utils;
use crate::utils::IdentPair;
use crate::utils::IdentText;
use crate::utils::RetainOrTake;
use crate::utils::StreamingIterator;
use crate::utils::TyWrap;
use crate::utils::WithIndex;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::ops::Not;
use syn::spanned::Spanned;
use syn::ExprLit;

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
    pub(crate) hidden_accessor: Option<IdentPair>,
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
        let hidden_accessor = field
            .attrs
            .retain_or_take(|attr| !attr.path.is_ident("hidden_accessor"))
            .map(|x| utils::GeneralizedMeta::parse_attribute(&x).and_then(|x| x.get_ident_value()))
            .collect::<syn::Result<Vec<_>>>()?;
        let hidden_accessor = match &hidden_accessor[..] {
            [] => None,
            [x] if accessor.is_some() => return Err(syn::Error::new(x.span(), "hidden_accessor cannot be specified together with accessor")),
            [x] => Some(x.clone()),
            [x, y, ..] => return Err(syn::Error::new(y.span(), "multiple hidden_accessor specified")),
        };
        let field = GeneralizedField {
            field: field.clone(),
            indexes_in_applicable_variants: vec![index_in_first_applicable_variant],
            accessor: accessor.map(IdentPair::from_snake_case_ident),
            hidden_accessor: hidden_accessor.map(IdentPair::from_snake_case_ident),
            flags,
        };
        Ok(field)
    }

    pub(crate) fn vis(&self) -> syn::Visibility {
        self.field.vis.clone()
    }

    pub(crate) fn has_getter(&self, hidden: bool) -> bool {
        match self.flags {
            GeneralizedFieldFlags::Getter | GeneralizedFieldFlags::GetterAndSetter => self.hidden_accessor.is_some() == hidden,
            GeneralizedFieldFlags::None
            | GeneralizedFieldFlags::RefGetter
            | GeneralizedFieldFlags::Setter
            | GeneralizedFieldFlags::RefGetterAndSetter => false,
        }
    }

    pub(crate) fn has_ref_getter(&self, hidden: bool) -> bool {
        match self.flags {
            GeneralizedFieldFlags::RefGetter | GeneralizedFieldFlags::RefGetterAndSetter => self.hidden_accessor.is_some() == hidden,
            GeneralizedFieldFlags::None
            | GeneralizedFieldFlags::Getter
            | GeneralizedFieldFlags::Setter
            | GeneralizedFieldFlags::GetterAndSetter => false,
        }
    }

    pub(crate) fn has_setter(&self, hidden: bool) -> bool {
        match self.flags {
            GeneralizedFieldFlags::Setter
            | GeneralizedFieldFlags::GetterAndSetter
            | GeneralizedFieldFlags::RefGetterAndSetter => self.hidden_accessor.is_some() == hidden,
            GeneralizedFieldFlags::None
            | GeneralizedFieldFlags::RefGetter
            | GeneralizedFieldFlags::Getter => false,
        }
    }

    pub(crate) fn accessor_ident(&self) -> Option<syn::Ident> {
        if let Some(ident_pair) = &self.accessor {
            Some(ident_pair.snake_case_ident())
        } else if let Some(ident) = &self.field.ident {
            Some(ident.clone())
        } else {
            None
        }
    }

    pub(crate) fn hidden_accessor_ident(&self) -> Option<syn::Ident> {
        if let Some(ident_pair) = &self.hidden_accessor {
            Some(ident_pair.snake_case_ident())
        } else {
            self.accessor_ident()
        }
    }

    pub(crate) fn accessor_or_generative_naming_ident(
        &self,
        field_idx_in_variant: usize,
    ) -> syn::Ident {
        if let Some(ident) = self.accessor_ident() {
            ident
        } else {
            syn::Ident::new_raw(&format!("field_{field_idx_in_variant}"), self.field.span())
        }
    }

    pub(crate) fn key_ident(&self) -> Option<syn::Ident> {
        if let Some(ident_pair) = &self.accessor {
            Some(ident_pair.snake_case_ident())
        } else if let Some(ident) = &self.field.ident {
            Some(ident.clone())
        } else {
            None
        }
    }

    pub(crate) fn ctor_param_ty(
        &self,
        is_mandatory: bool,
        use_keyed: bool,
        repo_crate_ident: &syn::Ident,
    ) -> syn::Type {
        let inner;
        let key = if use_keyed { self.key_ident() } else { None };
        let keyed_inner;
        if let Some(key) = key {
            let default_span = key.span();
            keyed_inner = syn::Type::ImplTrait(syn::TypeImplTrait {
                impl_token: Default::default(),
                bounds: syn::punctuated::Punctuated::from_iter(vec![syn::TypeParamBound::Trait(
                    syn::TraitBound {
                        path: syn::Path {
                            leading_colon: None,
                            segments: syn::punctuated::Punctuated::from_iter(vec![
                                syn::PathSegment {
                                    ident: syn::Ident::new("Into", default_span),
                                    arguments: syn::PathArguments::AngleBracketed(
                                        syn::AngleBracketedGenericArguments {
                                            args: syn::punctuated::Punctuated::from_iter(
                                                vec![syn::GenericArgument::Type(syn::Type::Path(
                                                    syn::TypePath {
                                                        qself: None,
                                                        path: syn::Path {
                                                            leading_colon: None,
                                                            segments: syn::punctuated::Punctuated::from_iter(
                                                                vec![
                                                                    syn::PathSegment { ident: repo_crate_ident.clone(), arguments: Default::default() },
                                                                    syn::PathSegment {  ident: syn::Ident::new("keyed_value", default_span), arguments: Default::default()},
                                                                    syn::PathSegment {  ident: syn::Ident::new("KeyedValue", default_span), arguments:
                                                                     syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { colon2_token: Default::default(), lt_token: Default::default(),
                                                                        args: syn::punctuated::Punctuated::from_iter(vec![
                                                                            syn::GenericArgument::Const(syn::Expr::Lit(
                                                                                ExprLit {
                                                                                    attrs: Default::default(),
                                                                                    lit: syn::Lit::Str(syn::LitStr::new(&key.ident_text(), default_span))
                                                                                }
                                                                            )),
                                                                            syn::GenericArgument::Type(self.field.ty.clone())
                                                                        ]), gt_token: Default::default() })
                                                                    },
                                                                ]
                                                            ),
                                                        },
                                                    },
                                                )),]
                                            ),
                                            colon2_token: Default::default(),
                                            lt_token: Default::default(),
                                            gt_token: Default::default(),
                                        },
                                    ),
                                },
                            ]),
                        },
                        modifier: syn::TraitBoundModifier::None,
                        paren_token: Default::default(),
                        lifetimes: Default::default(),
                    },
                )]),
            });
            inner = &keyed_inner;
        } else {
            inner = &self.field.ty;
        }
        utils::type_opt_if_not_mandatory(is_mandatory, &self.field.ty)
    }

    pub(crate) fn build_entity_accessor(
        this: WithIndex<&Self>,
        repo_crate_ident: &syn::Ident,
        default_span: proc_macro2::Span,
        repo_ty: &syn::Path,
        handle_ident: &syn::Ident,
        comp_ty: &syn::Path,
        is_mandatory: bool,
    ) -> (TokenStream2, TokenStream2) {
        let Some(accessor_ident) = this.accessor_ident() else {
            return (TokenStream2::new(), TokenStream2::new());
        };
        let hidden_accessor_ident = this.hidden_accessor_ident().unwrap_or_else(|| accessor_ident.clone());
        let accessor_vis = this.vis();
        let error_ty_component_not_present = syn::Type::Path(syn::TypePath {
            qself: None,
            path: crate::path_repo_error_component_not_present(
                this.field.ty.span(),
                repo_crate_ident.clone(),
            ),
        });
        #[derive(Clone, Copy)]
        enum SubAccessor {
            Getter,
            HiddenGetter,
            RefGetter,
            HiddenRefGetter,
            RefMutGetter,
            HiddenRefMutGetter,
            Setter,
            HiddenSetter,
        }
        let extra_input_ident = syn::Ident::new("__value", accessor_ident.span());
        let repo_input_ident = syn::Ident::new("__repo", accessor_ident.span());
        let entity_input_ident = syn::Ident::new("__entity", accessor_ident.span());

        let mut result_inherent = TokenStream2::new();
        let mut result_nonassoc = TokenStream2::new();

        let field_is_named = this.field.ident.is_some();

        let getter_tokens = if field_is_named {
            let field_name = this.field.ident.as_ref().unwrap();
            quote!(let #comp_ty{#field_name: value, ..} = __comp)
        } else {
            let mut pattern = TokenStream2::new();
            for _ in 0..this.index() {
                pattern.extend(quote!(_,));
            }
            pattern.extend(quote!(value, ..));
            quote!(let #comp_ty(#pattern) = __comp)
        };
        let setter_token = if field_is_named {
            let field_name = this.field.ident.as_ref().unwrap();
            quote!(#field_name)
        } else {
            let idx = syn::Index::from(this.index());
            quote!(#idx)
        };

        for subaccessor in [
            SubAccessor::Getter,
            SubAccessor::HiddenGetter,
            SubAccessor::RefGetter,
            SubAccessor::HiddenRefGetter,
            SubAccessor::RefMutGetter,
            SubAccessor::HiddenRefMutGetter,
            SubAccessor::Setter,
            SubAccessor::HiddenSetter,
        ] {
            let subaccessor_exist = match subaccessor {
                SubAccessor::Getter => this.has_getter(false),
                SubAccessor::HiddenGetter => this.has_getter(true),
                SubAccessor::RefGetter => this.has_ref_getter(false),
                SubAccessor::HiddenRefGetter => this.has_ref_getter(true),
                SubAccessor::RefMutGetter => this.has_ref_getter(false),
                SubAccessor::HiddenRefMutGetter => this.has_ref_getter(true),
                SubAccessor::Setter => this.has_setter(false),
                SubAccessor::HiddenSetter => this.has_setter(true),
            };
            if !subaccessor_exist {
                continue;
            }
            let subaccessor_ident = match subaccessor {
                SubAccessor::Getter => accessor_ident.clone(),
                SubAccessor::HiddenGetter => hidden_accessor_ident.clone(),
                SubAccessor::RefGetter => accessor_ident.clone(),
                SubAccessor::HiddenRefGetter => hidden_accessor_ident.clone(),
                SubAccessor::RefMutGetter => {
                    ident_from_combining!(accessor_ident.span() => accessor_ident, "mut")
                }
                SubAccessor::HiddenRefMutGetter => {
                    ident_from_combining!(hidden_accessor_ident.span() => hidden_accessor_ident, "mut")
                }
                SubAccessor::Setter => {
                    ident_from_combining!(accessor_ident.span() => "set", accessor_ident)
                }
                SubAccessor::HiddenSetter => {
                    ident_from_combining!(hidden_accessor_ident.span() => "set", hidden_accessor_ident)
                }
            };

            let subaccessor_input = match subaccessor {
                SubAccessor::Getter | SubAccessor::RefGetter | SubAccessor::RefMutGetter |
                SubAccessor::HiddenGetter | SubAccessor::HiddenRefGetter | SubAccessor::HiddenRefMutGetter => None,
                SubAccessor::Setter | SubAccessor::HiddenSetter => Some(this.field.ty.clone()),
            };

            let subaccessor_extra_input = match &subaccessor_input {
                Some(ty) => quote!(#extra_input_ident : #ty,),
                _ => quote!(),
            };

            let subaccessor_repo_input = match subaccessor {
                SubAccessor::Getter | SubAccessor::RefGetter | SubAccessor::HiddenGetter | SubAccessor::HiddenRefGetter => {
                    quote!(#repo_input_ident: &#repo_ty)
                }
                SubAccessor::RefMutGetter | SubAccessor::Setter | SubAccessor::HiddenRefMutGetter | SubAccessor::HiddenSetter => {
                    quote!(#repo_input_ident: &mut #repo_ty)
                }
            };

            let subaccessor_output_ty = match subaccessor {
                SubAccessor::Getter | SubAccessor::HiddenGetter => this.field.ty.clone(),
                SubAccessor::RefGetter | SubAccessor::HiddenRefGetter => this.field.ty.clone().wrap_ty_with_shared_ref(),
                SubAccessor::RefMutGetter | SubAccessor::HiddenRefMutGetter => this.field.ty.clone().wrap_ty_with_unique_ref(),
                SubAccessor::Setter | SubAccessor::HiddenSetter => crate::type_unit(default_span),
            };

            let subaccessor_result_ty = if is_mandatory {
                subaccessor_output_ty.clone()
            } else {
                subaccessor_output_ty
                    .clone()
                    .wrap_ty_with_result(error_ty_component_not_present.clone())
            };

            let prepare_comp_stmt = match (subaccessor, is_mandatory) {
                (SubAccessor::Getter | SubAccessor::RefGetter | SubAccessor::HiddenGetter | SubAccessor::HiddenRefGetter, true) => {
                    quote! {
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage(#repo_input_ident);
                        let __comp = __comp_storage.get(#entity_input_ident.entity_id);
                    }
                }
                (SubAccessor::Getter | SubAccessor::RefGetter | SubAccessor::HiddenGetter | SubAccessor::HiddenRefGetter, false) => {
                    quote! {
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage(#repo_input_ident);
                        let __comp = __comp_storage.get(#entity_input_ident.entity_id).ok_or(#error_ty_component_not_present)?;
                    }
                }
                (SubAccessor::RefMutGetter | SubAccessor::Setter | SubAccessor::HiddenRefMutGetter | SubAccessor::HiddenSetter, true) => {
                    quote! {
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage_mut(#repo_input_ident);
                        let __comp = __comp_storage.get_mut(#entity_input_ident.entity_id);
                    }
                }
                (SubAccessor::RefMutGetter | SubAccessor::Setter | SubAccessor::HiddenRefMutGetter | SubAccessor::HiddenSetter, false) => {
                    quote! {
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage_mut(#repo_input_ident);
                        let __comp = __comp_storage.get_mut(#entity_input_ident.entity_id).ok_or(#error_ty_component_not_present)?;
                    }
                }
            };
            let subaccessor_body_stmt = {
                match subaccessor {
                    SubAccessor::Getter | SubAccessor::HiddenGetter => {
                        quote! {
                            let __value = if #getter_tokens {
                                <#subaccessor_output_ty as Clone>::clone(value)
                            } else {
                                unreachable!()
                            };
                        }
                    }
                    SubAccessor::RefGetter | SubAccessor::RefMutGetter |  SubAccessor::HiddenRefGetter | SubAccessor::HiddenRefMutGetter => {
                        quote! {
                            let __value = if #getter_tokens {
                                value
                            } else {
                                unreachable!()
                            };
                        }
                    }
                    SubAccessor::Setter | SubAccessor::HiddenSetter => {
                        quote! {
                            __comp.#setter_token = #extra_input_ident;
                            let __value = ();
                        }
                    }
                }
            };

            let ret_expr = if is_mandatory {
                quote! {
                    __value
                }
            } else {
                quote! {
                    Ok(__value)
                }
            };

            match subaccessor {
                SubAccessor::Getter |
                SubAccessor::RefGetter  |
                SubAccessor::RefMutGetter |
                SubAccessor::Setter => {
                    let subaccesor_impl = quote!(
                        #accessor_vis fn #subaccessor_ident(self, #subaccessor_extra_input #subaccessor_repo_input) -> #subaccessor_result_ty {
                            let #entity_input_ident = self;
                            #prepare_comp_stmt
                            #subaccessor_body_stmt
                            #ret_expr
                        }
                    );
                    result_inherent.extend(subaccesor_impl);
                },
                SubAccessor::HiddenGetter |
                SubAccessor::HiddenRefGetter |
                SubAccessor::HiddenRefMutGetter |
                SubAccessor::HiddenSetter => {
                    let subaccesor_impl = quote!(
                        fn #subaccessor_ident(#entity_input_ident: #handle_ident, #subaccessor_extra_input #subaccessor_repo_input) -> #subaccessor_result_ty {
                            #prepare_comp_stmt
                            #subaccessor_body_stmt
                            #ret_expr
                        }
                    );
                    result_nonassoc.extend(subaccesor_impl);
                },
            }
            
        }
        (result_inherent, result_nonassoc)
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
    pub(crate) fn default_ctor_fn_name(
        &self,
        ctor_prefix: &'static str,
        default_span: proc_macro2::Span,
    ) -> syn::Ident {
        if let Some(ident_pair) = &self.variant_name {
            ident_from_combining!(ident_pair.span() => ctor_prefix, ident_pair.snake_case)
        } else {
            syn::Ident::new_raw(ctor_prefix, default_span)
        }
    }

    pub(crate) fn default_ttor_fn_name(
        &self,
        ttor_prefix: &'static str,
        ttor_verb1: Option<&'static str>,
        ttor_verb2: Option<&'static str>,
        src: &Self,
        default_span: proc_macro2::Span,
    ) -> syn::Ident {
        let dest = self.variant_name.as_ref().unwrap();
        let src = src.variant_name.as_ref().unwrap();
        match (ttor_verb1, ttor_verb2) {
            (None, None) => {
                ident_from_combining!(dest.span() => ttor_prefix, dest.snake_case, src.snake_case)
            }
            (None, Some(v2)) => {
                ident_from_combining!(dest.span() => ttor_prefix, dest.snake_case, v2, src.snake_case)
            }
            (Some(v1), None) => {
                ident_from_combining!(dest.span() => ttor_prefix, v1, dest.snake_case, src.snake_case)
            }
            (Some(v1), Some(v2)) => {
                ident_from_combining!(dest.span() => ttor_prefix, v1, dest.snake_case, v2, src.snake_case)
            }
        }
    }

    pub(crate) fn calc_transition_appending_mandatory_components(
        &self,
        existing: Option<&Self>,
    ) -> impl Iterator<Item = usize> + Clone + '_ {
        let mut existing_components = Vec::new();
        if let Some(existing) = existing {
            existing_components.extend(existing.mandatory_components.iter().copied());
            existing_components.extend(existing.optional_components.iter().copied());
        }
        self.mandatory_components
            .iter()
            .copied()
            .filter(move |component_idx| existing_components.contains(component_idx).not())
    }

    pub(crate) fn calc_transition_appending_optional_components(
        &self,
        existing: Option<&Self>,
    ) -> impl Iterator<Item = usize> + Clone + '_ {
        let mut existing_components = Vec::new();
        if let Some(existing) = existing {
            existing_components.extend(existing.mandatory_components.iter().copied());
            existing_components.extend(existing.optional_components.iter().copied());
        }
        self.optional_components
            .iter()
            .copied()
            .filter(move |component_idx| existing_components.contains(component_idx).not())
    }

    pub(crate) fn calc_transition_strengthen_from_optional_to_mandatory_components(
        &self,
        existing: Option<&Self>,
    ) -> impl Iterator<Item = usize> + Clone + '_ {
        let mut existing_components = Vec::new();
        if let Some(existing) = existing {
            existing_components.extend(existing.optional_components.iter().copied());
        }
        self.mandatory_components
            .iter()
            .copied()
            .filter(move |component_idx| existing_components.contains(component_idx))
    }

    #[allow(dead_code)]
    pub(crate) fn calc_transition_weaken_from_mandatory_to_optional_components(
        &self,
        existing: Option<&Self>,
    ) -> impl Iterator<Item = usize> + Clone + '_ {
        let mut existing_components = Vec::new();
        if let Some(existing) = existing {
            existing_components.extend(existing.mandatory_components.iter().copied());
        }
        self.optional_components
            .iter()
            .copied()
            .filter(move |component_idx| existing_components.contains(component_idx))
    }

    pub(crate) fn calc_transition_removal_optional_components(
        &self,
        existing: Option<&Self>,
    ) -> impl Iterator<Item = usize> + Clone + '_ {
        let mut new_components = Vec::new();
        new_components.extend(self.mandatory_components.iter().copied());
        new_components.extend(self.optional_components.iter().copied());

        let mut existing_components = Vec::new();
        if let Some(existing) = existing {
            existing_components.extend(existing.optional_components.iter().copied());
        }
        existing_components
            .into_iter()
            .filter(move |component_idx| new_components.contains(component_idx).not())
    }

    pub(crate) fn calc_transition_removal_mandatory_components(
        &self,
        existing: Option<&Self>,
    ) -> impl Iterator<Item = usize> + Clone + '_ {
        let mut new_components = Vec::new();
        new_components.extend(self.mandatory_components.iter().copied());
        new_components.extend(self.optional_components.iter().copied());

        let mut existing_components = Vec::new();
        if let Some(existing) = existing {
            existing_components.extend(existing.mandatory_components.iter().copied());
        }
        existing_components
            .into_iter()
            .filter(move |component_idx| new_components.contains(component_idx).not())
    }
}

impl AdtVariantDefinition {
    fn iter_owned_fields_with_index_in_components<'a>(
        this: WithIndex<&Self>,
        comp_def: &'a AdtComponentDefinition,
    ) -> impl Iterator<Item = (usize, &'a GeneralizedField)> + 'a {
        let var_def_idx = this.index();
        let applicable_variants_pos = comp_def
            .applicable_variants
            .iter()
            .position(|variant_idx_in_list| *variant_idx_in_list == var_def_idx)
            .unwrap();

        comp_def.fields.iter().map(move |field_def| {
            let field_idx_in_variant =
                field_def.indexes_in_applicable_variants[applicable_variants_pos];
            (field_idx_in_variant, field_def)
        })
    }

    fn prepare_adt_initializer_param_list(
        this: WithIndex<&Self>,
        comp_def_idx_and_is_mandatory_list: impl Iterator<Item = (usize, bool)>,
        comp_defs: &Vec<AdtComponentDefinition>,
        use_keyed: bool,
        repo_crate_ident: &syn::Ident,
    ) -> Vec<Option<(syn::Ident, syn::Type, usize, Option<syn::Ident>)>> {
        let mut initializer_param_list: Vec<Option<(_, _, _, _)>> = Vec::new();
        for (comp_def_idx, comp_is_mandatory) in comp_def_idx_and_is_mandatory_list {
            let comp_def = &comp_defs[comp_def_idx];
            for (field_idx_in_variant, field_def) in
                AdtVariantDefinition::iter_owned_fields_with_index_in_components(this, comp_def)
            {
                if field_idx_in_variant >= initializer_param_list.len() {
                    initializer_param_list.resize_with(field_idx_in_variant + 1, Default::default);
                }
                assert!(initializer_param_list[field_idx_in_variant].is_none());
                let variant_ctor_param_ident =
                    field_def.accessor_or_generative_naming_ident(field_idx_in_variant);
                let variant_ctor_param_ty =
                    field_def.ctor_param_ty(comp_is_mandatory, use_keyed, repo_crate_ident);

                let variant_ctor_param_comp_def_idx = comp_def_idx;
                let variant_ctor_param_comp_field_name = field_def.field.ident.clone();
                initializer_param_list[field_idx_in_variant] = Some((
                    variant_ctor_param_ident,
                    variant_ctor_param_ty,
                    variant_ctor_param_comp_def_idx,
                    variant_ctor_param_comp_field_name,
                ));
            }
        }
        initializer_param_list
    }

    fn prepare_adt_initializer_tokens(
        initializer_param_list: impl Iterator<
            Item = Option<(syn::Ident, syn::Type, usize, Option<syn::Ident>)>,
        >,
        allow_empty_slot: bool,
    ) -> Vec<TokenStream2> {
        initializer_param_list
            .flat_map(|initializer_param: Option<_>| {
                let Some((param_ident, param_ty, _, _)) = initializer_param.as_ref() else {
                    if !allow_empty_slot {
                        unreachable!()
                    }
                    return None;
                };
                Some(quote!(#param_ident: #param_ty,))
            })
            .collect()
    }

    fn prepare_dispose_comp_statements(
        comp_list: impl Iterator<Item = (usize, bool)>,
        comp_defs: &Vec<AdtComponentDefinition>,
        use_keyed: bool,
        repo_crate_ident: &syn::Ident,
        repo_input_ident: &syn::Ident,
        repo_ty: &syn::Path,
    ) -> Vec<TokenStream2> {
        comp_list
            .map(|(comp_def_idx, dont_skip_missing)| {
                let comp_def = &comp_defs[comp_def_idx];
                assert!(comp_def.always_mandatory.not());
                let comp_ty = comp_def.component_name.camel_case_ident();
                if !dont_skip_missing {
                    quote! {{
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage_mut(#repo_input_ident);
                        __comp_storage.remove(__id);
                    }}
                } else {
                    quote! {{
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage_mut(#repo_input_ident);
                        let _ = __comp_storage.remove(__id).unwrap();
                    }}
                }
                
            })
            .collect()
    }

    fn prepare_allocate_id_comp_statements(
        this: &Self,
        comp_list: impl Iterator<Item = (usize, bool)>,
        comp_defs: &Vec<AdtComponentDefinition>,
        initalizer_param_list: &Vec<Option<(syn::Ident, syn::Type, usize, Option<syn::Ident>)>>,
        use_keyed: bool,
        repo_crate_ident: &syn::Ident,
        repo_input_ident: &syn::Ident,
        repo_ty: &syn::Path,
    ) -> Vec<TokenStream2> {
        comp_list
            .map(|(comp_def_idx, comp_is_mandatory)| {
                assert!(comp_is_mandatory);
                let comp_def = &comp_defs[comp_def_idx];
                let comp_ty = comp_def.component_name.camel_case_ident();
                assert!(comp_def.fields_named);
                let comp_style = AdtVariantCtorStyle::BraceNamed;

                let build_comp_expr = expr_build_adt(
                    comp_style,
                    comp_ty.clone().into(),
                    use_keyed,
                    !comp_is_mandatory,
                    initalizer_param_list
                        .iter()
                        .flat_map(|param| {
                            let (param_ident, _, param_comp_idx, param_field_name) =
                                param.as_ref()?;
                            if *param_comp_idx != comp_def_idx {
                                return None;
                            }
                            Some((param_field_name.as_ref(), param_ident))
                        }),
                );

                assert!(comp_is_mandatory);

                quote! {
                    __id = {
                        let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage_mut(#repo_input_ident);
                        __comp_storage.allocate_next(#build_comp_expr)
                    };
                }
            })
            .collect()
    }

    fn prepare_setup_comp_statements(
        this: &Self,
        comp_list: impl Iterator<Item = (usize, bool)>,
        comp_defs: &Vec<AdtComponentDefinition>,
        initalizer_param_list: &Vec<Option<(syn::Ident, syn::Type, usize, Option<syn::Ident>)>>,
        use_keyed: bool,
        repo_crate_ident: &syn::Ident,
        repo_input_ident: &syn::Ident,
        repo_ty: &syn::Path,
    ) -> Vec<TokenStream2> {
        comp_list
            .map(|(comp_def_idx, comp_is_mandatory)| {
                let comp_def = &comp_defs[comp_def_idx];
                let comp_ty = comp_def.component_name.camel_case_ident();
                let comp_style = this.ctor_style;

                let build_comp_expr = expr_build_adt(
                    comp_style,
                    comp_ty.clone().into(),
                    use_keyed,
                    !comp_is_mandatory,
                    initalizer_param_list
                        .iter()
                        .flat_map(|param| {
                            let (param_ident, _, param_comp_idx, param_field_name) =
                                param.as_ref()?;
                            if *param_comp_idx != comp_def_idx {
                                return None;
                            }
                            Some((param_field_name.as_ref(), param_ident))
                        }),
                );

                if !comp_is_mandatory {
                    todo!(); // FIXME
                }

                quote! {{
                    let __comp_storage = <#repo_ty as #repo_crate_ident ::component::HasComponent<#comp_ty>>::component_storage_mut(#repo_input_ident);
                    __comp_storage.append(__id, #build_comp_expr);
                }}
            })
            .collect()
    }

    pub(crate) fn build_entity_ctor(
        this: WithIndex<&Self>,
        comp_defs: &Vec<AdtComponentDefinition>,
        repo_crate_ident: &syn::Ident,
        default_span: proc_macro2::Span,
        repo_ty: &syn::Path,
        handle_vis: &syn::Visibility,
        handle_ident: &syn::Ident,
    ) -> proc_macro2::TokenStream {
        let ctor_ident = this.default_ctor_fn_name("new", default_span);
        let use_keyed = false;

        let mandatory_list = this.calc_transition_appending_mandatory_components(None);
        let optional_list = this.calc_transition_appending_optional_components(None);

        let setup_comp_list = mandatory_list
            .map(|x| (x, true))
            .chain(optional_list.map(|x| (x, false)));

        let mut variant_ctor_param_list = Self::prepare_adt_initializer_param_list(
            this,
            setup_comp_list.clone(),
            comp_defs,
            use_keyed,
            repo_crate_ident,
        );

        let repo_input_ident = syn::Ident::new("repo", handle_ident.span());

        let alloc_id_components: Vec<TokenStream2> = Self::prepare_allocate_id_comp_statements(
            &this,
            setup_comp_list
                .clone()
                .filter(|(comp_def_idx, comp_is_mandatory)| comp_defs[*comp_def_idx].is_inherent),
            comp_defs,
            &variant_ctor_param_list,
            use_keyed,
            repo_crate_ident,
            &repo_input_ident,
            repo_ty,
        );

        let setup_new_components: Vec<TokenStream2> = Self::prepare_setup_comp_statements(
            &this,
            setup_comp_list
                .filter(|(comp_def_idx, comp_is_mandatory)| !comp_defs[*comp_def_idx].is_inherent),
            comp_defs,
            &variant_ctor_param_list,
            use_keyed,
            repo_crate_ident,
            &repo_input_ident,
            repo_ty,
        );

        let ctor_args =
            Self::prepare_adt_initializer_tokens(variant_ctor_param_list.into_iter(), false);

        quote! {
            #[allow(non_snake_case)]
            #handle_vis fn #ctor_ident(#(#ctor_args)* #repo_input_ident: &mut #repo_ty, ) -> Self {
                let __id;
                #(#alloc_id_components)*
                #(#setup_new_components)*
                #handle_ident {
                    repo_id: <#repo_ty as #repo_crate_ident ::repo::Repo>::repo_id(#repo_input_ident),
                    entity_id: __id
                }
            }
        }
    }

    pub(crate) fn build_entity_ttor(
        this: WithIndex<&Self>,
        src_var_def: WithIndex<&Self>,
        comp_defs: &Vec<AdtComponentDefinition>,
        repo_crate_ident: &syn::Ident,
        default_span: proc_macro2::Span,
        repo_ty: &syn::Path,
        handle_vis: &syn::Visibility,
        handle_ident: &syn::Ident,
    ) -> proc_macro2::TokenStream {
        let ttor_ident = this.default_ttor_fn_name(
            "transition",
            Some("to"),
            Some("from"),
            &src_var_def,
            default_span,
        );
        let use_keyed = false;

        let mandatory_list =
            this.calc_transition_appending_mandatory_components(Some(&src_var_def));
        let mandatory_list_override = this
            .calc_transition_strengthen_from_optional_to_mandatory_components(Some(&src_var_def));
        let optional_list = this.calc_transition_appending_optional_components(Some(&src_var_def));
        let removal_list = this.calc_transition_removal_mandatory_components(Some(&src_var_def));
        let removal_list_skipmissing =
            this.calc_transition_removal_optional_components(Some(&src_var_def));

        let repo_input_ident = syn::Ident::new("repo", handle_ident.span());

        let dispose_comp_list = removal_list
            .map(|x| (x, true))
            .chain(removal_list_skipmissing.map(|x| (x, false)))
            .chain(mandatory_list_override.clone().map(|x| (x, false)));

        let setup_comp_list = mandatory_list
            .map(|x| (x, true))
            .chain(mandatory_list_override.map(|x| (x, true)))
            .chain(optional_list.map(|x| (x, false)));

        let mut variant_ttor_param_list = Self::prepare_adt_initializer_param_list(
            this,
            setup_comp_list.clone(),
            comp_defs,
            use_keyed,
            repo_crate_ident,
        );

        let dispose_old_components: Vec<TokenStream2> = Self::prepare_dispose_comp_statements(
            dispose_comp_list,
            comp_defs,
            use_keyed,
            repo_crate_ident,
            &repo_input_ident,
            repo_ty,
        );
        let setup_new_components: Vec<TokenStream2> = Self::prepare_setup_comp_statements(
            &this,
            setup_comp_list,
            comp_defs,
            &variant_ttor_param_list,
            use_keyed,
            repo_crate_ident,
            &repo_input_ident,
            repo_ty,
        );

        let ttor_args =
            Self::prepare_adt_initializer_tokens(variant_ttor_param_list.into_iter(), true);

        quote!(
            #[allow(non_snake_case)]
            #handle_vis fn #ttor_ident(self, #(#ttor_args)* #repo_input_ident: &mut #repo_ty) {
                let __repo_id = self.repo_id;
                let __id = self.entity_id;
                assert_eq!(__repo_id, <#repo_ty as #repo_crate_ident ::repo::Repo>::repo_id(#repo_input_ident));
                // FIXME: could check discriminant here.
                #(#dispose_old_components)*
                #(#setup_new_components)*
            }
        )
    }
}

fn expr_build_adt<'a>(
    adt_ctor_style: AdtVariantCtorStyle,
    adt_ctor: syn::Path,
    use_keyed: bool,
    is_values_wrapped_in_some: bool,
    param_list: impl Iterator<Item = (Option<&'a syn::Ident>, &'a syn::Ident)>,
) -> proc_macro2::TokenStream {
    match adt_ctor_style {
        AdtVariantCtorStyle::BraceNamed => {
            if !use_keyed {
                let param_list = param_list
                    .map(|(field_name, field_value)| {
                        let field_name = field_name.unwrap();
                        let field_value = if !is_values_wrapped_in_some {
                            quote!(#field_value)
                        } else {
                            quote!((#field_value).unwrap())
                        };
                        quote!(#field_name: #field_value,)
                    })
                    .collect::<Vec<_>>();
                quote!(
                    #adt_ctor{#(#param_list)*}
                )
            } else {
                unimplemented!() // FIXME
            }
        }
        AdtVariantCtorStyle::ParenUnamed => {
            let param_list = param_list
                .map(|(field_name, field_value)| {
                    assert!(field_name.is_none());
                    let field_value = if !is_values_wrapped_in_some {
                        quote!(#field_value)
                    } else {
                        quote!((#field_value).unwrap())
                    };

                    quote!(#field_value , )
                })
                .collect::<Vec<_>>();
            quote!(
                #adt_ctor(#(#param_list)*)
            )
        }
        AdtVariantCtorStyle::Unit => {
            assert!(param_list.count() == 0);
            quote!(#adt_ctor)
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum AdtVariantCtorStyle {
    BraceNamed,
    ParenUnamed,
    Unit,
}

impl AdtVariantCtorStyle {
    pub(crate) fn from_non_unit_is_named(is_named: bool) -> Self {
        if is_named {
            AdtVariantCtorStyle::BraceNamed
        } else {
            AdtVariantCtorStyle::ParenUnamed
        }
    }
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
