#![allow(dead_code, unused)]

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, quote_spanned};
use syn::{parse::Parse, parse_macro_input, spanned::Spanned};

#[macro_use]
mod utils;

#[proc_macro_attribute]
pub fn repo(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as utils::AttributeArgs);
    let option_crate_name = quote!(repo);
    let option_storage_field = unwrap_result_in_macro!(args.get_ident_with_name("storage_field"));
    let mut item = parse_macro_input!(item as utils::ItemStructOnly);
    let repo_ident = item.name();
    let storage_field = if let Some(field_name) = &option_storage_field {
        field_name.clone()
    } else {
        syn::Ident::new("storage", repo_ident.span())
    };
    let storage_ty = syn::parse_quote!(#option_crate_name ::repo::Storage<Self>);
    item.append_or_replace_field(storage_field.clone(), storage_ty);
    let members = unwrap_result_in_macro!(item
        .take_attr_with_ident_name("member")
        .map(|x| utils::GeneralizedMeta::parse_attribute(&x).and_then(|x| x.get_path_values()))
        .collect::<Result<Vec<_>, _>>());
    let members = members
        .into_iter()
        .flat_map(|x| x.into_iter())
        .collect::<Vec<_>>();

    let component_lists_storage_tys = members.iter().map(|member| {
        quote!(<#member as #option_crate_name ::component::ComponentList>::ComponentListDef)
    }).collect::<Vec<_>>();

    let component_lists_storage_builds = members.iter().map(|member| {
        quote!(<#member as #option_crate_name ::component::ComponentList>::create_component_list(repo_id, routes))
    }).collect::<Vec<_>>();

    let repo_impl = quote! {
        impl #option_crate_name ::repo::Repo for #repo_ident {
            type ParentRepo = Self;
            type ComponentListsStorage = (#(#component_lists_storage_tys,)*);

            fn storage(&self) -> &#option_crate_name ::repo::Storage<Self> {
                &self.#storage_field
            }

            fn storage_mut(&mut self) -> &mut #option_crate_name ::repo::Storage<Self> {
                &mut self.#storage_field
            }

            fn create_component_lists_storage(repo_id: #option_crate_name::id::Id, routes: &mut #option_crate_name ::repo::Routes<Self>)
                -> Self::ComponentListsStorage {
                (#(#component_lists_storage_builds,)*)
            }
        }
    };

    let component_list_impls = members
        .iter()
        .enumerate()
        .map(|(member_idx, member)| {
            let member_idx = syn::Index::from(member_idx);
            quote! {
                impl #option_crate_name ::component::HasComponentList<#member> for #repo_ident {
                    fn component_list(&self) -> &<#member as #option_crate_name ::component::ComponentList>::ComponentListDef {
                        use #option_crate_name ::repo::Repo;
                        &self.storage().component_lists_storage.#member_idx
                    }

                    fn component_list_mut(&mut self) -> &mut <#member as #option_crate_name ::component::ComponentList>::ComponentListDef {
                        use #option_crate_name ::repo::Repo;
                        &mut self.storage_mut().component_lists_storage.#member_idx
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        #item
        #repo_impl
        #( #component_list_impls )*
    }
    .into()
}

fn path_crate_repo(span: proc_macro2::Span) -> syn::Path {
    syn::Path {
        leading_colon: None,
        segments: syn::punctuated::Punctuated::from_iter(vec![
            syn::PathSegment::from(<syn::Token![crate]>::default()),
            syn::PathSegment::from(syn::Ident::new("Repo", span)),
        ]),
    }
}

#[derive(Debug)]
struct AdtVariantDefinition {
    variant_name: Option<utils::IdentPair>,
    ctor_path: syn::Path,
    ctor_style: AdtVariantCtorStyle,
    mandatory_components: Vec<usize>,
    optional_components: Vec<usize>,
}

#[derive(Clone, Copy, Debug)]
enum AdtVariantCtorStyle {
    BraceNamed,
    ParenUnamed,
    Unit,
}

#[derive(Debug)]
struct AdtGroupComponentDefinition {
    component_name: Option<utils::IdentPair>,
    always_mandatory: bool,
    applicable_variants: Vec<usize>,
    fields_named: bool,
    fields: Vec<utils::GeneralizedField>,
    field_indexes_in_applicable_variants: Vec<Vec<usize>>,
}

#[derive(Clone, Copy)]
enum AdtKind {
    Interned,
    Entity,
}

fn collect_adt_variants_and_components(
    item: utils::ItemAdtMutRef<'_>,
    adt_kind: AdtKind,
) -> syn::Result<(Vec<AdtVariantDefinition>, Vec<AdtGroupComponentDefinition>)> {
    use syn::spanned::Spanned;
    use utils::PushAndGetWithIndex;
    let mut variant_defs = vec![];
    let mut group_comp_defs = vec![];
    match item {
        utils::ItemAdtMutRef::Struct(syn::ItemStruct {
            ident: item_name,
            fields,
            ..
        }) => {
            let (variant_def_idx, variant_def) =
                variant_defs.push_and_get_with_index(AdtVariantDefinition {
                    variant_name: None,
                    ctor_path: item_name.clone().into(),
                    ctor_style: match fields {
                        syn::Fields::Named(_) => AdtVariantCtorStyle::BraceNamed,
                        syn::Fields::Unnamed(_) => AdtVariantCtorStyle::ParenUnamed,
                        syn::Fields::Unit => AdtVariantCtorStyle::Unit,
                    },
                    mandatory_components: Vec::new(),
                    optional_components: Vec::new(),
                });

            let (group_comp_def_idx, group_comp_def) =
                group_comp_defs.push_and_get_with_index(AdtGroupComponentDefinition {
                    component_name: None,
                    always_mandatory: true,
                    applicable_variants: vec![variant_def_idx],
                    fields_named: !matches!(fields, syn::Fields::Unnamed { .. }),
                    fields: Vec::new(),
                    field_indexes_in_applicable_variants: vec![Vec::new()],
                });
            variant_def.mandatory_components.push(group_comp_def_idx);
            for (field_idx, field) in fields.iter_mut().enumerate() {
                group_comp_def
                    .fields
                    .push(utils::GeneralizedField::new_from_syn_field(
                        field, field_idx, false,
                    )?);
            }
        }
        utils::ItemAdtMutRef::Enum(syn::ItemEnum {
            ident: item_name,
            variants,
            ..
        }) => {
            for variant in variants {
                if let (AdtKind::Entity, Some((_, e))) = (adt_kind, &variant.discriminant) {
                    return Err(syn::Error::new(
                        e.span(),
                        "enumerate entity cannot specify discriminant",
                    ));
                }
                let (variant_def_idx, variant_def) =
                    variant_defs.push_and_get_with_index(AdtVariantDefinition {
                        variant_name: Some(utils::IdentPair::from_camel_case_ident(
                            variant.ident.clone(),
                        )),
                        ctor_path: syn::Path {
                            leading_colon: None,
                            segments: syn::punctuated::Punctuated::from_iter(vec![
                                syn::PathSegment {
                                    ident: item_name.clone(),
                                    arguments: syn::PathArguments::None,
                                },
                                syn::PathSegment {
                                    ident: variant.ident.clone(),
                                    arguments: syn::PathArguments::None,
                                },
                            ]),
                        },
                        ctor_style: match variant.fields {
                            syn::Fields::Named(_) => AdtVariantCtorStyle::BraceNamed,
                            syn::Fields::Unnamed(_) => AdtVariantCtorStyle::ParenUnamed,
                            syn::Fields::Unit => AdtVariantCtorStyle::Unit,
                        },
                        mandatory_components: Vec::new(),
                        optional_components: Vec::new(),
                    });
                let (group_comp_def_idx, group_comp_def) =
                    group_comp_defs.push_and_get_with_index(AdtGroupComponentDefinition {
                        component_name: None,
                        always_mandatory: true,
                        applicable_variants: vec![variant_def_idx],
                        fields_named: !matches!(variant.fields, syn::Fields::Unnamed { .. }),
                        fields: Vec::new(),
                        field_indexes_in_applicable_variants: vec![Vec::new()],
                    });
                variant_def.mandatory_components.push(group_comp_def_idx);
                for (field_idx, field) in variant.fields.iter_mut().enumerate() {
                    group_comp_def
                        .fields
                        .push(utils::GeneralizedField::new_from_syn_field(
                            field, field_idx, false,
                        )?);
                }
            }
        }
        utils::ItemAdtMutRef::Union(syn::ItemUnion {
            ident: item_name,
            fields,
            ..
        }) => {
            for field in &mut fields.named {
                let variant_name = field
                    .ident
                    .clone()
                    .map(utils::IdentPair::from_snake_case_ident);
                let (variant_def_idx, variant_def) =
                    variant_defs.push_and_get_with_index(AdtVariantDefinition {
                        variant_name,
                        ctor_path: item_name.clone().into(),
                        ctor_style: AdtVariantCtorStyle::BraceNamed,
                        mandatory_components: Vec::new(),
                        optional_components: Vec::new(),
                    });
                let (group_comp_def_idx, group_comp_def) =
                    group_comp_defs.push_and_get_with_index(AdtGroupComponentDefinition {
                        component_name: None,
                        always_mandatory: true,
                        applicable_variants: vec![variant_def_idx],
                        fields_named: false,
                        fields: Vec::new(),
                        field_indexes_in_applicable_variants: vec![Vec::new()],
                    });
                variant_def.mandatory_components.push(group_comp_def_idx);
                group_comp_def
                    .fields
                    .push(utils::GeneralizedField::new_from_syn_field(field, 0, true)?);
            }
        }
    }
    Ok((variant_defs, group_comp_defs))
}

#[proc_macro_attribute]
pub fn interned(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as utils::AttributeArgs);
    // let option_shared = utils::has_str_meta_in_args(&args, "shared");   // FIXME: NOT SUPPORTED YET
    let option_data = unwrap_result_in_macro!(args.get_ident_with_name("data"));
    let option_repo_ty = unwrap_result_in_macro!(args.get_path_with_name("repo"));
    let option_crate_name = quote!(repo);

    let mut item = parse_macro_input!(item as utils::ItemStructOrEnumOrUnion);

    let derives = item.take_attr_with_ident_name("derive").collect::<Vec<_>>();

    let handle_ident = item.name();
    let handle_vis = item.vis();
    validate_name_in_macro!(handle_ident);
    let data_ident = if let Some(ident) = &option_data {
        ident.clone()
    } else {
        let data_suffix = "Data";
        ident_from_combining!(handle_ident.span() => handle_ident.to_string(), data_suffix)
    };
    let repo_ty = if let Some(ty) = &option_repo_ty {
        ty.clone()
    } else {
        path_crate_repo(handle_ident.span())
    };

    let data_storage =
        quote!(#option_crate_name ::component_storage::DenseUniqStorageWithId<#data_ident>);
    let mut data_struct = item.clone_with_name(data_ident.clone());
    data_struct.borrow_mut().append_attrs(derives);

    let (var_defs, comp_defs) = unwrap_result_in_macro!(collect_adt_variants_and_components(
        data_struct.borrow_mut(),
        AdtKind::Interned
    ));

    let ctor_definitions = var_defs
        .iter()
        .enumerate()
        .map(|(var_def_idx, var_def)| {
            let ctor_ident = if let Some(ident_pair) = &var_def.variant_name {
                ident_from_combining!(ident_pair.span => "new", ident_pair.snake_case)
            } else {
                syn::Ident::new("new", handle_ident.span())
            };
            assert!(var_def.optional_components.is_empty());
            let mut ctor_param_list: Vec<Option<(syn::Ident, syn::Type, Option<syn::Ident>)>> =
                Vec::new();
            for comp_def_idx in var_def.mandatory_components.iter().cloned() {
                let comp_def = &comp_defs[comp_def_idx];
                let applicable_variants_pos = comp_def
                    .applicable_variants
                    .iter()
                    .position(|variant_idx_in_list| *variant_idx_in_list == var_def_idx)
                    .unwrap();
                for field_def in comp_def.fields.iter() {
                    let ctor_param_pos =
                        field_def.indexes_in_applicable_variants[applicable_variants_pos];
                    if ctor_param_pos >= ctor_param_list.len() {
                        ctor_param_list.resize_with(ctor_param_pos + 1, Default::default);
                    }
                    assert!(ctor_param_list[ctor_param_pos].is_none());
                    let ctor_param_ident = if let Some(ident_pair) = &field_def.accessor {
                        ident_pair.snake_case_ident()
                    } else if let Some(ident) = &field_def.field.ident {
                        ident.clone()
                    } else {
                        syn::Ident::new(&format!("field_{ctor_param_pos}"), field_def.field.span())
                    };
                    let ctor_param_ty = field_def.field.ty.clone();
                    let ctor_param_field_name = field_def.field.ident.clone();
                    ctor_param_list[ctor_param_pos] =
                        Some((ctor_param_ident, ctor_param_ty, ctor_param_field_name));
                }
            }
            let use_keyed = false;
            let ctor_style = var_def.ctor_style;
            let ctor_path = &var_def.ctor_path;
            let ctor_args = if !use_keyed {
                ctor_param_list
                    .iter()
                    .map(|ctor_param| {
                        let (ctor_param_ident, ctor_param_ty, _) = ctor_param.as_ref().unwrap();
                        quote!(#ctor_param_ident: #ctor_param_ty,)
                    })
                    .collect::<Vec<_>>()
            } else {
                unimplemented!() // FIXME
            };
            let ctor_expr = match ctor_style {
                AdtVariantCtorStyle::BraceNamed => {
                    let ctor_args = if !use_keyed {
                        ctor_param_list
                            .iter()
                            .map(|ctor_param| {
                                let (ctor_param_ident, _, ctor_param_field_name) =
                                    ctor_param.as_ref().unwrap();
                                let ctor_param_field_name = ctor_param_field_name.as_ref().unwrap();
                                quote!(#ctor_param_field_name: #ctor_param_ident,)
                            })
                            .collect::<Vec<_>>()
                    } else {
                        unimplemented!() // FIXME
                    };
                    quote!(
                        #ctor_path{#(#ctor_args)*}
                    )
                }
                AdtVariantCtorStyle::ParenUnamed => {
                    let ctor_args = ctor_param_list
                        .iter()
                        .map(|ctor_param| {
                            let (ctor_param_ident, _, _) = ctor_param.as_ref().unwrap();
                            quote!(#ctor_param_ident , )
                        })
                        .collect::<Vec<_>>();
                    quote!(
                        #ctor_path(#(#ctor_args)*)
                    )
                }
                AdtVariantCtorStyle::Unit => quote!(
                    #ctor_path
                ),
            };
            quote! {
                #handle_vis fn #ctor_ident(__repo: &mut #repo_ty, #(#ctor_args)* ) -> Self {
                    let data = #ctor_expr;
                    Self::with_data(__repo, data)
                }
            }
        })
        .collect::<Vec<_>>();

    let accessor_definitions = comp_defs
        .iter()
        .flat_map(|comp_def| {
            comp_def.fields.iter().map(|field_def| {
                let accessor_ident = if let Some(ident_pair) = &field_def.accessor {
                    ident_pair.snake_case_ident()
                } else if let Some(ident) = &field_def.field.ident {
                    ident.clone()
                } else {
                    return quote! {};
                };
                let accessor_vis = field_def.vis();
                let getter_ret_ty = field_def.field.ty.clone();
                assert!(!comp_def.applicable_variants.is_empty());
                let getter_expr = if comp_def.applicable_variants.len() == 1 {
                    let var_def_idx = comp_def.applicable_variants[0];
                    let var_def = &var_defs[var_def_idx];
                    let ctor_path = &var_def.ctor_path;
                    let is_named = field_def.field.ident.is_some();
                    if is_named {
                        let field_name = field_def.field.ident.as_ref().unwrap();
                        quote!{{
                            if let #ctor_path{#field_name: value, ..} = self.data(__repo) {
                                value.clone()
                            } else {
                                unreachable!()
                            }
                        }}
                    } else {
                        let mut pattern = TokenStream2::new();
                        for _ in 0..field_def.indexes_in_applicable_variants[0] {
                            pattern.extend(quote!(_,));
                        }
                        pattern.extend(quote!(value, ..));
                        quote!{{
                            if let #ctor_path(#pattern) = self.data(__repo) {
                                value.clone()
                            } else {
                                unreachable!()
                            }
                        }}
                    }
                } else {
                    todo!()
                };
                let getter_impl = if field_def.has_getter() {
                    quote! {
                        #accessor_vis fn #accessor_ident(self, __repo: &#repo_ty) -> #getter_ret_ty {
                            #getter_expr
                        }
                    }
                } else {
                    quote! {}
                };
                quote! {
                    #getter_impl
                }
            })
        })
        .collect::<Vec<_>>();

    let handle_definition = quote!(
        #[derive(Copy, Clone)]
        #handle_vis struct #handle_ident {
            repo_id: #option_crate_name ::id::Id,
            interned_id: #option_crate_name ::id::Id,
        }

        impl #option_crate_name ::component::ComponentList for #handle_ident {
            type ComponentListDef = (#data_storage,);

            fn create_component_list<R: #option_crate_name ::repo::Repo>(repo_id: #option_crate_name::id::Id,
                routes: &mut #option_crate_name ::repo::Routes<R>) -> Self::ComponentListDef {
                /*
                FIXME: check whether routing is useful to us;
                 */
                let kiosk_entry = #option_crate_name ::interned::kiosk_nonsharing_interned_entry::<#handle_ident>(repo_id);
                (<#data_storage>::new(kiosk_entry),)
            }
        }

        impl #option_crate_name ::interned::Interned for #handle_ident {
            const SHARING_BETWEEN_REPOS: bool = false;
        }


        impl #handle_ident {
            #(#ctor_definitions)*

            #(#accessor_definitions)*

            fn with_data(repo: &mut #repo_ty, data: #data_ident) -> Self {
                use #option_crate_name ::repo::Repo;
                let repo_id = repo.repo_id();
                let component = <#repo_ty as #option_crate_name ::component::HasComponent<#data_ident>>::component_storage_mut(repo);
                let interned_id = component.intern(data);
                #handle_ident {
                    repo_id,
                    interned_id
                }
            }

            fn data(self, repo: &#repo_ty) -> &#data_ident
            {
                use #option_crate_name ::repo::Repo;
                assert_eq!(self.repo_id, repo.repo_id());
                let component = <#repo_ty as #option_crate_name ::component::HasComponent<#data_ident>>::component_storage(repo);
                <_ as #option_crate_name ::component::ComponentStorage<#repo_ty>>::data_by_id(component, self.interned_id)
            }
        }

    );

    let data_definition = quote!(
        #data_struct

        impl #option_crate_name ::component::HasComponent<#data_ident> for #repo_ty {
            type Storage = #data_storage;

            fn component_storage(&self) -> &Self::Storage {
                let component_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list(self);
                &component_list.0
            }
            fn component_storage_mut(&mut self) -> &mut Self::Storage {
                let component_list_mut = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list_mut(self);
                &mut component_list_mut.0
            }
        }

        impl #option_crate_name ::component::Component<#repo_ty> for #data_ident {

        }

        impl #option_crate_name ::component::ComponentStorage<#repo_ty> for #data_storage {
            type Data = #data_ident;

            fn data_by_id(&self, id: #option_crate_name ::id::Id) -> &Self::Data {
                self.get(id)
            }
        }

    );

    quote!(
        #handle_definition
        #data_definition
    )
    .into()
}

#[proc_macro_attribute]
pub fn entity(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as utils::AttributeArgs);
    let option_inherent = unwrap_result_in_macro!(args.get_ident_with_name("inherent"));
    let option_repo_ty = unwrap_result_in_macro!(args.get_path_with_name("repo"));
    let option_crate_name = quote!(repo);

    let mut item = parse_macro_input!(item as utils::ItemStructOrEnum);
    let derives = item.take_attr_with_ident_name("derive").collect::<Vec<_>>();

    let handle_ident = item.name();
    validate_name_in_macro!(handle_ident);
    let inherent_ident = if let Some(ident) = &option_inherent {
        ident.clone()
    } else {
        let inherent_suffix = "Inherent";
        ident_from_combining!(handle_ident.span() => handle_ident.to_string(), inherent_suffix)
    };
    let repo_ty = if let Some(ty) = &option_repo_ty {
        ty.clone()
    } else {
        path_crate_repo(handle_ident.span())
    };

    let (var_defs, comp_defs) = unwrap_result_in_macro!(collect_adt_variants_and_components(
        item.borrow_mut(),
        AdtKind::Entity
    ));

    let component_definitions: Vec<TokenStream2> = Vec::new();

    let handle_definition = quote!();

    quote!(
        #handle_definition
        #(#component_definitions)*
    )
    .into()
}

#[proc_macro]
pub fn keyed(input: TokenStream) -> TokenStream {
    struct KeyedValue {
        key: syn::Ident,
        colon: syn::Token![:],
        value: syn::Expr,
    }

    impl syn::parse::Parse for KeyedValue {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            Ok(KeyedValue {
                key: input.parse()?,
                colon: input.parse()?,
                value: input.parse()?,
            })
        }
    }
    let keyed_value = parse_macro_input!(input as KeyedValue);
    let key_str = syn::Lit::Str(syn::LitStr::new(
        &keyed_value.key.to_string(),
        keyed_value.key.span(),
    ));
    let value_expr = keyed_value.value;

    let option_crate_name = quote!(repo);

    quote! {
        #option_crate_name ::keyed_value::KeyedValue::<#key_str, _>::new(#value_expr)
    }
    .into()
}
