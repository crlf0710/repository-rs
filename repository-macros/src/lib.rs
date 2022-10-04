#![allow(dead_code, unused)]

use crate::utils::IdentPair;
use crate::utils::IdentText;
use crate::utils::TyWrap;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, spanned::Spanned};

#[macro_use]
mod utils;

mod adt;

#[proc_macro_attribute]
pub fn repo(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as utils::AttributeArgs);
    let option_storage_field = unwrap_result_in_macro!(args.get_ident_with_name("storage_field"));
    let mut item = parse_macro_input!(item as adt::ItemStructOnly);
    let option_crate_name = ident_crate_repo(item.span());
    let repo_ident = item.name();
    let storage_field = if let Some(field_name) = &option_storage_field {
        field_name.clone()
    } else {
        syn::Ident::new_raw("storage", repo_ident.span())
    };
    let storage_ty = syn::parse_quote!(#option_crate_name ::repo::Storage<Self>);
    item.append_or_replace_field(storage_field.clone(), storage_ty);
    let members =
        unwrap_result_in_macro!(adt::attrs_take_with_ident_name(item.attrs_mut(), "member")
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
        quote!(<#member as #option_crate_name ::component::ComponentList>::create_component_list::<Self>(repo_id))
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

            fn create_component_lists_storage(repo_id: #option_crate_name::id::Id) -> Self::ComponentListsStorage {
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

fn ident_crate_repo(span: proc_macro2::Span) -> syn::Ident {
    syn::Ident::new_raw("repo", span)
}

fn path_crate_repo(span: proc_macro2::Span) -> syn::Path {
    syn::Path {
        leading_colon: None,
        segments: syn::punctuated::Punctuated::from_iter(vec![
            syn::PathSegment::from(<syn::Token![crate]>::default()),
            syn::PathSegment::from(syn::Ident::new_raw("Repo", span)),
        ]),
    }
}

fn path_repo_error_component_not_present(
    span: proc_macro2::Span,
    repo_ident: syn::Ident,
) -> syn::Path {
    syn::Path {
        leading_colon: None,
        segments: syn::punctuated::Punctuated::from_iter(vec![
            syn::PathSegment::from(repo_ident),
            syn::PathSegment::from(syn::Ident::new_raw("error", span)),
            syn::PathSegment::from(syn::Ident::new_raw("ComponentNotPresent", span)),
        ]),
    }
}

fn attr_allow_non_camel_case_types(span: proc_macro2::Span) -> syn::Attribute {
    syn::Attribute {
        pound_token: Default::default(),
        style: syn::AttrStyle::Outer,
        bracket_token: Default::default(),
        path: syn::Ident::new_raw("allow", span).into(),
        tokens: quote!((non_camel_case_types)),
    }
}

fn type_unit(span: proc_macro2::Span) -> syn::Type {
    syn::Type::Tuple(syn::TypeTuple {
        paren_token: Default::default(),
        elems: Default::default(),
    })
}

#[proc_macro_attribute]
pub fn interned(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as utils::AttributeArgs);
    // let option_shared = utils::has_str_meta_in_args(&args, "shared");   // FIXME: NOT SUPPORTED YET
    let option_data = unwrap_result_in_macro!(args.get_ident_with_name("data"));
    let option_repo_ty = unwrap_result_in_macro!(args.get_path_with_name("repo"));
    let mut item = parse_macro_input!(item as adt::ItemStructOrEnumOrUnion);
    let option_crate_name = ident_crate_repo(item.span());

    let derives = adt::attrs_take_with_ident_name(item.attrs_mut(), "derive").collect::<Vec<_>>();

    let handle_ident = item.name();
    let handle_vis = item.vis();
    validate_name_in_macro!(handle_ident);
    let data_ident = if let Some(ident) = &option_data {
        ident.clone()
    } else {
        let data_suffix = "Data";
        ident_from_combining!(handle_ident.span() => handle_ident.ident_text(), data_suffix)
    };
    let repo_ty = if let Some(ty) = &option_repo_ty {
        ty.clone()
    } else {
        path_crate_repo(handle_ident.span())
    };

    let data_storage =
        quote!(#option_crate_name ::component_storage::DenseUniqStorageWithId<#data_ident>);
    let mut data_struct = item.clone_with_name(data_ident.clone());
    data_struct.borrow_mut().attrs_mut().extend(derives);
    data_struct
        .borrow_mut()
        .attrs_mut()
        .extend(Some(attr_allow_non_camel_case_types(handle_ident.span())));

    let (var_defs, comp_defs) = unwrap_result_in_macro!(adt::collect_adt_variants_and_components(
        data_struct.borrow_mut(),
        adt::AdtKind::Interned,
        &utils::IdentPair::from_camel_case_ident(data_ident.clone()),
    ));

    let ctor_definitions = var_defs
        .iter()
        .enumerate()
        .map(|(var_def_idx, var_def)| {
            let ctor_ident = var_def.default_ctor_fn_name(handle_ident.span());
            assert!(var_def.optional_components.is_empty());
            let mut ctor_param_list: Vec<Option<(syn::Ident, syn::Type, Option<syn::Ident>)>> =
                Vec::new();
            for comp_def_idx in var_def.mandatory_components.iter().cloned() {
                let comp_def = &comp_defs[comp_def_idx];
                let applicable_variants_pos = comp_def
                    .applicable_variants
                    .iter()
                    .position(|variant_idx_in_list| *variant_idx_in_list == var_def_idx)
                    .expect("variant not found in `applicable_variants`");
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
                        syn::Ident::new_raw(
                            &format!("field_{ctor_param_pos}"),
                            field_def.field.span(),
                        )
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
                adt::AdtVariantCtorStyle::BraceNamed => {
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
                adt::AdtVariantCtorStyle::ParenUnamed => {
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
                adt::AdtVariantCtorStyle::Unit => quote!(
                    #ctor_path
                ),
            };
            quote! {
                #[allow(non_snake_case)]
                #handle_vis fn #ctor_ident(#(#ctor_args)* __repo: &mut #repo_ty, ) -> Self {
                    let data = #ctor_expr;
                    Self::with_data(data, __repo)
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
                    todo!("shared by multiple variants")
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
        #[derive(Copy, Clone, PartialEq, Debug)]
        #handle_vis struct #handle_ident {
            repo_id: #option_crate_name ::id::Id,
            interned_id: #option_crate_name ::id::Id,
        }

        impl #option_crate_name ::component::ComponentList for #handle_ident {
            type ComponentListDef = (#data_storage,);

            fn create_component_list<R: #option_crate_name ::repo::Repo>(repo_id: #option_crate_name::id::Id) -> Self::ComponentListDef {
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

            fn with_data(data: #data_ident, repo: &mut #repo_ty) -> Self {
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
    let mut item = parse_macro_input!(item as adt::ItemStructOrEnum);
    let option_crate_name = ident_crate_repo(item.span());
    let derives = adt::attrs_take_with_ident_name(item.attrs_mut(), "derive").collect::<Vec<_>>();

    let handle_ident = item.name();
    validate_name_in_macro!(handle_ident);
    let handle_vis = item.vis();
    let inherent_ident = option_inherent
        .clone()
        .unwrap_or_else(|| ident_from_combining!(handle_ident.span() => handle_ident, "Inherent"));

    let repo_ty = option_repo_ty
        .clone()
        .unwrap_or_else(|| path_crate_repo(handle_ident.span()));

    let (var_defs, comp_defs) = unwrap_result_in_macro!(adt::collect_adt_variants_and_components(
        item.borrow_mut(),
        adt::AdtKind::Entity,
        &utils::IdentPair::from_camel_case_ident(inherent_ident.clone())
    ));

    let mut component_definitions: Vec<TokenStream2> = Vec::new();
    let mut component_storages: Vec<TokenStream2> = Vec::new();
    let mut component_storage_new_expr: Vec<TokenStream2> = Vec::new();

    for comp_def in comp_defs.iter() {
        let mut comp_attrs = item
            .attrs()
            .iter()
            .cloned()
            .chain(derives.iter().cloned())
            .chain(Some(attr_allow_non_camel_case_types(handle_ident.span())))
            .collect::<Vec<_>>();
        let comp_name = comp_def.component_name.camel_case_ident();
        let comp_fields = adt::build_fields_from_field_iter(
            if comp_def.fields_named {
                adt::AdtVariantCtorStyle::BraceNamed
            } else {
                adt::AdtVariantCtorStyle::ParenUnamed
            },
            comp_def.fields.iter().map(|x| x.field.clone()),
        );

        let comp_def_struct = syn::ItemStruct {
            attrs: comp_attrs,
            vis: handle_vis.clone(),
            ident: comp_name.clone(),
            fields: comp_fields,
            struct_token: Default::default(),
            generics: Default::default(),
            semi_token: Default::default(),
        };

        component_definitions.push(quote! {
            #comp_def_struct
        });

        let (storage, storage_new_expr) = adt::component_storage_ty_and_build_expr(
            adt::AdtKind::Entity,
            &option_crate_name,
            &comp_name,
            &quote!(kiosk_entry),
            comp_def.is_inherent,
            comp_def.always_mandatory,
        );
        component_storages.push(storage);
        component_storage_new_expr.push(storage_new_expr);
    }

    let ctor_definitions = var_defs
        .iter()
        .enumerate()
        .map(|(var_def_idx, var_def)| {
            let ctor_ident = var_def.default_ctor_fn_name(handle_ident.span());
            let mut ctor_param_list: Vec<Option<(syn::Ident, syn::Type, Option<syn::Ident>, usize)>> =
                Vec::new();
            let mut ctor_comp_expr_list: Vec<proc_macro2::TokenStream> = Vec::new();
            for (comp_def_idx, comp_is_mandatory) in var_def
                .mandatory_components
                .iter()
                .map(|x| (*x, true))
                .chain(var_def.optional_components.iter().map(|x| (*x, false)))
            {
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
                        syn::Ident::new_raw(
                            &format!("field_{ctor_param_pos}"),
                            field_def.field.span(),
                        )
                    };
                    let ctor_param_inner_ty = field_def.field.ty.clone();
                    let ctor_param_ty = if comp_is_mandatory {
                        ctor_param_inner_ty
                    } else {
                        ctor_param_inner_ty.wrap_ty_with_option()
                    };
                    let ctor_param_field_name = field_def.field.ident.clone();
                    ctor_param_list[ctor_param_pos] =
                        Some((ctor_param_ident, ctor_param_ty, ctor_param_field_name, comp_def_idx));
                }

                let use_keyed = false;
                let comp_name = comp_def.component_name.camel_case_ident();
                let comp_def_index = syn::Index::from(comp_def_idx);
                let comp_style = if comp_def.is_inherent {
                    adt::AdtVariantCtorStyle::BraceNamed
                } else {
                    var_def.ctor_style
                };
                let comp_expr = match comp_style {
                    adt::AdtVariantCtorStyle::BraceNamed => {
                        let comp_args = if !use_keyed {
                            ctor_param_list
                                .iter()
                                .flat_map(|ctor_param| {
                                    let (ctor_param_ident, _, ctor_param_field_name, ctor_param_comp_idx) =
                                        ctor_param.as_ref().unwrap();
                                    if *ctor_param_comp_idx != comp_def_idx{
                                        return None;
                                    }
                                    let ctor_param_field_name = ctor_param_field_name.as_ref().unwrap();
                                    Some(quote!(#ctor_param_field_name: #ctor_param_ident,))
                                })
                                .collect::<Vec<_>>()
                        } else {
                            unimplemented!() // FIXME
                        };
                        quote!(
                            #comp_name{#(#comp_args)*}
                        )
                    }
                    adt::AdtVariantCtorStyle::ParenUnamed => {
                        let comp_args = ctor_param_list
                            .iter()
                            .flat_map(|ctor_param| {
                                let (ctor_param_ident, _, _, ctor_param_comp_idx) = ctor_param.as_ref().unwrap();
                                if *ctor_param_comp_idx != comp_def_idx{
                                    return None;
                                }
                                Some(quote!(#ctor_param_ident , ))
                            })
                            .collect::<Vec<_>>();
                        quote!(
                            #comp_name(#(#comp_args)*)
                        )
                    }
                    adt::AdtVariantCtorStyle::Unit => quote!(
                        #comp_name
                    ),
                };

                let ctor_comp_expr = if comp_def.is_inherent {
                    assert!(comp_is_mandatory);
                    assert!(comp_def.fields_named);
                    quote! {
                        let __id;
                        {
                            __id = __comp_list.#comp_def_index.allocate_next(#comp_expr);
                        }
                    }
                } else {
                    if comp_is_mandatory {
                        quote! {{
                            __comp_list.#comp_def_index.append(__id, #comp_expr);
                        }}
                    } else {
                        quote! {{
                            todo!();   // FIXME
                        }}
                    }
                };
                ctor_comp_expr_list.push(ctor_comp_expr);
            }

            let use_keyed = false;
            let ctor_style = var_def.ctor_style;
            let ctor_path = &var_def.ctor_path;
            let ctor_args = if !use_keyed {
                ctor_param_list
                    .iter()
                    .map(|ctor_param| {
                        let (ctor_param_ident, ctor_param_ty, _, _) = ctor_param.as_ref().unwrap();
                        quote!(#ctor_param_ident: #ctor_param_ty,)
                    })
                    .collect::<Vec<_>>()
            } else {
                unimplemented!() // FIXME
            };
            quote! {
                #[allow(non_snake_case)]
                #handle_vis fn #ctor_ident(#(#ctor_args)* __repo: &mut #repo_ty, ) -> Self {
                    use #option_crate_name ::repo::Repo;
                    let __comp_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list_mut(__repo);
                    #(#ctor_comp_expr_list)*
                    let repo_id = __repo.repo_id();
                    #handle_ident {
                        repo_id,
                        entity_id: __id
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    let accessor_definitions = comp_defs
        .iter()
        .enumerate()
        .flat_map(|(comp_def_idx, comp_def)| {
            let option_crate_name = option_crate_name.clone();
            let repo_ty = repo_ty.clone();
            let handle_ident = handle_ident.clone();
            comp_def.fields.iter().map(move |field_def| {
                let accessor_ident = if let Some(ident_pair) = &field_def.accessor {
                    ident_pair.snake_case_ident()
                } else if let Some(ident) = &field_def.field.ident {
                    ident.clone()
                } else {
                    return quote! {};
                };
                let accessor_setter_ident = ident_from_combining!(accessor_ident.span() => "set", accessor_ident);
                let accessor_vis = field_def.vis();
                let getter_ret_inner_ty = field_def.field.ty.clone();
                let setter_ret_inner_ty = type_unit(proc_macro2::Span::call_site());
                let setter_input_ty = field_def.field.ty.clone();
                let error_ty_component_not_present = syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: path_repo_error_component_not_present(
                        field_def.field.ty.span(),
                        option_crate_name.clone(),
                    ),
                });
                let getter_ret_ty = if comp_def.always_mandatory {
                    getter_ret_inner_ty
                } else {
                    let error_ty = error_ty_component_not_present.clone();
                    getter_ret_inner_ty.wrap_ty_with_result(error_ty)
                };
                let setter_ret_ty = if comp_def.always_mandatory {
                    setter_ret_inner_ty
                } else {
                    let error_ty = error_ty_component_not_present.clone();
                    setter_ret_inner_ty.wrap_ty_with_result(error_ty)
                };


                assert!(!comp_def.applicable_variants.is_empty());
                let comp_name = comp_def.component_name.camel_case_ident();
                let is_named = field_def.field.ident.is_some();
                let comp_def_index = syn::Index::from(comp_def_idx);
                let (comp_expr, comp_mut_expr, ret_expr) = if comp_def.always_mandatory {
                    (
                        quote! {
                            let __comp_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list(__repo);
                            let __comp = __comp_list.#comp_def_index.get(self.entity_id).unwrap()
                        },
                        quote! {
                            let __comp_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list_mut(__repo);
                            let __comp = __comp_list.#comp_def_index.get_mut(self.entity_id).unwrap()
                        },
                        quote! {
                            __value
                        }
                    )
                } else {
                    let error_ty = error_ty_component_not_present.clone();
                    (
                        quote! {
                            let __comp_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list(__repo);
                            let __comp = __comp_list.#comp_def_index.get(self.entity_id).ok_or(#error_ty)?;
                        },
                        quote! {
                            let __comp_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list_mut(__repo);
                            let __comp = __comp_list.#comp_def_index.get_mut(self.entity_id).ok_or(#error_ty)?;
                        },
                        quote! {
                            Ok(__value)
                        }
                    )
                };
                let getter_expr = {
                    if is_named {
                        let field_name = field_def.field.ident.as_ref().unwrap();
                        quote!{{
                            #comp_expr
                            let __value = if let #comp_name{#field_name: value, ..} = __comp {
                                value.clone()
                            } else {
                                unreachable!()
                            };
                            #ret_expr
                        }}
                    } else {
                        let mut pattern = TokenStream2::new();
                        for _ in 0..field_def.indexes_in_applicable_variants[0] {
                            pattern.extend(quote!(_,));
                        }
                        pattern.extend(quote!(value, ..));
                        quote!{{
                            #comp_expr
                            let __value = if let #comp_name(#pattern) = __comp {
                                value.clone()
                            } else {
                                unreachable!()
                            };
                            #ret_expr
                        }}
                    }
                };
                let setter_expr  = {
                    if is_named {
                        let field_name = field_def.field.ident.as_ref().unwrap();
                        quote!{{
                            #comp_mut_expr
                            __comp.#field_name = value;
                            let __value = ();
                            #ret_expr
                        }}
                    } else {
                        let idx = syn::Index::from(field_def.indexes_in_applicable_variants[0]);
                        quote!{{
                            #comp_mut_expr
                            __comp.#idx = value;
                            let __value = ();
                            #ret_expr
                        }}
                    }
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
                let setter_impl = if field_def.has_setter() {
                    quote! {
                        #accessor_vis fn #accessor_setter_ident(self, value: #setter_input_ty, __repo: &mut #repo_ty) -> #setter_ret_ty {
                            #setter_expr
                        }
                    }
                } else {
                    quote! {}
                };
                quote! {
                    #getter_impl
                    #setter_impl
                }
            })
        })
        .collect::<Vec<_>>();
    let handle_definition = quote! {
        #[derive(Copy, Clone, PartialEq, Debug)]
        #handle_vis struct #handle_ident {
            repo_id: #option_crate_name ::id::Id,
            entity_id: #option_crate_name ::id::Id,
        }

        impl #option_crate_name ::component::ComponentList for #handle_ident {
            type ComponentListDef = (#(#component_storages,)*);

            fn create_component_list<R: #option_crate_name ::repo::Repo>(repo_id: #option_crate_name::id::Id) -> Self::ComponentListDef {
                let kiosk_entry = #option_crate_name ::entity::kiosk_entity_entry::<#handle_ident>(repo_id);
                (#(#component_storage_new_expr,)*)
            }
        }

        impl #option_crate_name ::entity::Entity for #handle_ident {
        }

        impl #handle_ident {
            #(#ctor_definitions)*
            #(#accessor_definitions)*
        }
    };

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
        &keyed_value.key.ident_text(),
        keyed_value.key.span(),
    ));
    let value_expr = keyed_value.value;

    let option_crate_name = ident_crate_repo(proc_macro2::Span::call_site());

    quote! {
        #option_crate_name ::keyed_value::KeyedValue::<#key_str, _>::new(#value_expr)
    }
    .into()
}
