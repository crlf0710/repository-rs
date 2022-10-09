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

fn repo_macro_impl(attr: TokenStream, item: TokenStream) -> TokenStream {
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

fn type_keyed_value(
    default_span: proc_macro2::Span,
    key: &str,
    value_ty: syn::Type,
    repo_crate_ident: &syn::Ident,
) -> syn::Type {
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: syn::punctuated::Punctuated::from_iter(vec![
                syn::PathSegment {
                    ident: repo_crate_ident.clone(),
                    arguments: Default::default(),
                },
                syn::PathSegment {
                    ident: syn::Ident::new("keyed_value", default_span),
                    arguments: Default::default(),
                },
                syn::PathSegment {
                    ident: syn::Ident::new("KeyedValue", default_span),
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            colon2_token: Default::default(),
                            lt_token: Default::default(),
                            args: syn::punctuated::Punctuated::from_iter(vec![
                                syn::GenericArgument::Const(syn::Expr::Lit(syn::ExprLit {
                                    attrs: Default::default(),
                                    lit: syn::Lit::Str(syn::LitStr::new(key, default_span)),
                                })),
                                syn::GenericArgument::Type(value_ty),
                            ]),
                            gt_token: Default::default(),
                        },
                    ),
                },
            ]),
        },
    })
}

fn trait_into_type(default_span: proc_macro2::Span, ty: syn::Type) -> syn::TraitBound {
    syn::TraitBound {
        path: syn::Path {
            leading_colon: None,
            segments: syn::punctuated::Punctuated::from_iter(vec![syn::PathSegment {
                ident: syn::Ident::new("Into", default_span),
                arguments: syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments {
                        args: syn::punctuated::Punctuated::from_iter(vec![
                            syn::GenericArgument::Type(ty),
                        ]),
                        colon2_token: Default::default(),
                        lt_token: Default::default(),
                        gt_token: Default::default(),
                    },
                ),
            }]),
        },
        modifier: syn::TraitBoundModifier::None,
        paren_token: Default::default(),
        lifetimes: Default::default(),
    }
}

fn interned_macro_impl(attr: TokenStream, item: TokenStream, use_keyed: bool) -> TokenStream {
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
            let ctor_ident = var_def.default_ctor_fn_name("new", handle_ident.span());
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
                let getter_impl = if field_def.has_getter(false) {
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
            marker: #option_crate_name ::__priv::std::marker::PhantomData<
                #option_crate_name ::__priv::std::rc::Rc<()>>
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

            fn repo_id(&self) -> #option_crate_name ::id::Id {
                self.repo_id
            }

            fn interned_id(&self) -> #option_crate_name ::id::Id {
                self.interned_id
            }
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
                    interned_id,
                    marker: #option_crate_name ::__priv::std::marker::PhantomData,
                }
            }

            fn data(self, repo: &#repo_ty) -> &#data_ident
            {
                use #option_crate_name ::repo::Repo;
                assert_eq!(self.repo_id, repo.repo_id());
                let component = <#repo_ty as #option_crate_name ::component::HasComponent<#data_ident>>::component_storage(repo);
                <_ as #option_crate_name ::component::ComponentStorage<#repo_ty>>::data_by_id(component, self.interned_id).unwrap()
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

            fn data_by_id(&self, id: #option_crate_name ::id::Id) -> Option<&Self::Data> {
                self.checked_get(id)
            }
        }

    );

    quote!(
        #handle_definition
        #data_definition
    )
    .into()
}

fn entity_macro_impl(attr: TokenStream, item: TokenStream, use_keyed: bool) -> TokenStream {
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

    for (comp_def_idx, comp_def) in comp_defs.iter().enumerate() {
        let mut comp_attrs = item
            .attrs()
            .iter()
            .cloned()
            .chain(derives.iter().cloned())
            .chain(Some(attr_allow_non_camel_case_types(handle_ident.span())))
            .collect::<Vec<_>>();
        let comp_name = comp_def.component_name.camel_case_ident();
        let comp_fields = adt::build_fields_from_field_iter(
            adt::AdtVariantCtorStyle::from_non_unit_is_named(comp_def.fields_named),
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

        let (storage, storage_new_expr) = adt::component_storage_ty_and_build_expr(
            adt::AdtKind::Entity,
            &option_crate_name,
            &comp_name,
            &quote!(kiosk_entry),
            comp_def.is_inherent,
            comp_def.always_mandatory,
        );
        let comp_def_index = syn::Index::from(comp_def_idx);

        component_definitions.push(quote! {
            #comp_def_struct

            impl #option_crate_name ::component::HasComponent<#comp_name> for #repo_ty {
                type Storage = #storage;

                fn component_storage(&self) -> &Self::Storage {
                    let component_list = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list(self);
                    &component_list.#comp_def_index
                }
                fn component_storage_mut(&mut self) -> &mut Self::Storage {
                    let component_list_mut = <#repo_ty as #option_crate_name ::component::HasComponentList<#handle_ident>>::component_list_mut(self);
                    &mut component_list_mut.#comp_def_index
                }
            }

            impl #option_crate_name ::component::Component<#repo_ty> for #comp_name {

            }

            impl #option_crate_name ::component::ComponentStorage<#repo_ty> for #storage {
                type Data = #comp_name;

                fn data_by_id(&self, id: #option_crate_name ::id::Id) -> Option<&Self::Data> {
                    self.checked_get(id)
                }
            }
        });
        component_storages.push(storage);
        component_storage_new_expr.push(storage_new_expr);
    }

    let mut ctor_definitions = Vec::new();
    let mut stor_definitions = Vec::new();
    let mut ttor_definitions = Vec::new();

    let is_singleton = var_defs
        .iter()
        .any(|var_def| var_def.singleton_init_args.is_some());
    for (var_def_idx, var_def) in var_defs.iter().enumerate() {
        let var_def = utils::WithIndex::from_index_and_inner(var_def_idx, var_def);
        if !is_singleton {
            let ctor_definition = adt::AdtVariantDefinition::build_entity_ctor(
                var_def,
                &comp_defs,
                use_keyed,
                &option_crate_name,
                handle_ident.span(),
                &repo_ty,
                &handle_vis,
                &handle_ident,
            );
            ctor_definitions.push(ctor_definition);
        } else if let Some(stor_args) = var_def.singleton_init_args.as_ref() {
            let stor_args_values = match stor_args {
                proc_macro2::TokenTree::Group(g) => {
                    if matches!(g.delimiter(), proc_macro2::Delimiter::Parenthesis) {
                        Some(g.stream())
                    } else {
                        None
                    }
                }
                _ => None,
            };
            let stor_args_values = unwrap_result_in_macro!(stor_args_values
                .ok_or_else(|| syn::Error::new(stor_args.span(), "Invalid singleton initializer")));
            let stor_definition = adt::AdtVariantDefinition::build_entity_stor(
                var_def,
                stor_args_values,
                &comp_defs,
                use_keyed,
                &option_crate_name,
                handle_ident.span(),
                &repo_ty,
                &handle_vis,
                &handle_ident,
            );
            stor_definitions.push(stor_definition);
        }
        for (src_var_def_idx, src_var_def) in var_defs.iter().enumerate() {
            if src_var_def_idx == var_def_idx {
                continue;
            }
            let src_var_def = utils::WithIndex::from_index_and_inner(src_var_def_idx, src_var_def);
            let ttor_definition = adt::AdtVariantDefinition::build_entity_ttor(
                var_def,
                src_var_def,
                &comp_defs,
                use_keyed,
                &option_crate_name,
                handle_ident.span(),
                &repo_ty,
                &handle_vis,
                &handle_ident,
            );
            ttor_definitions.push(ttor_definition);
        }
    }

    let mut accessor_definitions = Vec::new();
    let mut hidden_accessor_definitions = Vec::new();
    for (comp_def_idx, comp_def) in comp_defs.iter().enumerate() {
        assert!(!comp_def.applicable_variants.is_empty());
        for (field_idx, field_def) in comp_def.fields.iter().enumerate() {
            let (accessor_definition, hidden_accessor_definition) =
                adt::GeneralizedField::build_entity_accessor(
                    utils::WithIndex::from_index_and_inner(field_idx, field_def),
                    &option_crate_name,
                    handle_ident.span(),
                    &repo_ty,
                    &handle_ident,
                    &comp_def.component_name.camel_case_ident().into(),
                    comp_def.always_mandatory,
                );
            accessor_definitions.push(accessor_definition);
            hidden_accessor_definitions.push(hidden_accessor_definition);
        }
    }

    let handle_definition = quote! {
        #[derive(Copy, Clone, PartialEq, Debug)]
        #handle_vis struct #handle_ident {
            repo_id: #option_crate_name ::id::Id,
            entity_id: #option_crate_name ::id::Id,
            marker: #option_crate_name ::__priv::std::marker::PhantomData<
                #option_crate_name ::__priv::std::rc::Rc<()>>
        }

        impl #option_crate_name ::component::ComponentList for #handle_ident {
            type ComponentListDef = (#(#component_storages,)*);

            fn create_component_list<R: #option_crate_name ::repo::Repo>(repo_id: #option_crate_name::id::Id) -> Self::ComponentListDef {
                let kiosk_entry = #option_crate_name ::entity::kiosk_entity_entry::<#handle_ident>(repo_id);
                (#(#component_storage_new_expr,)*)
            }
        }

        impl #option_crate_name ::entity::Entity for #handle_ident {
            fn repo_id(&self) -> #option_crate_name ::id::Id {
                self.repo_id
            }
            fn entity_id(&self) -> #option_crate_name ::id::Id {
                self.entity_id
            }
        }

        impl #handle_ident {
            #(#ctor_definitions)*
            #(#stor_definitions)*
            #(#ttor_definitions)*
            #(#accessor_definitions)*
        }

        #(#hidden_accessor_definitions)*
    };

    quote!(
        #handle_definition
        #(#component_definitions)*
    )
    .into()
}

fn keyed_macro_impl(input: TokenStream, non_fallback: bool) -> TokenStream {
    struct KeyedValue {
        key: syn::Ident,
        colon: syn::Token![:],
        value: syn::Expr,
    }

    impl syn::parse::Parse for KeyedValue {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let key: syn::Ident = input.parse()?;
            if input.is_empty() {
                return Ok(KeyedValue {
                    key: key.clone(),
                    colon: Default::default(),
                    value: syn::Expr::Path(syn::ExprPath {
                        attrs: Default::default(),
                        qself: Default::default(),
                        path: key.into(),
                    }),
                });
            }
            Ok(KeyedValue {
                key,
                colon: input.parse()?,
                value: input.parse()?,
            })
        }
    }

    let keyed_value = parse_macro_input!(input as KeyedValue);
    if non_fallback {
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
    } else {
        let value = keyed_value.value;
        quote! {
            (#value)
        }
        .into()
    }
}

#[proc_macro_attribute]
pub fn repo(attr: TokenStream, item: TokenStream) -> TokenStream {
    repo_macro_impl(attr, item)
}

#[proc_macro_attribute]
pub fn interned(attr: TokenStream, item: TokenStream) -> TokenStream {
    interned_macro_impl(attr, item, true)
}

#[proc_macro_attribute]
pub fn interned_without_keyed(attr: TokenStream, item: TokenStream) -> TokenStream {
    interned_macro_impl(attr, item, false)
}

#[proc_macro_attribute]
pub fn entity(attr: TokenStream, item: TokenStream) -> TokenStream {
    entity_macro_impl(attr, item, true)
}

#[proc_macro_attribute]
pub fn entity_without_keyed(attr: TokenStream, item: TokenStream) -> TokenStream {
    entity_macro_impl(attr, item, false)
}

#[proc_macro]
pub fn keyed(input: TokenStream) -> TokenStream {
    keyed_macro_impl(input, true)
}

#[proc_macro]
pub fn keyed_fallback(input: TokenStream) -> TokenStream {
    keyed_macro_impl(input, false)
}
