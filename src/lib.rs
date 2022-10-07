#![allow(unused_parens, type_alias_bounds, dead_code)]
#![cfg_attr(feature = "keyed", feature(adt_const_params))]
#![cfg_attr(feature = "keyed", allow(incomplete_features))]

pub mod id {
    use std::num::NonZeroUsize;

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    pub struct Id(pub(crate) NonZeroUsize);

    impl Id {
        pub(crate) const ONE: Id = Id(match NonZeroUsize::new(1) {
            Some(v) => v,
            None => panic!(),
        });

        pub(crate) const INVALID: Id = Id(match NonZeroUsize::new(usize::MAX) {
            Some(v) => v,
            None => panic!(),
        });
    }
}

mod kiosk {
    use std::{any::TypeId, cell::Cell, num::NonZeroUsize};

    use elsa::{FrozenBTreeMap, FrozenVec};

    use crate::id::Id;

    pub(crate) struct Kiosk {
        entries: FrozenVec<Box<KioskEntry>>,
    }

    impl Default for Kiosk {
        fn default() -> Self {
            let root_entry = KioskEntry::default();
            Kiosk {
                entries: FrozenVec::from_iter(Some(Box::new(root_entry))),
            }
        }
    }

    impl Kiosk {
        pub(crate) fn entry_root(&self) -> &KioskEntry {
            self.entries.get(0).unwrap()
        }

        pub(crate) fn entry_with_path(
            &self,
            selectors: impl IntoIterator<Item = KioskSelector>,
        ) -> &KioskEntry {
            let mut entry = self.entries.get(0).unwrap();
            for selector in selectors {
                let next = match entry.child_entries.get(&selector) {
                    Some(next_id) => self.entries.get(next_id.0.get()).unwrap(),
                    None => {
                        let new_entry = KioskEntry::default();
                        let new_entry_id = self.entries.len();
                        let new_entry = self.entries.push_get(Box::new(new_entry));
                        let new_entry_id = Id(NonZeroUsize::new(new_entry_id).unwrap());
                        entry.child_entries.insert(selector, Box::new(new_entry_id));
                        new_entry
                    }
                };
                entry = next;
            }
            entry
        }

        pub(crate) fn entry_id_with_path(
            &self,
            selectors: impl IntoIterator<Item = KioskSelector>,
        ) -> Option<Id> {
            let mut entry = self.entries.get(0).unwrap();
            let mut entry_id = None;
            for selector in selectors {
                let next = match entry.child_entries.get(&selector) {
                    Some(next_id) => {
                        entry_id = Some(next_id.clone());
                        self.entries.get(next_id.0.get()).unwrap()
                    }
                    None => {
                        let new_entry = KioskEntry::default();
                        let new_entry_id = self.entries.len();
                        let new_entry = self.entries.push_get(Box::new(new_entry));
                        let new_entry_id = Id(NonZeroUsize::new(new_entry_id).unwrap());
                        entry_id = Some(new_entry_id);
                        entry.child_entries.insert(selector, Box::new(new_entry_id));
                        new_entry
                    }
                };
                entry = next;
            }
            entry_id
        }

        pub(crate) fn entry_with_id(&self, id: Id) -> &KioskEntry {
            let idx = id.0.get();
            self.entries.get(idx).unwrap()
        }
    }

    pub(crate) struct KioskEntry {
        child_entries: FrozenBTreeMap<KioskSelector, Box<Id>>,
        next_id: Cell<Id>,
    }

    impl Default for KioskEntry {
        fn default() -> Self {
            KioskEntry {
                child_entries: FrozenBTreeMap::new(),
                next_id: Cell::new(Id::ONE),
            }
        }
    }

    impl KioskEntry {
        pub(crate) fn allocate(&self) -> Option<Id> {
            let cur_id = self.next_id.get();
            if cur_id != Id::INVALID {
                let next_id = Id(NonZeroUsize::new(cur_id.0.get() + 1).unwrap());
                self.next_id.set(next_id);
                Some(cur_id)
            } else {
                None
            }
        }
    }

    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
    pub(crate) enum KioskSelector {
        Token(&'static str),
        TypeId(TypeId),
        Id(Id),
    }
}

#[cfg(feature = "keyed")]
pub mod keyed_value {
    pub struct KeyedValue<const KEY: &'static str, T>(pub T);
    impl<const KEY: &'static str, T> From<T> for KeyedValue<KEY, T> {
        fn from(v: T) -> Self {
            KeyedValue(v)
        }
    }

    impl<const KEY: &'static str, T> KeyedValue<KEY, T> {
        pub fn new(v: T) -> Self {
            KeyedValue(v)
        }
        pub fn into_inner(self) -> T {
            self.0
        }
    }
}

pub mod error {
    #[derive(Clone, Copy, Debug)]
    pub struct ComponentNotPresent;
}

pub mod repo {
    use crate::component::Component;
    use crate::id::Id;
    use crate::kiosk::{Kiosk, KioskSelector};

    thread_local! {
        pub(crate) static KIOSK: Kiosk = Kiosk::default();
    }

    pub trait Repo: Sized {
        type ParentRepo: Repo;
        type ComponentListsStorage;

        fn storage(&self) -> &Storage<Self>;

        fn storage_mut(&mut self) -> &mut Storage<Self>;

        fn create_component_lists_storage(repo_id: Id) -> Self::ComponentListsStorage;

        fn repo_id(&self) -> Id {
            self.storage().repo_id
        }
    }

    fn allocate_repo_id() -> Option<Id> {
        KIOSK.with(|kiosk| {
            let entry = kiosk.entry_with_path([REPO_KIOSK_SELECTOR]);
            entry.allocate()
        })
    }

    pub struct Storage<R: Repo> {
        repo_id: Id,
        parent_repo_id: Option<Id>,
        pub component_lists_storage: <R as Repo>::ComponentListsStorage,
    }

    impl<R: Repo> Default for Storage<R> {
        fn default() -> Self {
            let repo_id = allocate_repo_id().unwrap();
            let component_lists_storage = R::create_component_lists_storage(repo_id);
            Storage {
                repo_id,
                parent_repo_id: None,
                component_lists_storage,
            }
        }
    }

    pub(crate) const REPO_KIOSK_SELECTOR: KioskSelector = KioskSelector::Token("repo");

    pub type DynRoute<R: Repo> = dyn Fn(&R) -> (&dyn Component<R>) + Send + Sync;

    pub type DynMutRoute<R: Repo> = dyn Fn(&mut R) -> (&mut dyn Component<R>) + Send + Sync;
}

pub mod component {
    use crate::id::Id;
    use crate::repo::Repo;

    pub trait HasComponentList<CL>
    where
        CL: ComponentList,
    {
        fn component_list(&self) -> &CL::ComponentListDef;
        fn component_list_mut(&mut self) -> &mut CL::ComponentListDef;
    }

    pub trait ComponentList {
        type ComponentListDef;

        fn create_component_list<R: Repo>(repo_id: Id) -> Self::ComponentListDef;
    }

    pub trait HasComponent<C>: Repo
    where
        C: Component<Self>,
    {
        type Storage: ComponentStorage<Self>;
        fn component_storage(&self) -> &Self::Storage;
        fn component_storage_mut(&mut self) -> &mut Self::Storage;
    }

    pub trait Component<R: Repo> {}

    pub trait ComponentStorage<R: Repo> {
        type Data;
        fn data_by_id(&self, id: Id) -> Option<&Self::Data>;
    }

    pub trait ComponentStorageMut<R: Repo>: ComponentStorage<R> {
        fn data_by_id_mut(&self, id: Id) -> Option<&mut Self::Data>;
    }
}

pub mod component_storage {
    use crate::id::Id;
    use crate::repo::KIOSK;
    use indexmap::IndexSet;
    use std::collections::BTreeMap;
    use std::hash::Hash;
    use std::num::NonZeroUsize;

    pub struct DenseUniqStorage<D> {
        data: IndexSet<D>,
    }

    impl<D> DenseUniqStorage<D>
    where
        D: Hash + Eq,
    {
        fn new() -> Self {
            DenseUniqStorage {
                data: IndexSet::new(),
            }
        }

        #[inline]
        pub fn append(&mut self, id: Id, d: D) {
            let idx = id.0.get() - 1;
            assert_eq!(idx, self.data.len());
            let inserted = self.data.insert(d);
            assert!(inserted);
        }

        #[inline]
        pub fn find(&self, d: &D) -> Option<Id> {
            self.data
                .get_index_of(d)
                .map(|idx| Id(NonZeroUsize::new(idx + 1).unwrap()))
        }

        #[inline]
        pub fn get(&self, id: Id) -> &D {
            self.data.get_index(id.0.get() - 1).unwrap()
        }

        #[inline]
        pub fn checked_get(&self, id: Id) -> Option<&D> {
            self.data.get_index(id.0.get() - 1)
        }
    }

    pub struct DenseUniqStorageWithId<D> {
        kiosk_entry: Id,
        inner: DenseUniqStorage<D>,
    }

    impl<D> DenseUniqStorageWithId<D>
    where
        D: Hash + Eq,
    {
        pub fn new(kiosk_entry: Id) -> Self {
            DenseUniqStorageWithId {
                kiosk_entry,
                inner: DenseUniqStorage::new(),
            }
        }

        #[inline]
        pub fn intern(&mut self, d: D) -> Id {
            if let Some(id) = self.find(&d) {
                id
            } else {
                self.allocate_next(d)
            }
        }

        #[inline]
        pub fn allocate_next(&mut self, d: D) -> Id {
            let next_id = KIOSK
                .with(|kiosk| {
                    let entry = kiosk.entry_with_id(self.kiosk_entry);
                    entry.allocate()
                })
                .unwrap();
            self.append(next_id, d);
            next_id
        }

        #[inline]
        pub fn append(&mut self, id: Id, d: D) {
            self.inner.append(id, d);
        }

        #[inline]
        pub fn find(&self, d: &D) -> Option<Id> {
            self.inner.find(d)
        }

        #[inline]
        pub fn get(&self, id: Id) -> &D {
            self.inner.get(id)
        }

        #[inline]
        pub fn checked_get(&self, id: Id) -> Option<&D> {
            self.inner.checked_get(id)
        }
    }

    pub struct DenseStorage<D> {
        data: Vec<D>,
    }

    impl<D> DenseStorage<D> {
        pub fn new() -> Self {
            DenseStorage { data: Vec::new() }
        }

        #[inline]
        pub fn append(&mut self, id: Id, d: D) {
            let idx = id.0.get() - 1;
            assert_eq!(idx, self.data.len());
            self.data.push(d);
        }

        #[inline]
        pub fn get(&self, id: Id) -> &D {
            self.data.get((id.0.get() - 1)).unwrap()
        }

        #[inline]
        pub fn get_mut(&mut self, id: Id) -> &mut D {
            self.data.get_mut((id.0.get() - 1)).unwrap()
        }

        #[inline]
        pub fn checked_get(&self, id: Id) -> Option<&D> {
            self.data.get((id.0.get() - 1))
        }
    }

    pub struct SparseStorage<D> {
        data: BTreeMap<usize, D>,
    }

    impl<D> SparseStorage<D> {
        pub fn new() -> Self {
            SparseStorage {
                data: BTreeMap::new(),
            }
        }

        #[inline]
        pub fn append(&mut self, id: Id, d: D) {
            use std::collections::btree_map::Entry;
            let entry = self.data.entry(id.0.get() - 1);
            if matches!(entry, Entry::Occupied(..)) {
                unreachable!();
            }
            entry.or_insert(d);
        }

        #[inline]
        pub fn get(&self, id: Id) -> Option<&D> {
            self.data.get(&(id.0.get() - 1))
        }

        #[inline]
        pub fn get_mut(&mut self, id: Id) -> Option<&mut D> {
            self.data.get_mut(&(id.0.get() - 1))
        }

        #[inline]
        pub fn remove(&mut self, id: Id) -> Option<D> {
            self.data.remove(&(id.0.get() - 1))
        }

        #[inline]
        pub fn checked_get(&self, id: Id) -> Option<&D> {
            self.get(id)
        }
    }

    pub struct DenseStorageWithId<D> {
        kiosk_entry: Id,
        inner: DenseStorage<D>,
    }

    impl<D> DenseStorageWithId<D> {
        pub fn new(kiosk_entry: Id) -> Self {
            DenseStorageWithId {
                kiosk_entry,
                inner: DenseStorage::new(),
            }
        }

        #[inline]
        pub fn allocate_next(&mut self, d: D) -> Id {
            let next_id = KIOSK
                .with(|kiosk| {
                    let entry = kiosk.entry_with_id(self.kiosk_entry);
                    entry.allocate()
                })
                .unwrap();
            self.append(next_id, d);
            next_id
        }

        #[inline]
        pub fn append(&mut self, id: Id, d: D) {
            self.inner.append(id, d);
        }

        #[inline]
        pub fn get(&self, id: Id) -> &D {
            self.inner.get(id)
        }

        #[inline]
        pub fn checked_get(&self, id: Id) -> Option<&D> {
            self.inner.checked_get(id)
        }
    }
}

pub mod interned {
    use crate::id::Id;
    use crate::kiosk::KioskSelector;
    use crate::repo::KIOSK;
    use crate::repo::REPO_KIOSK_SELECTOR;
    use std::any::Any;
    use std::any::TypeId;

    const INTERNED_KIOSK_SELECTOR: KioskSelector = KioskSelector::Token("interned");

    pub trait Interned: Any + Sized {
        const SHARING_BETWEEN_REPOS: bool;
        fn repo_id(&self) -> Id;
        fn interned_id(&self) -> Id;
    }

    pub fn kiosk_sharing_interned_entry<T: Interned>() -> Id {
        assert!(!T::SHARING_BETWEEN_REPOS);
        KIOSK
            .with(|kiosk| {
                kiosk.entry_id_with_path([
                    INTERNED_KIOSK_SELECTOR,
                    KioskSelector::TypeId(TypeId::of::<T>()),
                ])
            })
            .unwrap()
    }

    pub fn kiosk_nonsharing_interned_entry<T: Interned>(repo_id: Id) -> Id {
        assert!(!T::SHARING_BETWEEN_REPOS);
        KIOSK
            .with(|kiosk| {
                kiosk.entry_id_with_path([
                    REPO_KIOSK_SELECTOR,
                    KioskSelector::Id(repo_id),
                    INTERNED_KIOSK_SELECTOR,
                    KioskSelector::TypeId(TypeId::of::<T>()),
                ])
            })
            .unwrap()
    }

    #[derive(Clone, Copy)]
    pub struct InternedIdxOf<T: Interned>(pub T);

    impl<T: Interned> PartialEq for InternedIdxOf<T> {
        fn eq(&self, other: &Self) -> bool {
            self.0.repo_id() == other.0.repo_id() && self.0.interned_id() == other.0.interned_id()
        }
    }
    impl<T: Interned> Eq for InternedIdxOf<T> {}

    impl<T: Interned> std::hash::Hash for InternedIdxOf<T> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.0.repo_id().hash(state);
            self.0.interned_id().hash(state);
        }
    }
}

pub mod entity {
    use std::any::Any;
    use std::any::TypeId;

    use crate::id::Id;
    use crate::kiosk::KioskSelector;
    use crate::repo::KIOSK;
    use crate::repo::REPO_KIOSK_SELECTOR;

    const ENTITY_KIOSK_SELECTOR: KioskSelector = KioskSelector::Token("entity");

    pub trait Entity: Any + Sized {
        fn repo_id(&self) -> Id;
        fn entity_id(&self) -> Id;
    }

    pub fn kiosk_entity_entry<T: Entity>(repo_id: Id) -> Id {
        KIOSK
            .with(|kiosk| {
                kiosk.entry_id_with_path([
                    REPO_KIOSK_SELECTOR,
                    KioskSelector::Id(repo_id),
                    ENTITY_KIOSK_SELECTOR,
                    KioskSelector::TypeId(TypeId::of::<T>()),
                ])
            })
            .unwrap()
    }

    #[derive(Clone, Copy)]
    pub struct EntityIdxOf<T: Entity>(pub T);

    impl<T: Entity> PartialEq for EntityIdxOf<T> {
        fn eq(&self, other: &Self) -> bool {
            self.0.repo_id() == other.0.repo_id() && self.0.entity_id() == other.0.entity_id()
        }
    }
    impl<T: Entity> Eq for EntityIdxOf<T> {}

    impl<T: Entity> std::hash::Hash for EntityIdxOf<T> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.0.repo_id().hash(state);
            self.0.entity_id().hash(state);
        }
    }
}

#[doc(hidden)]
pub mod __priv {
    #[doc(hidden)]
    pub use std;
}

pub use crate::repo::Storage;
pub use repository_macros::entity;
pub use repository_macros::interned;
#[cfg(feature = "keyed")]
pub use repository_macros::keyed;
#[cfg(not(feature = "keyed"))]
pub use repository_macros::keyed_fallback as keyed;
pub use repository_macros::repo;
