// Copyright 2020 Repository-rs Author(s)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ------------------------OR----------------------------
// Copyright 2020 Repository-rs Author(s)
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#![deny(warnings, missing_docs, missing_debug_implementations)]
//! A special kind of arena that can support storing multiple data types.
//!
//! `Repo` provides storage for multiple data types, it provides
//! its own kind of index handle called `EntityId` and its own kind of typed
//! handle called `EntityPtr<T>`. With these handles it allows the values
//! resident in the same `Repo` to reference each other easily.
//!
//! The data behind the `EntityPtr<T>` handle can be accessed when you have
//! a corresponding reference to the whole repository.
//!
//! The data behind the `EntityId` handle can be accessed when you know its
//! type and have a corresponding reference to the whole repository.
//!
//! Note that these handles may be reused after remove operation is executed.

extern crate alloc;

use crate::slab::Slab;
use alloc::alloc::Layout;
use alloc::rc::Rc;
use core::any::{Any, TypeId};
use core::fmt;
use core::marker::PhantomData;
use core::mem::ManuallyDrop;
use core::ops;
use hashbrown::raw::{self as hashbrown_raw, RawTable};
use std::hash::Hash;
use thiserror::Error;

fn type_id_to_u64(v: TypeId) -> u64 {
    use std::mem::transmute;
    unsafe { transmute(v) }
}

pub mod prealloc_tx;
mod slab;

/// A special kind of arena that can support storing multiple data types.
///
/// See the [module documentation] for more details.
///
/// [module documentation]: index.html
pub struct Repo {
    entity_catalog: Slab<EntityRecord>,
    entity_table: RawTable<EntityStorage>,
    marker: PhantomData<Rc<()>>,
}

impl Default for Repo {
    fn default() -> Self {
        Repo::new()
    }
}

impl fmt::Debug for Repo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{Repo}}")?;
        Ok(())
    }
}

impl Repo {
    /// Create a new `Repo`.
    pub fn new() -> Self {
        Repo {
            entity_catalog: Slab::new(),
            entity_table: RawTable::new(),
            marker: PhantomData,
        }
    }

    fn find_type_storage_bucket<T: Any>(&self) -> Option<hashbrown_raw::Bucket<EntityStorage>> {
        let type_id = TypeId::of::<T>();
        let bucket = self.entity_table.find(type_id_to_u64(type_id), |storage| {
            if storage.type_id != type_id {
                return false;
            }
            debug_assert_eq!(storage.type_layout, Layout::new::<T>());
            true
        })?;
        Some(bucket)
    }

    fn ensure_type_storage_bucket<T: Any>(&mut self) -> hashbrown_raw::Bucket<EntityStorage> {
        if let Some(bucket) = self.find_type_storage_bucket::<T>() {
            bucket
        } else {
            let type_id = TypeId::of::<T>();
            self.entity_table.insert(
                type_id_to_u64(type_id),
                EntityStorage::new::<T>(),
                |storage| type_id_to_u64(storage.type_id),
            )
        }
    }

    fn create_entity<T: Any>(&mut self, v: T) -> EntityId {
        let bucket = self.ensure_type_storage_bucket::<T>();
        let bucket_id = unsafe { self.entity_table.bucket_index(&bucket) };
        let storage_mut = unsafe { bucket.as_mut() };
        let (_, entity_id) = storage_mut
            .view_mut::<T>(&mut self.entity_catalog, bucket_id)
            .unwrap()
            .insert(v);
        EntityId(entity_id, PhantomData)
    }

    fn preallocate_entity<T: Any>(&mut self) -> EntityPtr<T> {
        let bucket = self.ensure_type_storage_bucket::<T>();
        let bucket_id = unsafe { self.entity_table.bucket_index(&bucket) };
        let storage_mut = unsafe { bucket.as_mut() };
        let storage_idx = storage_mut
            .view_mut::<T>(&mut self.entity_catalog, bucket_id)
            .unwrap()
            .preallocate();
        let record = EntityRecord {
            bucket_id,
            storage_idx,
        };
        EntityPtr {
            record,
            phantom: PhantomData,
        }
    }

    fn init_preallocate_entity<T:Any>(&mut self, record: EntityRecord, value: T) -> Result<(), (Error, T)> {
        let bucket = self.ensure_type_storage_bucket::<T>();
        let bucket_id = unsafe { self.entity_table.bucket_index(&bucket) };
        let storage_mut = unsafe { bucket.as_mut() };
        let mut storage_view_mut = storage_mut
            .view_mut::<T>(&mut self.entity_catalog, bucket_id)
            .unwrap();
        if !storage_view_mut.is_preallocated(record.storage_idx) {
            return Err((Error::InvalidPtr, value));
        }
        let _ = storage_view_mut.init_preallocated(record.storage_idx, value);
        Ok(())
    }

    fn cancel_preallocate_entity(&mut self, record: EntityRecord) -> Result<bool, Error> {
        let bucket_id = record.bucket_id;
        if bucket_id >= self.entity_table.buckets() {
            return Err(Error::InvalidPtr);
        }
        let bucket = unsafe { self.entity_table.bucket(bucket_id) };
        let storage_mut = unsafe { bucket.as_mut() };
        match storage_mut.untyped_reattach_vacant(record.storage_idx) {
            Ok(()) => Ok(true),
            Err(slab::EntryError::EntryIsOccupied) => Ok(false),
            Err(slab::EntryError::InvalidIdx) => Err(Error::InvalidPtr),
            Err(slab::EntryError::EntryIsVacant) => Err(Error::InvalidPtr),
            Err(slab::EntryError::EntryIsDetachedVacant) => unreachable!(),
        }
    }

    fn validate_record_type<T: Any>(&self, record: EntityRecord) -> bool {
        let bucket = match self.find_type_storage_bucket::<T>() {
            Some(bucket) => bucket,
            None => return false,
        };
        let bucket_id = unsafe { self.entity_table.bucket_index(&bucket) };
        bucket_id == record.bucket_id
    }

    /// Add a new value into the repository.
    /// Returning a pointer handle to this value.
    pub fn insert<T: Any>(&mut self, v: T) -> EntityPtr<T> {
        let entity_id = self.create_entity(v);
        let entity_ptr = entity_id
            .cast_ptr(self)
            .ok_or(Error::InternalError001)
            .unwrap();
        entity_ptr
    }

    /// Add a new value into the repository.
    /// Returning an index handle to this value.
    pub fn insert_for_id<T: Any>(&mut self, v: T) -> EntityId {
        self.create_entity(v)
    }

    /// Remove a value from the repository with its pointer handle.
    pub fn remove<T: Any>(&mut self, entity_ptr: EntityPtr<T>) -> Option<T> {
        let record = entity_ptr.record;
        let bucket = unsafe { self.entity_table.bucket(record.bucket_id) };
        let mut storage_view_mut = unsafe {
            bucket
                .as_mut()
                .view_mut::<T>(&mut self.entity_catalog, record.bucket_id)
                .ok()?
        };
        storage_view_mut.remove(record.storage_idx)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct EntityRecord {
    bucket_id: usize,
    storage_idx: usize,
}

struct EntityStorage {
    type_id: TypeId,
    type_layout: Layout,
    type_drop: fn(slab::ErasedSlab),
    type_reattach_vacant: fn(&mut slab::ErasedSlab, usize) -> Result<(), slab::EntryError>,
    type_data: slab::ErasedSlab,
}

struct EntityStorageEntry<T> {
    data: T,
    entity_id: usize,
}

impl Drop for EntityStorage {
    fn drop(&mut self) {
        (self.type_drop)(self.type_data);
    }
}

impl EntityStorage {
    fn new<T: Any>() -> Self {
        EntityStorage {
            type_id: TypeId::of::<T>(),
            type_layout: Layout::new::<T>(),
            type_drop: |erased_slab| {
                let _ = unsafe { Slab::<EntityStorageEntry<T>>::from_erased_slab(erased_slab) };
            },
            type_reattach_vacant: |erased_slab, index| {
                let mut slab = unsafe { ManuallyDrop::new(Slab::<EntityStorageEntry<T>>::from_erased_slab(*erased_slab)) };
                let result = slab.reattach_vacant(index);
                *erased_slab = ManuallyDrop::into_inner(slab).into_erased_slab();
                result
            },
            type_data: Slab::<EntityStorageEntry<T>>::new().into_erased_slab(),
        }
    }

    fn view<T: Any>(&self) -> Result<EntityStorageView<'_, T>, Error> {
        let type_id = TypeId::of::<T>();
        if self.type_id != type_id {
            return Err(Error::InvalidPtr);
        }
        debug_assert_eq!(self.type_layout, Layout::new::<T>());
        Ok(EntityStorageView {
            storage: self,
            phantom: PhantomData,
        })
    }

    fn view_mut<'a, T: Any>(
        &'a mut self,
        catalog: &'a mut Slab<EntityRecord>,
        bucket_id: usize,
    ) -> Result<EntityStorageViewMut<'a, T>, Error> {
        let type_id = TypeId::of::<T>();
        if self.type_id != type_id {
            return Err(Error::InvalidPtr);
        }
        debug_assert_eq!(self.type_layout, Layout::new::<T>());
        Ok(EntityStorageViewMut {
            storage: self,
            catalog,
            bucket_id,
            phantom: PhantomData,
        })
    }

    unsafe fn raw_slab<T: Any>(
        &self,
    ) -> Result<ManuallyDrop<Slab<EntityStorageEntry<T>>>, Error> {
        let type_id = TypeId::of::<T>();
        if self.type_id != type_id {
            return Err(Error::InvalidPtr);
        }
        debug_assert_eq!(self.type_layout, Layout::new::<T>());
        Ok(ManuallyDrop::new(Slab::from_erased_slab(
            self.type_data,
        )))
    }
    unsafe fn finish_raw_slab<T: Any>(
        &mut self,
        slab: ManuallyDrop<Slab<EntityStorageEntry<T>>>,
    ) -> Result<(), Error> {
        let type_id = TypeId::of::<T>();
        if self.type_id != type_id {
            return Err(Error::InvalidPtr);
        }
        debug_assert_eq!(self.type_layout, Layout::new::<T>());
        self.type_data = ManuallyDrop::into_inner(slab).into_erased_slab();
        Ok(())
    }

    fn untyped_reattach_vacant(&mut self, storage_idx: usize) -> Result<(), slab::EntryError> {
        (self.type_reattach_vacant)(&mut self.type_data, storage_idx)
    }
}

#[derive(Copy, Clone)]
struct EntityStorageView<'a, T: Any> {
    storage: &'a EntityStorage,
    phantom: PhantomData<&'a [T]>,
}

impl<'a, T: Any> EntityStorageView<'a, T> {
    fn is_valid_idx(&self, v: usize) -> bool {
        let slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        slab.is_occupied(v)
    }

    fn index(self, index: usize) -> &'a T {
        unsafe {
            &self
                .storage
                .type_data
                .index::<EntityStorageEntry<T>>(index)
                .data
        }
    }
}

struct EntityStorageViewMut<'a, T: Any> {
    storage: &'a mut EntityStorage,
    catalog: &'a mut Slab<EntityRecord>,
    bucket_id: usize,
    phantom: PhantomData<&'a mut [T]>,
}

impl<'a, T: Any> EntityStorageViewMut<'a, T> {
    fn is_valid_idx(&self, v: usize) -> bool {
        let slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        slab.is_occupied(v)
    }

    fn preallocate(&mut self) -> usize {
        let mut slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        let storage_idx = slab.detach_vacant();
        unsafe { self.storage.finish_raw_slab(slab).unwrap() };
        storage_idx
    }

    fn insert(&mut self, data: T) -> (usize, usize) {
        let mut slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        let entity_id = self.catalog.detach_vacant();
        let entry = EntityStorageEntry { data, entity_id };
        let storage_idx = slab.push(entry);
        self.catalog.occupy_detached_vacant(
            entity_id,
            EntityRecord {
                bucket_id: self.bucket_id,
                storage_idx,
            },
        ).unwrap();
        unsafe { self.storage.finish_raw_slab(slab).unwrap() };
        (storage_idx, entity_id)
    }

    fn is_preallocated(&self, storage_idx: usize) -> bool {
        let slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        slab.is_detached_vacant(storage_idx)
    }

    fn init_preallocated(&mut self, storage_idx: usize, data: T) -> usize {
        let mut slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        let entity_id = self.catalog.detach_vacant();
        let entry = EntityStorageEntry { data, entity_id };
        slab.occupy_detached_vacant(storage_idx, entry).unwrap();
        self.catalog.occupy_detached_vacant(
            entity_id,
            EntityRecord {
                bucket_id: self.bucket_id,
                storage_idx,
            },
        ).unwrap();
        unsafe { self.storage.finish_raw_slab(slab).unwrap() };
        entity_id
    }

    fn remove(&mut self, storage_idx: usize) -> Option<T> {
        let mut slab = unsafe { self.storage.raw_slab::<T>().unwrap() };
        let entry = slab.remove(storage_idx)?;
        self.catalog.remove(entry.entity_id);
        Some(entry.data)
    }

    #[allow(dead_code)]
    fn index(self, index: usize) -> &'a T {
        unsafe {
            &self
                .storage
                .type_data
                .index::<EntityStorageEntry<T>>(index)
                .data
        }
    }

    fn index_mut(self, index: usize) -> &'a mut T {
        unsafe {
            &mut self
                .storage
                .type_data
                .index_mut::<EntityStorageEntry<T>>(index)
                .data
        }
    }
}
/// Conversion from reference to a repository reference
pub trait AsRepoRef {
    /// Performs the conversion
    fn as_repo_ref(&self) -> &Repo;
}

/// Conversion from reference to a repository reference
pub trait AsRepoMut: AsRepoRef {
    /// Performs the conversion
    fn as_repo_mut(&mut self) -> &mut Repo;
}

impl AsRepoRef for Repo {
    fn as_repo_ref(&self) -> &Repo {
        self
    }
}

impl AsRepoMut for Repo {
    fn as_repo_mut(&mut self) -> &mut Repo {
        self
    }
}

/// An index handle to a value in repository.
pub struct EntityId<R = Repo>(usize, PhantomData<R>);

impl<R> Copy for EntityId<R> {}

impl<R> Clone for EntityId<R> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<R> PartialEq for EntityId<R> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.0, &other.0)
    }
}

impl<R> Eq for EntityId<R> {}

impl<R> PartialOrd for EntityId<R> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(&self.0, &other.0)
    }
}

impl<R> Ord for EntityId<R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(&self.0, &other.0)
    }
}

impl<R> Hash for EntityId<R> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0, state)
    }
}

impl<R> fmt::Debug for EntityId<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EntityId({})", self.0)
    }
}

impl<R> EntityId<R> {
    /// Convert entity id to work with another repository type.
    pub fn cast_repo<R2>(self) -> EntityId<R2> {
        EntityId(self.0, PhantomData)
    }
}

impl<R: AsRepoRef> EntityId<R> {
    /// Try to downcast this index handle to a pointer handle.
    pub fn cast_ptr<T: Any>(self, repo: &R) -> Option<EntityPtr<T>> {
        let repo = repo.as_repo_ref();
        let &record = repo.entity_catalog.get(self.0)?;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view = unsafe { bucket.as_ref().view::<T>().ok()? };
        debug_assert!(storage_view.is_valid_idx(record.storage_idx));
        Some(EntityPtr {
            record,
            phantom: PhantomData,
        })
    }

    /// Try to downcast this index handle to a reference to the value.
    pub fn cast_ref<T: Any>(self, repo: &R) -> Option<EntityRef<'_, T>> {
        let repo = repo.as_repo_ref();
        let &record = repo.entity_catalog.get(self.0)?;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view = unsafe { bucket.as_ref().view::<T>().ok()? };
        debug_assert!(storage_view.is_valid_idx(record.storage_idx));
        Some(EntityRef {
            repo,
            record,
            phantom: PhantomData,
        })
    }
}

impl<R: AsRepoMut> EntityId<R> {
    /// Try to downcast this index handle to a mutable reference to the value.
    pub fn cast_mut<T: Any>(self, repo: &mut R) -> Option<EntityMut<'_, T>> {
        let repo = repo.as_repo_mut();
        let &record = repo.entity_catalog.get(self.0)?;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe {
            bucket
                .as_mut()
                .view_mut::<T>(&mut repo.entity_catalog, record.bucket_id)
                .ok()?
        };
        debug_assert!(storage_view_mut.is_valid_idx(record.storage_idx));
        Some(EntityMut {
            repo,
            record,
            phantom: PhantomData,
        })
    }
}

/// A pointer handle to a value in repository.
pub struct EntityPtr<T: Any, R = Repo> {
    record: EntityRecord,
    phantom: PhantomData<(*mut T, *mut R)>,
}

impl<T: Any, R> Copy for EntityPtr<T, R> {}

impl<T: Any, R> Clone for EntityPtr<T, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Any, R> PartialEq for EntityPtr<T, R> {
    fn eq(&self, other: &Self) -> bool {
        self.record.eq(&other.record)
    }
}

impl<T: Any, R> PartialOrd for EntityPtr<T, R> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.record.partial_cmp(&other.record)
    }
}

impl<T: Any, R> Eq for EntityPtr<T, R> {}

impl<T: Any, R> Ord for EntityPtr<T, R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.record.cmp(&other.record)
    }
}

impl<T: Any, R> Hash for EntityPtr<T, R> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.record.hash(state)
    }
}

unsafe impl<T: Any, R> Send for EntityPtr<T, R> {}
unsafe impl<T: Any, R> Sync for EntityPtr<T, R> {}

impl<T: Any, R> fmt::Debug for EntityPtr<T, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "EntityPtr(#{}, #{})",
            self.record.bucket_id, self.record.storage_idx
        )
    }
}

impl<T: Any, R> EntityPtr<T, R> {
    /// Convert entity pointer to work with another repository type.
    pub fn cast_repo<R2>(self) -> EntityPtr<T, R2> {
        EntityPtr {
            record: self.record,
            phantom: PhantomData,
        }
    }
}

impl<T: Any, R: AsRepoRef> EntityPtr<T, R> {
    /// Try to retrieve a reference to the value.
    pub fn get_ref(self, repo: &R) -> Result<EntityRef<'_, T>, Error> {
        let repo = repo.as_repo_ref();
        let record = self.record;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view = unsafe { bucket.as_ref().view::<T>()? };
        debug_assert!(storage_view.is_valid_idx(record.storage_idx));
        Ok(EntityRef {
            repo,
            record,
            phantom: PhantomData,
        })
    }
}

impl<T: Any, R: AsRepoMut> EntityPtr<T, R> {
    /// Try to retrieve a mutable reference to the value.
    pub fn get_mut(self, repo: &mut R) -> Result<EntityMut<'_, T>, Error> {
        let repo = repo.as_repo_mut();
        let record = self.record;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe {
            bucket
                .as_mut()
                .view_mut::<T>(&mut repo.entity_catalog, record.bucket_id)?
        };

        debug_assert!(storage_view_mut.is_valid_idx(record.storage_idx));
        Ok(EntityMut {
            repo,
            record,
            phantom: PhantomData,
        })
    }
}

/// Error raised during `Repo` operation.
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("Invalid pointer is specified.")]
    /// input is no longer a valid pointer.
    InvalidPtr,
    #[error("Internal bucket inconsistency.")]
    /// repository data become corrupted.
    InternalError001,
}

const ERROR_INTERNAL_BUCKET_INCONSISTENCY: Error = Error::InternalError001;

/// Represents a mutable reference to a value in repository.
pub struct EntityMut<'a, T> {
    repo: &'a mut Repo,
    record: EntityRecord,
    phantom: PhantomData<&'a mut T>,
}

impl<'a, T: Any + fmt::Debug> fmt::Debug for EntityMut<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", **self)?;
        Ok(())
    }
}

impl<'a, T: Any> ops::Deref for EntityMut<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let record = self.record;
        let bucket = unsafe { self.repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe {
            bucket
                .as_ref()
                .view::<T>()
                .map_err(|_| ERROR_INTERNAL_BUCKET_INCONSISTENCY)
                .unwrap()
        };
        storage_view_mut.index(record.storage_idx)
    }
}

impl<'a, T: Any> ops::DerefMut for EntityMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let record = self.record;
        let bucket = unsafe { self.repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe {
            bucket
                .as_mut()
                .view_mut::<T>(&mut self.repo.entity_catalog, record.bucket_id)
                .map_err(|_| ERROR_INTERNAL_BUCKET_INCONSISTENCY)
                .unwrap()
        };
        storage_view_mut.index_mut(record.storage_idx)
    }
}

impl<'a, T: Any> EntityMut<'a, T> {
    /// Convert a mutable reference to a value in repository to an immutable reference.
    pub fn as_ref<'b>(&'b mut self) -> EntityRef<'b, T> {
        EntityRef {
            repo: self.repo,
            record: self.record,
            phantom: PhantomData,
        }
    }
}

/// Represents a reference to a value in repository.
pub struct EntityRef<'a, T> {
    repo: &'a Repo,
    record: EntityRecord,
    phantom: PhantomData<&'a T>,
}

impl<'a, T> Copy for EntityRef<'a, T> {}

impl<'a, T> Clone for EntityRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: Any + fmt::Debug> fmt::Debug for EntityRef<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", **self)?;
        Ok(())
    }
}

impl<'a, T: Any> ops::Deref for EntityRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let record = self.record;
        let bucket = unsafe { self.repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe {
            bucket
                .as_ref()
                .view::<T>()
                .map_err(|_| ERROR_INTERNAL_BUCKET_INCONSISTENCY)
                .unwrap()
        };
        storage_view_mut.index(record.storage_idx)
    }
}

#[test]
fn test_repo_basic() {
    fn s(v: &str) -> String {
        v.to_string()
    }

    let mut repo = Repo::new();
    let a = repo.insert_for_id(42i32);
    let a_ptr = a.cast_ptr::<i32>(&repo).unwrap();
    assert_eq!(42i32, *a_ptr.get_ref(&repo).unwrap());
    let a_wrong_ptr = a.cast_ptr::<u32>(&repo);
    assert_eq!(None, a_wrong_ptr);

    let b = repo.insert_for_id("hello");
    assert_eq!(42i32, *a_ptr.get_ref(&repo).unwrap());
    let b_ref = b.cast_ref::<&'static str>(&repo).unwrap();
    assert_eq!("hello", *b_ref);

    let c = repo.insert_for_id(s("world"));
    assert_eq!(s("world"), *c.cast_mut::<String>(&mut repo).unwrap());
    *c.cast_mut::<String>(&mut repo).unwrap() = s("World");
    assert_eq!(s("World"), *c.cast_mut::<String>(&mut repo).unwrap());
}
