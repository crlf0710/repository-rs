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

extern crate alloc;

use alloc::alloc::Layout;
use alloc::rc::Rc;
use core::any::{Any, TypeId};
use core::fmt;
use core::marker::PhantomData;
use core::ops;
use hashbrown::raw::{self as hashbrown_raw, RawTable};
use std::hash::Hash;
use thiserror::Error;
use vec_map::VecMap;

pub struct Repo {
    next_id: usize,
    index: VecMap<EntityRecord>,
    entity_table: RawTable<EntityStorage>,
    marker: PhantomData<Rc<()>>,
}

fn type_id_to_u64(v: TypeId) -> u64 {
    use std::mem::transmute;
    unsafe { transmute(v) }
}

fn vec_into_raw_parts<T>(me: Vec<T>) -> (*mut T, usize, usize) {
    use std::mem::ManuallyDrop;
    let mut me = ManuallyDrop::new(me);
    (me.as_mut_ptr(), me.len(), me.capacity())
}

impl Repo {
    pub fn new() -> Self {
        Repo {
            next_id: 0,
            index: VecMap::new(),
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

    fn allocate_id(&mut self) -> usize {
        let new_id = self.next_id;
        self.next_id = self.next_id.checked_add(1).unwrap();
        new_id
    }

    fn allocate_entity(&mut self, rec: EntityRecord) -> EntityId {
        let new_id = self.allocate_id();
        self.index.insert(new_id, rec);
        EntityId(new_id)
    }

    fn create_entity<T: Any>(&mut self, v: T) -> EntityId {
        let bucket = self.ensure_type_storage_bucket::<T>();
        let bucket_id = unsafe { self.entity_table.bucket_index(&bucket) };
        let storage_mut = unsafe { bucket.as_mut() };
        let storage_idx = storage_mut.view_mut::<T>().unwrap().insert(v);
        let record = EntityRecord {
            bucket_id,
            storage_idx,
        };
        self.allocate_entity(record)
    }

    pub fn insert<T: Any>(&mut self, v: T) -> EntityPtr<T> {
        let entity_id = self.create_entity(v);
        let entity_ptr = entity_id
            .cast_ptr(self)
            .ok_or(Error::InternalError001)
            .unwrap();
        entity_ptr
    }

    pub fn insert_for_id<T: Any>(&mut self, v: T) -> EntityId {
        self.create_entity(v)
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
    type_drop: fn(usize, usize, usize),
    data_ptr: usize,
    data_len: usize,
    data_capacity: usize,
}

impl Drop for EntityStorage {
    fn drop(&mut self) {
        (self.type_drop)(self.data_ptr, self.data_len, self.data_capacity);
    }
}

impl EntityStorage {
    fn new<T: Any>() -> Self {
        use std::ptr::null_mut;
        EntityStorage {
            type_id: TypeId::of::<T>(),
            type_layout: Layout::new::<T>(),
            type_drop: |data_ptr, data_len, data_capacity| {
                let _ = unsafe { Vec::from_raw_parts(data_ptr as *mut T, data_len, data_capacity) };
            },
            data_ptr: null_mut::<T>() as usize,
            data_len: 0,
            data_capacity: 0,
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

    fn view_mut<T: Any>(&mut self) -> Result<EntityStorageViewMut<'_, T>, Error> {
        let type_id = TypeId::of::<T>();
        if self.type_id != type_id {
            return Err(Error::InvalidPtr);
        }
        debug_assert_eq!(self.type_layout, Layout::new::<T>());
        Ok(EntityStorageViewMut {
            storage: self,
            phantom: PhantomData,
        })
    }
}

#[derive(Copy, Clone)]
struct EntityStorageView<'a, T: Any> {
    storage: &'a EntityStorage,
    phantom: PhantomData<&'a [T]>,
}

impl<'a, T: Any> EntityStorageView<'a, T> {
    fn is_valid_idx(&self, v: usize) -> bool {
        v < self.storage.data_len
    }

    fn index(&self, index: usize) -> &'a T {
        use core::slice;
        let slice = unsafe {
            slice::from_raw_parts(self.storage.data_ptr as *const T, self.storage.data_len)
        };
        &slice[index]
    }
}

struct EntityStorageViewMut<'a, T: Any> {
    storage: &'a mut EntityStorage,
    phantom: PhantomData<&'a mut [T]>,
}

impl<'a, T: Any> EntityStorageViewMut<'a, T> {
    #[allow(dead_code)]
    fn is_valid_idx(&self, v: usize) -> bool {
        v < self.storage.data_len
    }

    fn insert(&mut self, v: T) -> usize {
        use std::mem::ManuallyDrop;
        let mut vec = ManuallyDrop::new(unsafe {
            Vec::from_raw_parts(
                self.storage.data_ptr as *mut T,
                self.storage.data_len,
                self.storage.data_capacity,
            )
        });
        let new_id = vec.len();
        vec.push(v);
        let (data_ptr, data_len, data_capacity) = vec_into_raw_parts(ManuallyDrop::into_inner(vec));
        self.storage.data_ptr = data_ptr as usize;
        self.storage.data_len = data_len;
        self.storage.data_capacity = data_capacity;
        new_id
    }

    fn index_mut(&mut self, index: usize) -> &'a mut T {
        use core::slice;
        let slice = unsafe {
            slice::from_raw_parts_mut(self.storage.data_ptr as *mut T, self.storage.data_len)
        };
        &mut slice[index]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntityId(usize);

impl fmt::Debug for EntityId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EntityId({})", self.0)
    }
}

impl EntityId {
    pub fn cast_ptr<T: Any>(self, repo: &Repo) -> Option<EntityPtr<T>> {
        let &record = repo.index.get(self.0)?;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view = unsafe { bucket.as_ref().view::<T>().ok()? };
        debug_assert!(storage_view.is_valid_idx(record.storage_idx));
        Some(EntityPtr {
            record,
            phantom: PhantomData,
        })
    }

    pub fn cast_ref<T: Any>(self, repo: &Repo) -> Option<EntityRef<'_, T>> {
        let &record = repo.index.get(self.0)?;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view = unsafe { bucket.as_ref().view::<T>().ok()? };
        debug_assert!(storage_view.is_valid_idx(record.storage_idx));
        Some(EntityRef {
            repo,
            record,
            phantom: PhantomData,
        })
    }

    pub fn cast_mut<T: Any>(self, repo: &mut Repo) -> Option<EntityMut<'_, T>> {
        let &record = repo.index.get(self.0)?;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe { bucket.as_mut().view_mut::<T>().ok()? };
        debug_assert!(storage_view_mut.is_valid_idx(record.storage_idx));
        Some(EntityMut {
            repo,
            record,
            phantom: PhantomData,
        })
    }
}

pub struct EntityPtr<T: Any> {
    record: EntityRecord,
    phantom: PhantomData<*mut T>,
}

impl<T: Any> Copy for EntityPtr<T> {}

impl<T: Any> Clone for EntityPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Any> PartialEq for EntityPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.record.eq(&other.record)
    }
}

impl<T: Any> PartialOrd for EntityPtr<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.record.partial_cmp(&other.record)
    }
}

impl<T: Any> Eq for EntityPtr<T> {}

impl<T: Any> Ord for EntityPtr<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.record.cmp(&other.record)
    }
}

impl<T: Any> Hash for EntityPtr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.record.hash(state)
    }
}

unsafe impl<T: Any> Send for EntityPtr<T> {}
unsafe impl<T: Any> Sync for EntityPtr<T> {}

impl<T: Any> fmt::Debug for EntityPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "EntityPtr(#{}, #{})",
            self.record.bucket_id, self.record.storage_idx
        )
    }
}

impl<T: Any> EntityPtr<T> {
    pub fn get_ref(self, repo: &Repo) -> Result<EntityRef<'_, T>, Error> {
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

    pub fn get_mut(self, repo: &mut Repo) -> Result<EntityMut<'_, T>, Error> {
        let record = self.record;
        let bucket = unsafe { repo.entity_table.bucket(record.bucket_id) };
        let storage_view_mut = unsafe { bucket.as_mut().view_mut::<T>()? };
        debug_assert!(storage_view_mut.is_valid_idx(record.storage_idx));
        Ok(EntityMut {
            repo,
            record,
            phantom: PhantomData,
        })
    }
}

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("Invalid pointer is specified.")]
    InvalidPtr,
    #[error("Internal bucket inconsistency.")]
    InternalError001,
}

const ERROR_INTERNAL_BUCKET_INCONSISTENCY: Error = Error::InternalError001;

pub struct EntityMut<'a, T> {
    repo: &'a mut Repo,
    record: EntityRecord,
    phantom: PhantomData<&'a mut T>,
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
        let mut storage_view_mut = unsafe {
            bucket
                .as_mut()
                .view_mut::<T>()
                .map_err(|_| ERROR_INTERNAL_BUCKET_INCONSISTENCY)
                .unwrap()
        };
        storage_view_mut.index_mut(record.storage_idx)
    }
}

impl<'a, T: Any> EntityMut<'a, T> {
    pub fn as_ref<'b>(&'b mut self) -> EntityRef<'b, T> {
        EntityRef {
            repo: self.repo,
            record: self.record,
            phantom: PhantomData,
        }
    }
}

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
