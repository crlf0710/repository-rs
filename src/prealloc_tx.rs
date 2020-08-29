//! Transaction for `Repo` that supports preallocation.
//!
//! With `PreallocTx`, `EntityPtr`s can be preallocated before the actual data
//! is ready to be inserted into the `Repo`.
//!
//! Preallocated `EntityPtr`s are created using `PreallocTx::preallocate` and initialized
//! with `PreallocTx::init_preallocation`. If the `EntityPtr` is not initialized, the
//! preallocation will be cancelled when `PreallocTx` is dropped, as if
//! `PreallocTx::cancel_preallocation` is called.
//!
//! This is especially useful for building data structures with circular references
//! but without the ability to represent the intermediate invalid state during construction.

use crate::{EntityPtr, Repo};
use alloc::collections::BTreeSet;
use core::any::Any;
use core::fmt;
use thiserror::Error;

/// Transaction for `Repo` that supports preallocation.
///
/// See the [module documentation] for more details.
///
/// [module documentation]: index.html
pub struct PreallocTx<'a> {
    repo: &'a mut Repo,
    preallocations: BTreeSet<crate::EntityRecord>,
}

impl<'a> fmt::Debug for PreallocTx<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PreAllocTx {{..}}")
    }
}

#[derive(Error)]
/// Errors for `PreallocTxError` operations.
pub enum PreallocTxError<T: Any, R = Repo> {
    #[error("invalid preallocation entity pointer")]
    /// Preallocation entity pointer is invalid.
    InvalidPreallocPtr(EntityPtr<T, R>),
    #[error("invalid preallocation entity pointer with value")]
    /// A value is passed when preallocation entity pointer is invalid.
    InvalidPreallocPtrWithValue(EntityPtr<T, R>, T),
    #[error("preallocation is already occupied")]
    /// Preallocation entity pointer is already occupied by another value.
    PreallocOccupied(EntityPtr<T, R>),
    #[error("internal error")]
    /// `Repo` internal error is generated during operation.
    RepoError(#[from] crate::Error),
}

impl<T: Any, R> fmt::Debug for PreallocTxError<T, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PreallocTxError::InvalidPreallocPtr(p) => write!(f, "InvalidPreallocPtr({:?})", p),
            PreallocTxError::InvalidPreallocPtrWithValue(p, _) => {
                write!(f, "InvalidPreallocPtr({:?}, <value>)", p)
            }
            PreallocTxError::PreallocOccupied(p) => write!(f, "PreallocOccupied({:?})", p),
            PreallocTxError::RepoError(e) => write!(f, "RepoError({:?})", e),
        }
    }
}

impl<'a> PreallocTx<'a> {
    pub(crate) fn new(repo: &'a mut Repo) -> Self {
        PreallocTx {
            repo,
            preallocations: Default::default(),
        }
    }

    /// Retrieves a reference to the underlying `Repo`.
    pub fn repo(&self) -> &Repo {
        self.repo
    }

    /// Retrieves a mutable reference to the underlying `Repo`.
    pub fn repo_mut(&mut self) -> &mut Repo {
        self.repo
    }

    /// Preallocates an `EntityPtr` with type `T`.
    pub fn preallocate<T: Any>(&mut self) -> EntityPtr<T> {
        let v = self.repo.preallocate_entity::<T>();
        assert!(
            !self.preallocations.contains(&v.record),
            "unusable preallocation retrieved"
        );
        self.preallocations.insert(v.record);
        v
    }

    /// Initializes a preallocated `EntityPtr` with value.
    pub fn init_preallocation<T: Any, R>(
        &mut self,
        ptr: EntityPtr<T, R>,
        value: T,
    ) -> Result<(), PreallocTxError<T, R>> {
        if let Some(_) = self.preallocations.take(&ptr.record) {
            if !self.repo.validate_record_type::<T>(ptr.record) {
                return Err(PreallocTxError::InvalidPreallocPtr(ptr));
            }
            if let Err((_, v)) = self.repo.init_preallocate_entity(ptr.record, value) {
                return Err(PreallocTxError::InvalidPreallocPtrWithValue(ptr, v));
            }
            Ok(())
        } else {
            Err(PreallocTxError::InvalidPreallocPtrWithValue(ptr, value))
        }
    }

    /// Cancel a preallocated `EntityPtr` and make it available for further use.
    ///
    /// Since all unused preallocated entity pointers are automatically recycled when `PreallocTx`
    /// is dropped, so this is seldom necessary.
    pub fn cancel_preallocation<T: Any, R>(
        &mut self,
        ptr: EntityPtr<T, R>,
    ) -> Result<(), PreallocTxError<T, R>> {
        if self.preallocations.contains(&ptr.record) {
            if !self.repo.validate_record_type::<T>(ptr.record) {
                return Err(PreallocTxError::InvalidPreallocPtr(ptr));
            }
            if !self.repo.cancel_preallocate_entity(ptr.record)? {
                return Err(PreallocTxError::PreallocOccupied(ptr));
            }
            self.preallocations.remove(&ptr.record);
            Ok(())
        } else {
            Err(PreallocTxError::InvalidPreallocPtr(ptr))
        }
    }
}

impl<'a> Drop for PreallocTx<'a> {
    fn drop(&mut self) {
        use core::mem;
        let mut preallocations = Default::default();
        mem::swap(&mut preallocations, &mut self.preallocations);
        for record in preallocations {
            let _ = self.repo.cancel_preallocate_entity(record);
        }
    }
}

impl Repo {
    /// Executes a transaction with preallocation support
    ///
    /// See the documentation of `PreallocTx` for more information.
    pub fn transaction_preallocate<TxFn, T, E>(&mut self, transaction: TxFn) -> Result<T, E>
    where
        TxFn: FnOnce(&mut PreallocTx<'_>) -> Result<T, E>,
    {
        let mut prealloc_tx = PreallocTx::new(self);
        (transaction)(&mut prealloc_tx)
    }
}
