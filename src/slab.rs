//! Provides storage for a single data type with vacant entry support.

use core::mem;

/// Provides storage for a single data type with vacant entry support.
#[derive(Debug)]
pub(crate) struct Slab<T> {
    vacant: usize,
    items: Vec<Entry<T>>,
}

/// An entry in a `Slab`.
#[derive(Debug)]
enum Entry<T> {
    Occupied(T),
    Vacant(usize),
}

impl<T> Entry<T> {
    #[inline]
    fn is_occupied(&self) -> bool {
        matches!(self, Entry::Occupied(_))
    }
}

#[derive(Copy, Clone)]
/// Type erased storage of a `Slab`.
pub(crate) struct ErasedSlab {
    data_vacant: usize,
    data_ptr: usize,
    data_len: usize,
    data_capacity: usize,
}

/// convert a `Vec` into its raw parts
fn vec_into_raw_parts<T>(me: Vec<T>) -> (*mut T, usize, usize) {
    use std::mem::ManuallyDrop;
    let mut me = ManuallyDrop::new(me);
    (me.as_mut_ptr(), me.len(), me.capacity())
}

impl<T> Slab<T> {
    pub(crate) fn new() -> Self {
        Self::with_capacity(0)
    }

    pub(crate) fn with_capacity(capacity: usize) -> Self {
        Slab {
            vacant: capacity,
            items: Vec::with_capacity(capacity),
        }
    }

    pub(crate) fn into_erased_slab(self) -> ErasedSlab {
        let (ptr, len, capacity) = vec_into_raw_parts(self.items);
        ErasedSlab {
            data_vacant: self.vacant,
            data_ptr: ptr as usize,
            data_len: len,
            data_capacity: capacity,
        }
    }

    pub(crate) unsafe fn from_erased_slab(erased_slab: ErasedSlab) -> Self {
        Slab {
            vacant: erased_slab.data_vacant,
            items: Vec::from_raw_parts(
                erased_slab.data_ptr as *mut Entry<T>,
                erased_slab.data_len,
                erased_slab.data_capacity,
            ),
        }
    }

    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        match self.items.get(index) {
            Some(Entry::Occupied(v)) => Some(v),
            _ => None,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        match self.items.get_mut(index) {
            Some(Entry::Occupied(v)) => Some(v),
            _ => None,
        }
    }

    pub(crate) fn is_occupied(&self, index: usize) -> bool {
        matches!(self.items.get(index), Some(Entry::Occupied(_)))
    }

    pub(crate) fn is_detached_vacant(&self, index: usize) -> bool {
        matches!(self.items.get(index), Some(Entry::Vacant(usize::MAX)))
    }

    pub(crate) fn remove(&mut self, index: usize) -> Option<T> {
        match self.items.get_mut(index) {
            Some(e @ Entry::Occupied(_)) => {
                let mut entry = Entry::Vacant(self.vacant);
                mem::swap(e, &mut entry);
                self.vacant = index;
                match entry {
                    Entry::Occupied(v) => Some(v),
                    _ => unreachable!(),
                }
            }
            _ => None,
        }
    }

    pub(crate) fn push(&mut self, value: T) -> usize {
        let index = self.vacant;
        if let Some(e) = self.items.get_mut(self.vacant) {
            let mut entry = Entry::Occupied(value);
            mem::swap(e, &mut entry);
            match entry {
                Entry::Vacant(v) => self.vacant = v,
                Entry::Occupied(_) => unreachable!(),
            };
            index
        } else {
            debug_assert_eq!(self.vacant, self.items.len());
            let entry = Entry::Occupied(value);
            self.items.push(entry);
            self.vacant += 1;
            index
        }
    }

    pub(crate) fn detach_vacant(&mut self) -> usize {
        let index = self.vacant;
        if let Some(e) = self.items.get_mut(self.vacant) {
            let mut entry = Entry::Vacant(usize::MAX);
            mem::swap(e, &mut entry);
            match entry {
                Entry::Vacant(v) => self.vacant = v,
                Entry::Occupied(_) => unreachable!(),
            };
            index
        } else {
            debug_assert_eq!(self.vacant, self.items.len());
            let entry = Entry::Vacant(usize::MAX);
            self.items.push(entry);
            self.vacant += 1;
            index
        }
    }

    pub(crate) fn occupy_detached_vacant(
        &mut self,
        index: usize,
        value: T,
    ) -> Result<(), EntryError> {
        let e = match self.items.get_mut(index) {
            Some(e) => e,
            None => return Err(EntryError::InvalidIdx),
        };
        if !matches!(e, Entry::Vacant(usize::MAX)) {
            if e.is_occupied() {
                return Err(EntryError::EntryIsOccupied);
            } else {
                return Err(EntryError::EntryIsVacant);
            }
        }
        *e = Entry::Occupied(value);
        Ok(())
    }

    pub(crate) fn reattach_vacant(&mut self, index: usize) -> Result<(), EntryError> {
        let e = match self.items.get_mut(index) {
            Some(e) => e,
            None => return Err(EntryError::InvalidIdx),
        };
        if !matches!(e, Entry::Vacant(usize::MAX)) {
            if e.is_occupied() {
                return Err(EntryError::EntryIsOccupied);
            } else {
                return Err(EntryError::EntryIsVacant);
            }
        }
        *e = Entry::Vacant(self.vacant);
        self.vacant = index;
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) enum EntryError {
    InvalidIdx,
    EntryIsOccupied,
    EntryIsVacant,
    #[allow(dead_code)]
    EntryIsDetachedVacant,
}

impl ErasedSlab {
    #[allow(unused_unsafe)]
    pub(crate) unsafe fn index<'a, T>(self, index: usize) -> &'a T {
        use core::slice;
        let slice =
            unsafe { slice::from_raw_parts(self.data_ptr as *const Entry<T>, self.data_len) };
        match &slice[index] {
            Entry::Occupied(v) => v,
            _ => unreachable!(),
        }
    }

    #[allow(unused_unsafe)]
    pub(crate) unsafe fn index_mut<'a, T>(self, index: usize) -> &'a mut T {
        use core::slice;
        let slice =
            unsafe { slice::from_raw_parts_mut(self.data_ptr as *mut Entry<T>, self.data_len) };
        match &mut slice[index] {
            Entry::Occupied(v) => v,
            _ => unreachable!(),
        }
    }
}
