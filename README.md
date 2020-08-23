## Repository: A repository for all kinds of entities.

`Repository` provides storage for multiple data types, it provides
its own kind of index handle called `EntityId` and its own kind of typed
handle called `EntityPtr<T>`. With these handles it allows the values
resident in the same `Repository` to reference each other easily.

The data behind the `EntityPtr<T>` handle can be accessed when you have
a corresponding reference to the whole repository.

The data behind the `EntityId` handle can be accessed when you know its
type and have a corresponding reference to the whole repository.

### Usage example:
```rust
// create a repository.
let mut repo = Repo::new();

// insert and retrieve a pointer handle.
let a = repo.insert(42i32);
assert_eq!(42i32, *a.get_ref(&repo).unwrap());

// insert and retrieve a index handle.
let b = repo.insert_for_id(42i32);

// downcast from index handle to pointer handle.
let b_ptr = b.cast_ptr::<i32>(&repo).unwrap();
assert_eq!(42i32, *b_ptr.get_ref(&repo).unwrap());

// downcasting fails when type is incorrect.
let b_wrong_ptr = b.cast_ptr::<u32>(&repo);
assert_eq!(None, b_wrong_ptr);
```

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>
