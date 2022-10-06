use std::cell::Cell;

#[repo::repo]
#[member(Person)]
#[derive(Default)]
struct Repo;

#[repo::entity]
struct Person {
    #[inherent]
    name: String,
    #[by_ref]
    age: IntWithoutClone,
}

struct IntWithoutClone(Cell<isize>);

impl IntWithoutClone {
    fn new(v: isize) -> Self {
        IntWithoutClone(Cell::new(v))
    }

    fn get(&self) -> isize {
        self.0.get()
    }

    fn set(&self, v: isize) {
        self.0.set(v)
    }
}

#[test]
fn test_0003() {
    let mut repo = Repo::default();

    let person = Person::new("Alice".to_string(), IntWithoutClone::new(24), &mut repo);

    assert_eq!(&person.name(&repo), "Alice");

    let age: &IntWithoutClone = person.age(&repo);

    assert_eq!(age.get(), 24);

    age.set(25);

    assert_eq!(person.age(&repo).get(), 25);
}
