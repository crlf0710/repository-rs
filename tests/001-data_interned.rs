#[repo::repo]
#[member(Name)]
#[derive(Default)]
struct Repo;

#[cfg(feature = "keyed")]
use repo::keyed as k;

#[cfg_attr(feature = "keyed", repo::interned(data = NameData, keyed = true))]
#[cfg_attr(not(feature = "keyed"), repo::interned(data = NameData))]
#[derive(Debug, PartialEq, Eq, Hash)]
struct Name(#[accessor = name] &'static str);

impl PartialEq<str> for NameData {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

#[test]
fn test_0001() {
    let mut repo = Repo::default();
    let name1 = Name::with_data(&mut repo, NameData("Alice"));
    let name2 = Name::new(&mut repo, "Bob");
    #[cfg(feature = "keyed")]
    let name3 = Name::new(&mut repo, k!(name: "Carol"));
    assert_eq!(name1.data(&repo), "Alice");
    assert_eq!(name2.name(&repo), "Bob");
    #[cfg(feature = "keyed")]
    assert_eq!(*name3.name(&repo), *"Carol");
}