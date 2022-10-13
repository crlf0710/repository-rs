use std::ops;

#[repo::repo]
#[member(Person)]
#[derive(Default)]
struct Repo;

#[repo::entity]
#[decorate_field(Editable for EditableTypes)]
struct Person {
    #[by_ref]
    name: String,
    #[by_ref]
    age: u32,
}

struct EditableTypes;

#[allow(non_camel_case_types)]
trait Editable {
    type String;
    type u32;
}

impl Editable for EditableTypes {
    type String = TextBoxString;
    type u32 = SpinBoxU32;
}

fn main() {
    let mut repo = Repo::default();
    let person = Person::new("Alice".to_string(), 17, &mut repo);
    person
        .name_mut(&mut repo)
        .set_proposed_value("ALICE".to_string());
    person.age_mut(&mut repo).set_proposed_value(18);
    person.submit_new_values(&mut repo).unwrap();
    assert_eq!(**person.name(&repo), "ALICE");
    assert_eq!(**person.age(&repo), 18);
}

impl Person {
    fn submit_new_values(self, repo: &mut Repo) -> Result<(), std::io::Error> {
        let name = self.name_mut(repo);
        name.value = name.proposed_value.clone();
        let age = self.age_mut(repo);
        age.value = age.proposed_value.clone();
        Ok(())
    }
}

#[derive(Debug)]
struct TextBoxString {
    value: String,
    proposed_value: String,
}

impl TextBoxString {
    fn set_proposed_value(&mut self, proposed_value: String) {
        self.proposed_value = proposed_value;
    }
}

impl ops::Deref for TextBoxString {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl From<String> for TextBoxString {
    fn from(v: String) -> Self {
        TextBoxString {
            value: v.clone(),
            proposed_value: v,
        }
    }
}

#[derive(Debug)]
struct SpinBoxU32 {
    value: u32,
    proposed_value: u32,
}

impl SpinBoxU32 {
    fn set_proposed_value(&mut self, proposed_value: u32) {
        self.proposed_value = proposed_value;
    }
}

impl ops::Deref for SpinBoxU32 {
    type Target = u32;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl From<u32> for SpinBoxU32 {
    fn from(v: u32) -> Self {
        SpinBoxU32 {
            value: v,
            proposed_value: v,
        }
    }
}
