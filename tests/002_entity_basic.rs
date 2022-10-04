#![allow(dead_code)]

#[repo::repo]
#[member(Color, Fruit)]
#[derive(Default)]
struct Repo;

#[repo::interned]
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
enum Color {
    Red,
    Yellow,
}

#[repo::entity]
enum Fruit {
    Apple {
        color: Color,
        #[accessor = radius]
        rad: usize,
    },
    Banana(#[accessor = length] usize),
}

#[test]
fn test_0002() {
    let mut repo = Repo::default();

    let fruit1 = Fruit::new_apple(Color::new_red(&mut repo), 16, &mut repo);
    let fruit2 = Fruit::new_banana(18, &mut repo);

    assert!(fruit2.length(&repo).unwrap() > fruit1.radius(&repo).unwrap());

    fruit2.set_length(15, &mut repo).unwrap();

    assert!(fruit2.length(&repo).unwrap() < fruit1.radius(&repo).unwrap());
}
