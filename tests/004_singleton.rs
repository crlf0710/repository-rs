#[repo::repo]
#[member(Everything, Answer)]
#[derive(Default)]
struct Repo;

#[repo::entity]
enum Everything {
    #[singleton_init(Answer::singleton($repo))]
    HasAnswer {
        answer: Answer,
    },
    HasNoAnswer,
}

#[repo::entity]
#[singleton_init(42)]
struct Answer {
    value: isize,
}

#[test]
fn test_0004() {
    let mut repo = Repo::default();

    let everything = Everything::singleton(&mut repo);

    assert_eq!(42, everything.answer(&mut repo).unwrap().value(&mut repo));
}
