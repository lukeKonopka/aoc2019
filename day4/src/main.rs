fn number_to_vec(input: u32) -> Vec<u8> {
    format!("{}", input)
        .chars()
        .map(|c| c.to_digit(10).expect(&format!("{} is not a digit", c)) as u8)
        .collect()
}

fn increasing(input: &Vec<u8>) -> bool {
    input
        .iter()
        .fold((true, 0), |(v, prev), &curr| {
            if prev <= curr {
                (v, curr)
            } else {
                (false, curr)
            }
        }).0
}

fn adjecent_digits(input: &Vec<u8>) -> bool {
    input
        .iter()
        .fold((false, 0), |(v, prev), &curr| {
            if prev == curr {
                (true, curr)
            } else {
                (v, curr)
            }
        }).0
}

fn adjecent_digits_once(input: &Vec<u8>) -> bool {
    let (adj, last) = input.iter().fold((vec![], vec![]), |(acc, adj), curr| {
        if adj.len() > 0 && adj[adj.len() - 1] == curr {
            (acc, [adj, vec![curr]].concat())
        } else {
            ([acc, vec![adj]].concat(), vec![curr])
        }
    });

    let all = [adj, vec![last]].concat();

    all.iter().any(|adj| adj.len() == 2)
}

fn part_1() -> usize {
    (146810..612564)
        .map(number_to_vec)
        .filter(|pass| increasing(pass) && adjecent_digits(pass))
        .count()
}

fn part_2() -> usize {
    (146810..612564)
        .map(number_to_vec)
        .filter(|pass| increasing(pass) && adjecent_digits_once(pass))
        .count()
}

fn main() {
    println!("Part 1: {}\nPart 2: {}", part_1(), part_2())
}

#[test]
fn tests() {
    assert_eq!(adjecent_digits_once(&vec![1, 1, 1, 1, 1, 1]), false);
    assert_eq!(adjecent_digits_once(&vec![2, 2, 3, 4, 5, 0]), true);
    assert_eq!(adjecent_digits_once(&vec![1, 2, 3, 7, 8, 9]), false);

    assert_eq!(adjecent_digits_once(&vec![1, 1, 2, 2, 3, 3]), true);
    assert_eq!(adjecent_digits_once(&vec![1, 2, 3, 4, 4, 4]), false);
    assert_eq!(adjecent_digits_once(&vec![1, 1, 1, 1, 2, 2]), true);
}
