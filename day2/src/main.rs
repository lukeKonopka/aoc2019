mod machine;

use crate::machine::{run, State};
use std::fs;

fn part_1(input: &Vec<usize>) -> Option<usize> {
    let mut input = input.clone();

    // apply patches
    input[1] = 12;
    input[2] = 2;

    let initial_state: State = input.into();
    let final_state = run(initial_state)?;
    Some(final_state.memory[0])
}

fn part_2(input: &Vec<usize>) -> Option<usize> {
    for noun in 0..100 {
        for verb in 0..100 {
            let mut patched_input = input.clone();

            // apply patches
            patched_input[1] = noun;
            patched_input[2] = verb;

            let initial_state: State = patched_input.into();
            let final_state = run(initial_state)?;
            if final_state.memory[0] == 19690720 {
                return Some(100 * noun + verb);
            }
        }
    }

    None
}

fn main() {
    let input_string = fs::read_to_string("input").expect("Error reading input");
    let input = input_string
        .split(",")
        .map(|v| v.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();

    println!(
        "Part 1: {}\nPart 2: {}",
        part_1(&input).unwrap(),
        part_2(&input).unwrap()
    );
}

#[test]
fn test_01() {
    let initial = State {
        memory: vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50],
        pointer: 0,
    };
    let final_state = run(initial);
    assert_eq!(final_state.unwrap().memory[0], 3500);
}

#[test]
fn test_02() {
    let initial = State {
        memory: vec![1, 1, 1, 4, 99, 5, 6, 0, 99],
        pointer: 0,
    };
    let final_state = run(initial);
    assert_eq!(final_state.unwrap().memory[0], 30);
}
