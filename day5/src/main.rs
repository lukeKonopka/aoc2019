mod machine;
mod ops;

use crate::machine::Machine;
use std::fs;

fn part_1(mem: &Vec<isize>) -> isize {
    let mut machine = Machine::create(mem).with_input(vec![1]);
    while !machine.halted {
        machine.tick().expect("Tick cannot be performed!");
    }
    *machine
        .output_buffer
        .get(machine.output_buffer.len() - 1)
        .expect("Output was empty")
}

fn part_2(mem: &Vec<isize>) -> isize {
    let mut machine = Machine::create(mem).with_input(vec![5]);
    while !machine.halted {
        machine.tick();
    }
    *machine
        .output_buffer
        .get(machine.output_buffer.len() - 1)
        .expect("Output was empty")
}

fn main() {
    let input = fs::read_to_string("./input")
        .expect("cannot read file")
        .split(",")
        .map(|s| s.parse::<isize>().unwrap())
        .collect::<Vec<isize>>();
    println!("Part 1: {}\nPart 2: {}", part_1(&input), part_2(&input));
}
