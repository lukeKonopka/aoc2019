#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Command {
  Add {
    loc_a: usize,
    loc_b: usize,
    loc_result: usize,
  },
  Mul {
    loc_a: usize,
    loc_b: usize,
    loc_result: usize,
  },
  Halt,
}

#[derive(Clone, Debug)]
pub struct State {
  pub pointer: usize,
  pub memory: Vec<usize>,
}

impl From<Vec<usize>> for State {
  fn from(mem: Vec<usize>) -> Self {
    State {
      pointer: 0,
      memory: mem,
    }
  }
}

fn fetch_command(current_state: &State) -> Option<Command> {
  let opcode = current_state.memory[current_state.pointer];
  let args = [1, 2, 3]
    .iter()
    .map(|offset| {
      *current_state
        .memory
        .get(current_state.pointer + offset)
        .unwrap_or(&0)
    }).collect::<Vec<usize>>();

  match opcode {
    1 => Some(Command::Add {
      loc_a: args[0],
      loc_b: args[1],
      loc_result: args[2],
    }),
    2 => Some(Command::Mul {
      loc_a: args[0],
      loc_b: args[1],
      loc_result: args[2],
    }),
    99 => Some(Command::Halt),
    _ => None,
  }
}

fn execute_command(command: &Command, state: &mut State) {
  match command {
    &Command::Add {
      loc_a,
      loc_b,
      loc_result,
    } => {
      let a = state.memory[loc_a];
      let b = state.memory[loc_b];
      state.memory[loc_result] = a + b;
    }
    &Command::Mul {
      loc_a,
      loc_b,
      loc_result,
    } => {
      let a = state.memory[loc_a];
      let b = state.memory[loc_b];
      state.memory[loc_result] = a * b;
    }
    &Command::Halt => {}
  };
}

pub fn run(initial_state: State) -> Option<State> {
  let mut current_state = initial_state.clone();
  let mut current_command = fetch_command(&current_state)?;
  while current_command != Command::Halt {
    execute_command(&current_command, &mut current_state);
    current_state.pointer = current_state.pointer + 4;
    current_command = fetch_command(&current_state)?;
  }

  Some(current_state)
}
