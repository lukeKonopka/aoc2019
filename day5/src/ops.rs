#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ParameterMode {
  Position,
  Immediate,
}

impl ParameterMode {
  pub fn from_value(value: isize) -> Option<ParameterMode> {
    match value {
      0 => Some(ParameterMode::Position),
      1 => Some(ParameterMode::Immediate),
      _ => None,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Parameter {
  pub val: isize,
  pub mode: ParameterMode,
}

fn indexed_data_to_param(
  instruction: isize,
  (idx, data): (usize, Option<isize>),
) -> Option<Parameter> {
  let mode = (instruction / (100 * (10 as isize).pow(idx as u32))) % 10;
  Some(Parameter {
    val: data?,
    mode: ParameterMode::from_value(mode)?,
  })
}

impl Parameter {
  pub fn from_data_vec(instruction: isize, raw_data: Vec<Option<isize>>) -> Vec<Option<Self>> {
    raw_data
      .into_iter()
      .enumerate()
      .map(|v| indexed_data_to_param(instruction, v))
      .collect::<Vec<Option<Self>>>()
  }
}

#[derive(PartialEq, Debug)]
pub enum Op {
  // math operations
  Add(Parameter, Parameter, Parameter),
  Mul(Parameter, Parameter, Parameter),

  // logic operations
  LessThan(Parameter, Parameter, Parameter),
  Equal(Parameter, Parameter, Parameter),

  // i/o
  Input(Parameter),
  Output(Parameter),

  // conditionals
  JumpNonZero(Parameter, Parameter),
  JumpZero(Parameter, Parameter),

  Halt(),
}

impl Op {
  pub fn len(&self) -> usize {
    match self {
      Op::Add(_, _, _) => 4,
      Op::Mul(_, _, _) => 4,
      Op::LessThan(_, _, _) => 4,
      Op::Equal(_, _, _) => 4,
      Op::Input(_) => 2,
      Op::Output(_) => 2,
      Op::JumpNonZero(_, _) => 3,
      Op::JumpZero(_, _) => 3,
      Op::Halt() => 1,
    }
  }
}
