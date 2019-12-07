use crate::ops::{Op, Parameter, ParameterMode};

pub struct Machine {
  ip: usize,
  memory: Vec<isize>,
  input_buffer: Vec<isize>,
  pub output_buffer: Vec<isize>,
  pub halted: bool,
}

impl Machine {
  pub fn create(memory: &Vec<isize>) -> Self {
    Machine {
      ip: 0,
      memory: memory.clone(),
      input_buffer: vec![],
      output_buffer: vec![],
      halted: false,
    }
  }

  pub fn with_input(mut self, input: Vec<isize>) -> Self {
    self.input_buffer = input;
    self
  }

  fn read(&self, addr: usize) -> Option<isize> {
    self.memory.get(addr).map(|x| *x)
  }

  fn write(&mut self, addr: usize, value: isize) {
    self.memory[addr] = value;
  }

  fn read_parameter(&self, param: &Parameter) -> Option<isize> {
    match param.mode {
      ParameterMode::Position => self.read(param.val as usize),
      ParameterMode::Immediate => Some(param.val),
    }
  }

  fn write_parameter(&mut self, param: &Parameter, value: isize) {
    match param.mode {
      ParameterMode::Position => self.write(param.val as usize, value),
      ParameterMode::Immediate => panic!("Cannot write to immediate mode param"),
    }
  }

  pub fn tick(&mut self) -> Option<()> {
    let op = self.fetch_op()?;
    self.ip = self.ip + op.len();
    self.execute_op(op);

    Some(())
  }

  fn execute_op(&mut self, op: Op) -> Option<()> {
    match op {
      Op::Add(a, b, res) => {
        let a_val = self.read_parameter(&a)?;
        let b_val = self.read_parameter(&b)?;
        self.write_parameter(&res, a_val + b_val);
      }
      Op::Mul(a, b, res) => {
        let a_val = self.read_parameter(&a)?;
        let b_val = self.read_parameter(&b)?;
        self.write_parameter(&res, a_val * b_val);
      }
      Op::Input(param) => {
        let input_val = self.input_buffer.pop()?;
        self.write_parameter(&param, input_val);
      }
      Op::Output(param) => {
        let output_val = self.read_parameter(&param)?;
        self.output_buffer.push(output_val);
      }
      Op::JumpNonZero(pred, dest) => {
        let pred_val = self.read_parameter(&pred)?;
        let dest_val = self.read_parameter(&dest)?;
        if pred_val != 0 {
          self.ip = dest_val as usize;
        }
      }
      Op::JumpZero(pred, dest) => {
        let pred_val = self.read_parameter(&pred)?;
        let dest_val = self.read_parameter(&dest)?;
        if pred_val == 0 {
          self.ip = dest_val as usize;
        }
      }
      Op::LessThan(a, b, res) => {
        let a_val = self.read_parameter(&a)?;
        let b_val = self.read_parameter(&b)?;
        self.write_parameter(&res, if a_val < b_val { 1 } else { 0 });
      }
      Op::Equal(a, b, res) => {
        let a_val = self.read_parameter(&a)?;
        let b_val = self.read_parameter(&b)?;
        self.write_parameter(&res, if a_val == b_val { 1 } else { 0 });
      }
      Op::Halt() => {
        self.halted = true;
      }
    }
    Some(())
  }

  fn fetch_op(&self) -> Option<Op> {
    let instruction = self.memory[self.ip];
    let raw_data: Vec<Option<isize>> = (1..4).map(|offset| self.read(self.ip + offset)).collect();
    let params = Parameter::from_data_vec(instruction, raw_data);

    let opcode = instruction % 100;

    match opcode {
      1 => Some(Op::Add(params[0]?, params[1]?, params[2]?)),
      2 => Some(Op::Mul(params[0]?, params[1]?, params[2]?)),
      3 => Some(Op::Input(params[0]?)),
      4 => Some(Op::Output(params[0]?)),
      5 => Some(Op::JumpNonZero(params[0]?, params[1]?)),
      6 => Some(Op::JumpZero(params[0]?, params[1]?)),
      7 => Some(Op::LessThan(params[0]?, params[1]?, params[2]?)),
      8 => Some(Op::Equal(params[0]?, params[1]?, params[2]?)),
      99 => Some(Op::Halt()),
      _ => None,
    }
  }
}
