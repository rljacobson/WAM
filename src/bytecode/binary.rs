/*!
  This module is responsible for the encoding and decoding of binary instructions.

*/

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use string_cache::DefaultAtom;

use crate::address::Address;
use crate::functor::{Functor, ArityType};
use super::{Operation, Instruction, Argument};
// ToDo: This dependency is frustrating. Can it be removed?
use crate::wvm::{intern_functor, try_get_interned_functor};
use super::instruction::{MAX_DOUBLE_WORD_OPCODE, MAX_UNARY_OPCODE, MAX_FUNCTOR_OPCODE};


// If you change this you must also change `encode_instruction` and `decode_instruction`.
pub type Word = u32;

/*
  Convenience for decomposing a u64 into a high word and a low word. In
  fact, the bytecode is designed so that no bit operations have to span
  a 32-bit word boundary, so this is more convenient than a u64 in general.
*/
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct DoubleWord {
  pub low  : Word,
  pub high : Word
}

/// An `Either` type for an encoded instruction, allowing the instruction to be
/// either one word or two.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Bytecode {
  Word(Word),
  DoubleWord(DoubleWord)
}

impl Display for Bytecode{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Bytecode::Word(word)         => write!(f, "0x{:0>4X}", word),
      Bytecode::DoubleWord(double) => write!(f, "0x{:0>5X}{:0>4X}", double.high, double.low)
    }

  }
}

pub fn try_decode_instruction(encoded: &Bytecode) -> Option<Instruction> {
  let mut bytecode = DoubleWord { low: 0, high: 0 };
  match &encoded {
    Bytecode::Word(w)        => { bytecode.low = *w; }
    Bytecode::DoubleWord(tw) => { bytecode = *tw; }
  }

  let opcode: Operation =
    match Operation::try_from((bytecode.low & 0xFF) as u8) {

      Ok(oc)  => oc,

      Err(_e) => { return None; }

    };

  let instruction = // = the value of the following giant if statement:
  if opcode.code() < MAX_FUNCTOR_OPCODE {
    // [OpCode:8][Address:24][Name:16][Arity:16]
    Instruction::Binary {
      opcode,
      address : Address::from_reg_idx( (bytecode.low >> 8) as usize),
      argument: Argument::Functor(decode_functor(&bytecode.high)),
    }
  }

  else if opcode.code() < MAX_DOUBLE_WORD_OPCODE {
    // [OpCode:8][Address:24][Address:24][Reserved:8]
    Instruction::Binary {
      opcode,
      address : Address::from_heap_idx((bytecode.low >> 8) as usize),
      argument: Argument::Address(Address::from_reg_idx(bytecode.high as usize))
    }
  }

  else if opcode.code() < MAX_UNARY_OPCODE {
    // [OpCode:8][Address:24]
    let argument =
      match opcode {

        Operation::Allocate => Argument::Word(bytecode.low >> 8),

        Operation::Call     => {
          Argument::Address(Address::from_code_idx((bytecode.low >> 8) as usize))
        }
        _                   => {
          Argument::Address(Address::from_reg_idx((bytecode.low >> 8) as usize))
        }

      };
    Instruction::Unary {
      opcode,
      argument
    }
  }

  else {
    // [OpCode:8]
    Instruction::Nullary(opcode)
  };

  Some(instruction)
}



/**
  Encodes the instruction into bytecode. It is the caller's responsibility to
  use the correct `Instruction` variant for the given opcode.
*/
pub fn encode_instruction(instruction: &Instruction) -> Bytecode {
  match instruction{

    Instruction::Binary {opcode, address, argument} => {
      let high = match argument {
        Argument::Address(address2) => address2.enc(),
        Argument::Functor(functor)  => encode_functor(&functor),
        Argument::Word(_)           => {
          unreachable!("Error: `Argument::Word` cannot be an argument to `Instruction::Binary`.");
        }
      };
      Bytecode::DoubleWord(
        DoubleWord {
          low: opcode.code() + (address.enc() << 8),
          high,
        }
      )
    }

    Instruction::Unary {opcode, argument} => {
      // [OpCode:8][Address:24]
      let parameter = match argument{
        Argument::Address(address) => address.enc(),
        Argument::Word(word)       => *word,
        Argument::Functor(_)       => {
          unreachable!(
            "Error: `Argument::Functor` cannot be an argument to `Instruction::Unary`."
          );
        },
      };
      Bytecode::Word( opcode.code() + (parameter << 8) )
    },

    Instruction::Nullary(opcode)   => {
      // [OpCode:8]
      Bytecode::Word(opcode.code())
    },
  }
}


/// Returns the size in WORDS of an instruction for the corresponding opcode.
#[allow(dead_code)]
pub fn instruction_size(opcode: &Operation) -> Word{
  match opcode.code() < MAX_DOUBLE_WORD_OPCODE {
    true  => 2, // Two words
    false => 1  // One Word
  }
}

/**
  Given the low word of an instruction, determines whether the entire instruction is two
  words. This is how to determine if a second word needs to be provided for decoding.

  Note that this function does not check if the input has a valid opcode.
*/
pub fn is_double_word_instruction(word: &Word) -> bool{
  (*word & 0xFF) < MAX_DOUBLE_WORD_OPCODE
}


fn decode_functor(word: &Word) -> Functor{
  let functor_address = Address::from_funct_idx((word & 0xFFFF) as usize);

  match try_get_interned_functor(&functor_address) {

    Some(functor) => functor.clone(),

    None          => {
      // ToDo: Make a more robust automatic naming scheme.
      let new_name = DefaultAtom::from(
        // ASCII 97 = 'a'.
        ((97u8 + functor_address.idx() as u8) as char).to_string()
      );
      let functor  = Functor {
        name  : new_name,
        arity : (word >> 16) as ArityType
      };
      intern_functor(&functor);
      functor
    }

  }
}

fn encode_functor(functor: &Functor) -> Word{
  let functor_address = intern_functor(functor);

  ((functor.arity as Word) << 16) + (functor_address.idx() as Word)
}
