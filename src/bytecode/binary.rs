/*!
  This module is responsible for the encoding and decoding of binary instructions.

*/

use std::convert::TryFrom;

use string_cache::DefaultAtom;

use crate::address::Address;
use crate::functor::{Functor, ArityType};
use super::{Operation, Instruction};
// ToDo: This dependency is frustrating. Can it be removed?
use crate::wvm::{intern_functor, try_get_interned_functor};
use super::instruction::{MAX_DOUBLE_WORD_OPCODE, MAX_BINARY_OPCODE, MAX_FUNCTOR_OPCODE};

// If you change this you must also change `encode_instruction` and `decode_instruction`.
pub type Word = u32;

/*
  Convenience for decomposing a DoubleWord into a high word and a low word. In fact, the
  bytecode is designed so that no bit operations have to span a word boundary, so this is more
  convenient that `DoubleWord` in general.
*/
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct TwoWords {
  pub low: Word,
  pub high: Word
}

/// An `Either` type for an encoded instruction, allowing the instruction to be
/// either one word or two.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Bytecode {
  Word(Word),
  DoubleWord(TwoWords)
}


pub fn try_decode_instruction(encoded: &Bytecode) -> Option<Instruction> {
  let mut bytecode = TwoWords{ low: 0, high: 0 };
  match &encoded {
    Bytecode::Word(w)        => { bytecode.low = *w; }
    Bytecode::DoubleWord(tw) => { bytecode = *tw; }
  }

  let opcode =
    match Operation::try_from((bytecode.low & 0xFF) as u8) {

      Ok(v)   => Some(v),

      Err(_e) => None // ToDo: panic!("{}", e);

    };
  if opcode == None{
    return None;
  }

  let opcode = opcode.unwrap();
  let opcode_value = Into::<u8>::into(opcode);

  let instruction =
    // = the value of the following giant if statement:
  if opcode_value < MAX_FUNCTOR_OPCODE {
    // [OpCode:8][Address:24][Name:16][Arity:16]
    Instruction::BinaryFunctor {
      opcode,
      address: Address::from_reg_idx( (bytecode.low >> 8) as usize),
      functor: decode_functor(&bytecode.high),
    }
  }

  else if opcode_value < MAX_DOUBLE_WORD_OPCODE {
    // [OpCode:8][Address:24][Address:24][Reserved:8]
    Instruction::Binary {
      opcode,
      address1: Address::from_heap_idx((bytecode.low >> 8) as usize),
      address2: Address::from_reg_idx(bytecode.high as usize)
    }
  }

  else if opcode_value < MAX_BINARY_OPCODE {
    // [OpCode:8][Address:24]
    let address =
      match opcode {
        Operation::Call => Address::from_code_idx((bytecode.low >> 8) as usize),
        _ => Address::from_reg_idx((bytecode.low >> 8) as usize)
      };
    Instruction::Unary {
      opcode,
      address
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

    Instruction::BinaryFunctor {opcode, address, functor} => {
      Bytecode::DoubleWord(
        TwoWords {
          low: (Into::<u8>::into(*opcode) as Word) + (address.enc() << 8),
          high: encode_functor(&functor),
        }
      )
    }

    Instruction::Binary {opcode, address1, address2 } => {
      let add1 = address1.enc();
      let add2 = address2.enc();
      // [OpCode:8][Address:24][Address:24][Reserved:8]
      Bytecode::DoubleWord(
        TwoWords {
          low: (Into::<u8>::into(*opcode) as Word) + (add1 << 8),
          high: add2,
        }
      )
    },

    Instruction::Unary {opcode, address} => {
      let add = address.enc() as Word;
      // [OpCode:8][Address:24]
      Bytecode::Word(
        (Into::<u8>::into(*opcode) as Word) + (add << 8 )
      )
    },

    Instruction::Nullary(opcode) => {
      // [OpCode:8]
      Bytecode::Word(Into::<u8>::into(*opcode) as Word)
    },
  }
}


/// Returns the size in WORDS of an instruction for the corresponding opcode.
#[allow(dead_code)]
pub fn instruction_size(opcode: &Operation) -> Word{
  match Into::<u8>::into(*opcode) < MAX_DOUBLE_WORD_OPCODE {
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
  (*word as u8) < MAX_DOUBLE_WORD_OPCODE
}


fn decode_functor(word: &Word) -> Functor{
  let functor_address = Address::from_funct_idx((word & 0xFFFF) as usize);

  match try_get_interned_functor(&functor_address) {

    Some(functor) => functor.clone(),

    None => {
      // ToDo: Make a more robust automatic naming scheme.
      // ASCII 97 = 'a'.
      let new_name = DefaultAtom::from(
        ((97u8 + functor_address.idx() as u8) as char).to_string()
      );
      let functor = Functor {
        name: new_name,
        arity: (word >> 16) as ArityType
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
