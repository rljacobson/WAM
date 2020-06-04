/*!
  This module is responsible for the encoding and decoding of binary instructions.

*/

use super::{Operation, Instruction};
use crate::address::{AddressType, Address};
use std::convert::TryFrom;
use crate::functor::Functor;

// If you change this you must also change `encode_instruction` and `decode_instruction`.
pub type Word = u32;
pub type DoubleWord = u64;
// Convenience for decomposing a DoubleWord into a high word and a low word:
pub struct TwoWords {
  pub low: Word,
  pub high: Word
}

/// An `Either` type for an encoded instruction, allowing the instruction to be
/// either one word or two.
pub enum EncodedInstruction{
  Word(Word),
  DoubleWord(DoubleWord)
}


pub fn try_decode_instruction(bin_inst: DoubleWord) -> Option<Instruction> {
  let opcode = match Operation::try_from((bin_inst & 0xFF) as u8) {
    Ok(v) => Some(v),
    Err(_e) => None // ToDo: panic!("{}", e);
  };
  if opcode == None{
    return None
  }
  let opcode = opcode.unwrap();

  let instruction: Instruction;
  if if Into::<u8>::into(opcode) < 2 {
    // [OpCode:8][Address:24][Name:16][Arity:16]

    Instruction::BinaryFunctor {
      opcode,
      address: Address::from_reg_idx( ((bin_inst >> 8) & 0xFFFFFF) as usize),
      functor: Functor::from_word((bin_inst >> 32) as Word),
    }
  }
  else if Into::<u8>::into(opcode) < 6 {
    // [OpCode:8][Address:24][Address:24][Reserved:8]
    Instruction::Binary {
      opcode,
      address1: ((bin_inst >> 8) & 0xFFFFFF) as AddressType,
      address2: (bin_inst >> 32) as AddressType,
    }
  } else if Into::<u8>::into(opcode) < 12 {
    // [OpCode:8][Address:24]
    Instruction::Unary {
      opcode,
      address: (bin_inst >> 8) as AddressType
    }
  } else {
    // [OpCode:8]
    Instruction::Nullary(opcode)
  };

  Some(instruction)
}



/**
  Encodes the instruction into bytecode. It is the caller's responsibility to
  use the correct `InstructionArguments` variant for the given opcode.
*/
pub fn encode_instruction(instruction: Instruction) -> EncodedInstruction{
  match instruction{

    Instruction::Binary {opcode, address1, address2 } => {
      let add1 = address1.enc();
      let add2 = address2.enc();
      // [OpCode:8][Address:24][Address:24][Reserved:8]
      EncodedInstruction::DoubleWord(
        ( opcode as DoubleWord)        +
          ((add1 as DoubleWord) << 8 ) +
          ((add2 as DoubleWord) << 32)
      )
    },

    Instruction::Unary {opcode, address} => {
      let add = address.enc();
      // [OpCode:8][Address:24]
      EncodedInstruction::Word(
        (opcode as Word)        +
          ((add as Word) << 8 )
      )
    },

    Instruction::Nullary(opcode) => {
      // [OpCode:8]
      EncodedInstruction::Word(opcode as Word)
    },
  }
}


/// Returns the size in WORDS of an instruction for the corresponding opcode.
pub fn instruction_size(opcode: Operation) -> u32{
  match Into::<u8>::into(opcode) < 6 {
    true  => 2, // Two words
    false => 1 // One Word
  }
}

/**
  Given the low word of an instruction, determines whether the entire instruction is two
  words. This is how to determine if a second word needs to be provided for decoding.

  Note that this function does not check if the input has a valid opcode.
*/
pub fn is_double_word_instruction(word: Word) -> bool{
  (word & Word::max_value()) < 6
}
