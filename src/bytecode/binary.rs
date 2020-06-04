/*!
  This module is responsible for the encoding and decoding of binary instructions.

*/

use std::convert::TryFrom;

use string_cache::DefaultAtom;

use crate::address::Address;
use crate::functor::{Functor, ArityType};
use super::{Operation, Instruction};
use crate::wvm::SYMBOLS;
use super::instruction::{MAX_DOUBLE_WORD_OPCODE, MAX_BINARY_OPCODE, MAX_FUNCTOR_OPCODE};
// ToDo: This dependency is frustrating.

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
    Ok(v)   => Some(v),
    Err(_e) => None // ToDo: panic!("{}", e);
  };
  if opcode == None{
    return None
  }

  let opcode = opcode.unwrap();
  let opcode_value = Into::<u8>::into(opcode);

  let instruction = // the value of the following if statement
  if opcode_value < MAX_FUNCTOR_OPCODE {
    // [OpCode:8][Address:24][Name:16][Arity:16]
    Instruction::BinaryFunctor {
      opcode,
      address: Address::from_reg_idx( ((bin_inst >> 8) & 0xFFFFFF) as usize),
      functor: decode_functor((bin_inst >> 32) as Word),
    }
  }

  else if opcode_value < MAX_DOUBLE_WORD_OPCODE {
    // [OpCode:8][Address:24][Address:24][Reserved:8]
    Instruction::Binary {
      opcode,
      address1: Address::from_heap_idx(((bin_inst >> 8) & 0xFFFFFF) as usize),
      address2: Address::from_reg_idx((bin_inst >> 32) as usize)
    }
  }

  else if opcode_value < MAX_BINARY_OPCODE {
    // [OpCode:8][Address:24]
    Instruction::Unary {
      opcode,
      address: Address::from_reg_idx((bin_inst >> 8) as usize)
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
  use the correct `InstructionArguments` variant for the given opcode.
*/
pub fn encode_instruction(instruction: &Instruction) -> EncodedInstruction{
  match instruction{

    Instruction::BinaryFunctor {opcode, address, functor} => {
      EncodedInstruction::DoubleWord(
        (Into::<u8>::into(*opcode) as DoubleWord) +
          (address.enc() << 8 ) + (encode_functor(&functor) << 32)
      )
    }

    Instruction::Binary {opcode, address1, address2 } => {
      let add1 = address1.enc();
      let add2 = address2.enc();
      // [OpCode:8][Address:24][Address:24][Reserved:8]
      EncodedInstruction::DoubleWord(
        (Into::<u8>::into(*opcode) as DoubleWord) + (add1 << 8) + (add2 << 32)
      )
    },

    Instruction::Unary {opcode, address} => {
      let add = address.enc() as Word;
      // [OpCode:8][Address:24]
      EncodedInstruction::Word(
        (Into::<u8>::into(*opcode) as Word) + (add << 8 )
      )
    },

    Instruction::Nullary(opcode) => {
      // [OpCode:8]
      EncodedInstruction::Word(Into::<u8>::into(*opcode) as Word)
    },
  }
}


/// Returns the size in WORDS of an instruction for the corresponding opcode.
pub fn instruction_size(opcode: Operation) -> u32{
  match Into::<u8>::into(opcode) < MAX_DOUBLE_WORD_OPCODE {
    true  => 2, // Two words
    false => 1  // One Word
  }
}

/**
  Given the low word of an instruction, determines whether the entire instruction is two
  words. This is how to determine if a second word needs to be provided for decoding.

  Note that this function does not check if the input has a valid opcode.
*/
pub fn is_double_word_instruction(word: Word) -> bool{
  (word as u8) < MAX_DOUBLE_WORD_OPCODE
}


pub fn decode_functor(word: Word) -> Functor{
  let functor_address = Address::from_funct_idx((word & 0xFFFF) as usize);
  let mut symbols = SYMBOLS.lock().unwrap();

  match symbols.get_by_right(&functor_address) {
    Some(functor) => functor.clone(),

    None => {
      // ToDo: Make a more robust automatic naming scheme. ASCII 97 = 'a'.
      let new_name = DefaultAtom::from(((97u8 + symbols.len() as u8) as char).to_string());
      let functor = Functor {
        name: new_name,
        arity: (word >> 16) as ArityType
      };
      symbols.insert(functor.clone(), functor_address);
      functor
    }
  }


}

pub fn encode_functor(functor: &Functor) -> DoubleWord{
  let mut symbols = SYMBOLS.lock().unwrap();
  let functor_idx = match symbols.get_by_left(functor) {

    Some(address) => address.idx(),

    None => {
      let address = Address::from_funct_idx(symbols.len());
      symbols.insert(functor.clone(), address);
      address.idx()
    }

  };

  ((functor.arity as DoubleWord) << 16) + (functor_idx as DoubleWord)
}
