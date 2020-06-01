/*!

  The VM uses a 32 bit little-endian word size. Instructions are either 32 or 64
  bits (including opcode, see last paragraph). Memory blocks are 64 bit aligned, and
  instructions within memory are 32 bit aligned, with trailing padding as necessary.
  Instructions are decoded bytewise. Memory addresses are 24 bits (3 bytes) and
  address words, not bytes. The sizes of instruction components are as follows:

    Opcode:   8 bits
    Address: 24 bits
    Arity:   24 bits

  Functors do not appear explicitly in the bytecode. They are symbolic representations
  labeling locations in code memory. During compilation and optionally for execution
  (for diagnostic or pedagogic purposes),  functors are resolved to their address via
  a lookup in a symbol table maintained for that purpose.

  One design decision that needed to be made is whether to store the arguments of the
  instruction as data members of enum variants, with one variant per opcode. Rust enums use
  an 8 bit discriminant and have size equal to the largest data payload plus the discriminant.
  (In certain circumstances not relevant here, the compiler can optimize enum values to be
  smaller.) Since the largest instruction is 48 bits excluding opcode, the equivalent enum values
  are 56 bits. Even without considering data alignment, this would waste ~50% of allocated
  memory for instructions on average, which is unacceptably wasteful. Instead, an enum is
  only used for the opcode itself, not the entire instruction, and inhabits a single byte.

*/
#![allow(dead_code)]

use strum_macros::{Display as StrumDisplay, IntoStaticStr};
use num_enum::{TryFromPrimitive, IntoPrimitive};
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

// If you change this you must also change `encode_instruction` and `decode_instruction`.
pub type Word = u32;
pub type DoubleWord = u64;
// Convenience for decomposing a DoubleWord into a high word and a low word:
pub struct TwoWords {
  pub low: Word,
  pub high: Word
}

/**
  Opcodes of the virtual machine.

  Rust stores enum variants as bytes. As in C, enum values are represented by consecutive
  natural numbers and can be treated as numeric types. Therefore, we group the 64 bit
  opcodes and 32 bit opcodes together so that a given opcode's size can be determined
  with a trivial comparison. The format of the instruction is likewise determined
  by order. Consequently, the order the opcodes are listed below is significant.
*/
#[derive(
  StrumDisplay, IntoStaticStr, EnumString, TryFromPrimitive, IntoPrimitive,
  Clone,        Copy,          PartialEq,  Debug,            Hash
)]
#[repr(u8)]
pub enum Opcode {
  // 64 bit instructions //
  // M0 Opcodes
  PutStructure,
  GetStructure,
  // M1 Opcodes
  PutVariable,
  GetVariable,
  PutValue,
  GetValue,         // Opcode 6

  // 32 bit instructions //
  // M0 Opcodes
  SetVariable,
  SetValue,
  UnifyVariable,
  UnifyValue,
  // M1 Opcode
  Call,
  // M2 Opcode
  Allocate,         // Opcode 12

  // Padded byte Opcodes (32 bits)
  // M1 Opcode
  Proceed,
  // M2 Opcode
  Deallocate,
}

/// Holds the unencoded components of an instruction
#[derive(Copy, Clone, Debug)]
pub enum InstructionArguments {
  /// [OpCode:8][Address:24][Address:24][Reserved:8]
  Binary {opcode: Opcode, address1: Word, address2: Word},
  /// [OpCode:8][Address:24]
  Unary {opcode: Opcode, address: Word},
  /// [OpCode:8][Reserved:24]
  Nullary(Opcode),
}

impl Display for InstructionArguments{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{

      InstructionArguments::Binary{opcode, address1: add1, address2: add2 } => {
        write!(f, "{}({}, {})", opcode, add1, add2)
      }

      InstructionArguments::Unary { opcode, address} => {
        write!(f, "{}({})", opcode, address)
      }

      InstructionArguments::Nullary(opcode) => {
        write!(f, "{}", opcode)
      }

    }
  }
}


/// An `Either` type for an encoded instruction, allowing the instruction to be
/// either one word or two.
pub enum EncodedInstruction{
  Word(Word),
  DoubleWord(DoubleWord)
}

pub fn decode_instruction(instruction: DoubleWord) -> InstructionArguments {
  let opcode: Opcode =
    match Opcode::try_from((instruction & 0xFF) as u8){
      Ok(v)  => v,
      Err(e) => {
        // ToDo: Alternatively, return an `Option<InstructionArguments>`.
        panic!("{}", e);
      }
    };

  if Into::<u8>::into(opcode) < 6 {
    // [OpCode:8][Address:24][Address:24][Reserved:8]
    InstructionArguments::Binary {
      opcode,
      address1: ((instruction >> 8) & 0xFFFFFF) as Word,
      address2: (instruction >> 32) as Word,
    }

  } else if Into::<u8>::into(opcode) < 12 {
    // [OpCode:8][Address:24]
    InstructionArguments::Unary {
      opcode,
      address: (instruction >> 8) as Word
    }

  } else {
    // [OpCode:8]
    InstructionArguments::Nullary(opcode)
  }
}

/**
  Encodes the instruction into bytecode. It is the caller's responsibility to
  use the correct `InstructionArguments` variant for the given opcode.
*/
pub fn encode_instruction(instruction: InstructionArguments) -> EncodedInstruction{
    match instruction{

      InstructionArguments::Binary {opcode, address1: add1, address2: add2 } => {
        EncodedInstruction::DoubleWord(
          ( opcode as DoubleWord)        +
          ((add1   as DoubleWord) << 8 ) +
          ((add2   as DoubleWord) << 32)
        )
      },

      InstructionArguments::Unary {opcode, address} => {
        EncodedInstruction::Word(
          ( opcode as Word)        +
          ((address   as Word) << 8 )
        )
      },

      InstructionArguments::Nullary(opcode) => {
        EncodedInstruction::Word(opcode as Word)
      },
    }
}


/// Returns the size in WORDS of an instruction for the corresponding opcode.
pub fn instruction_size(opcode: Opcode) -> u32{
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
