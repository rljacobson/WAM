/*!
  Representation of VM instructions abstractly and as binary bytecode.
*/


use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use strum_macros::{Display as StrumDisplay, IntoStaticStr, EnumString};
use num_enum::{TryFromPrimitive, IntoPrimitive};

use crate::address::Address;
use crate::functor::Functor;


// If you change this you must also change the encode and decode methods of everything that
// serializes to Words.
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

impl Bytecode{
  /**
  Given the low word of an instruction, determines whether the entire instruction is two
  words. This is how to determine if a second word needs to be provided for decoding.

  Note that this function does not check if the input has a valid opcode.
  */
  pub fn is_double_word(word: &Word) -> bool {
    (*word & 0xFF) < MAX_DOUBLE_WORD_OPCODE
  }

  pub fn try_decode(&self) -> Option<Instruction> {
    let mut bytecode = DoubleWord { low: 0, high: 0 };
    match &self {
      Bytecode::Word(w) => { bytecode.low = *w; }
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
      let address = match Address::try_decode(bytecode.low >> 8) {
        Some(a) => a,
        None    => { return None; }
      };
      Instruction::Binary {
        opcode,
        address,
        argument: Argument::Functor(Functor::dec(bytecode.high)),
      }
    } else if opcode.code() < MAX_DOUBLE_WORD_OPCODE {
      // [OpCode:8][Address:24][Address:24][Reserved:8]
      let address = match Address::try_decode(bytecode.low >> 8) {
        Some(a) => a,
        None    => { return None; }
      };
      let address2 = match Address::try_decode(bytecode.high) {
        Some(a) => a,
        None    => { return None; }
      };
      Instruction::Binary {
        opcode,
        address,
        argument: Argument::Address(address2)
      }
    } else if opcode.code() < MAX_UNARY_OPCODE {
      // [OpCode:8][Address:24]
      let argument =
      match opcode {
        Operation::Allocate => Argument::Word(bytecode.low >> 8),

        _ => {
          let address = match Address::try_decode(bytecode.low >> 8) {
            Some(a) => a,
            None => { return None; }
          };
          Argument::Address(address)
        }
      };
      Instruction::Unary {
        opcode,
        argument
      }
    } else {
      // [OpCode:8]
      Instruction::Nullary(opcode)
    };

    Some(instruction)
  }
}


impl Display for Bytecode{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Bytecode::Word(word)         => write!(f, "0x{:0>4X}", word),
      Bytecode::DoubleWord(double) => write!(f, "0x{:0>5X}{:0>4X}", double.high, double.low)
    }

  }
}

/**
  Opcodes of the virtual machine.

  Rust stores enum variants as bytes. As in C, enum values are represented by consecutive
  natural numbers and can be treated as numeric types. Therefore, we group the 64 bit
  opcodes and 32 bit opcodes together so that a given opcode's size can be determined
  with a trivial comparison. The format of the instruction is likewise determined
  by order. Consequently, the order the opcodes are listed below is significant.
  Order-dependencies:
      ```
      Operation::arity()
      binary::instruction_size()
      binary::try_decode_instruction()
      ```
*/
#[derive(
  StrumDisplay, IntoStaticStr, EnumString, TryFromPrimitive, IntoPrimitive,
  Clone,        Copy,          Eq,         PartialEq,        Debug, Hash
)]
#[repr(u8)]
pub enum Operation {
  // 64 bit instructions //
  // M0 Opcodes //
  PutStructure,      // put_structure( f/n, address )
  GetStructure,      // get_structure( f/n, address)
  // Opcode 2

  // M1 Opcodes
  PutVariable,       // put_variable( address, address )
  GetVariable,       // get_variable( address, address )
  PutValue,          // put_value( address, address )
  GetValue,          // get_value( address, address )
  // Opcode 6

  // 32 bit instructions //
  // M0 Opcodes //
  SetVariable,       // set_variable( address )
  SetValue,          // set_value( address )
  UnifyVariable,     // unify_variable( address )
  UnifyValue,        // unify_value( address )
  // M1 Opcode //
  Call,              // call( f/n )

  // M2 Opcode //
  Allocate,          // allocate( n )
  // Opcode 12

  // Padded byte Opcodes (32 bits) //
  // M1 Opcode //
  Proceed,           // proceed

  // M2 Opcode //
  Halt,              // halt
  Deallocate,        // deallocate

}

pub const MAX_UNARY_OPCODE       :  Word  = 12;
pub const MAX_DOUBLE_WORD_OPCODE :  Word  = 6;
pub const MAX_FUNCTOR_OPCODE     :  Word  = 2;

impl Operation{

  /// Gives the 8-bit numeric value that represents the operation. An operation's code is
  /// sometimes called a tag. The tag is padded with zeros to make a `Word`.
  pub fn code(&self) -> Word {
    // Into::<u8>::into(*self) as Word
    let val: u8 = (*self).into();
    val as Word
  }

  pub fn is_functor(&self) -> bool {
    self.code() < MAX_FUNCTOR_OPCODE
  }

  /**
  Given the low word of an instruction, determines whether the entire instruction is two
  words. This is how to determine if a second word needs to be provided for decoding.

  Note that this function does not check if the input has a valid opcode.
  */
  pub fn is_double_word(word: &Word) -> bool {
    (*word & 0xFF) < MAX_DOUBLE_WORD_OPCODE
  }

  pub fn arity(&self) -> Word {
    match self.code() {
      value if value < MAX_DOUBLE_WORD_OPCODE  => 2,
      value if value < MAX_UNARY_OPCODE        => 1,
      _value                                   => 0
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Argument{
  Address(Address),
  Word(Word),
  Functor(Functor)
}

impl Display for Argument{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{

      Argument::Address(address) => write!{f, "{}", address},

      Argument::Word(word)       => write!{f, "{}", word   },

      Argument::Functor(functor) => write!{f, "{}", functor}

    }
  }
}

/// Holds the unencoded components of an instruction. As such, it enumerates the possible
/// instruction argument combinations.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Instruction {
  /// [OpCode:8][Address:24][Address:24][Reserved:8]
  Binary{
    opcode   :  Operation,
    address  :  Address,
    argument :  Argument
  },
  /// [OpCode:8][Address:24]
  Unary {
    opcode   :  Operation,
    argument :  Argument
  },
  /// [OpCode:8][Reserved:24]
  Nullary(Operation),
}

impl Display for Instruction {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{

      Instruction::Binary{opcode, address, argument: Argument::Functor(functor) } => {
      write!(f, "{}({}, {})", opcode, functor, address)
      }

      Instruction::Binary{opcode, address, argument: Argument::Address(address2) } => {
      write!(f, "{}({}, {})", opcode, address, address2)
      }

      Instruction::Unary { opcode, argument}          => {
        write!(f, "{}({})", opcode, argument)
      }

      Instruction::Nullary(opcode)                    => {
        write!(f, "{}", opcode)
      }

      _ => { unreachable!() }
    }
  }
}

impl Instruction {

  /**
  Encodes the instruction into bytecode. It is the caller's responsibility to
  use the correct `Instruction` variant for the given opcode.
  */
  pub fn encode_instruction(&self) -> Bytecode {
    match self {
      Instruction::Binary { opcode, address, argument } => {
        let high = match argument {
          Argument::Address(address2) => address2.enc(),
          Argument::Functor(functor) => functor.enc(),
          Argument::Word(_) => {
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

      Instruction::Unary { opcode, argument } => {
        // [OpCode:8][Address:24]
        let parameter = match argument {
          Argument::Address(address) => address.enc(),
          Argument::Word(word) => *word,
          Argument::Functor(_) => {
            unreachable!(
              "Error: `Argument::Functor` cannot be an argument to `Instruction::Unary`."
            );
          },
        };
        Bytecode::Word(opcode.code() + (parameter << 8))
      },

      Instruction::Nullary(opcode) => {
        // [OpCode:8]
        Bytecode::Word(opcode.code())
      },
    }
  }


  /// Returns the size in WORDS of an instruction for the corresponding opcode.
  #[allow(dead_code)]
  pub fn size(&self) -> usize {
    match self {
      | Instruction::Binary{opcode, ..}
      | Instruction::Unary {opcode, ..}
      | Instruction::Nullary(opcode) => {
        match opcode.code() < MAX_DOUBLE_WORD_OPCODE {
          true => 2, // Two words
          false => 1  // One Word
        }
      }
    }

  }

}
