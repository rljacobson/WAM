
// use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use strum_macros::{Display as StrumDisplay, IntoStaticStr};
use num_enum::{TryFromPrimitive, IntoPrimitive};

use crate::address::Address;
use crate::functor::Functor;
use crate::bytecode::Word;

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
Clone,        Copy,          Eq, PartialEq,  Debug,            Hash
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

impl Operation{
  pub fn code(&self) -> Word {
    Into::<u8>::into(*self) as Word
    // Into::<u8>::into(*self)
  }

  pub fn is_functor(&self) -> bool {
    self.code() < MAX_FUNCTOR_OPCODE
  }

  pub fn arity(&self) -> Word {
    match self.code() {
      value if value < MAX_DOUBLE_WORD_OPCODE  => 2,
      value if value < MAX_UNARY_OPCODE        => 1,
      _value                                   => 0
    }
  }
}
