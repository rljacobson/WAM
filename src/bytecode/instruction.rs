
// use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use strum_macros::{Display as StrumDisplay, IntoStaticStr};
use num_enum::{TryFromPrimitive, IntoPrimitive};

use crate::address::{Address};
use crate::functor::Functor;
use crate::bytecode::Word;
// pub use assembly::parse_assembly;

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

pub const MAX_BINARY_OPCODE: u8 = 12u8;
pub const MAX_DOUBLE_WORD_OPCODE: u8 = 6u8;
pub const MAX_FUNCTOR_OPCODE: u8 = 2u8;

/// Holds the unencoded components of an instruction. As such, it enumerates the possible
/// instruction argument combinations.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Instruction {
  /// [OpCode:8][Address:24][Address:24][Reserved:8]
  BinaryFunctor{
    opcode   :  Operation,
    address  :  Address,
    functor  :  Functor
  },
  Binary {
    opcode: Operation,
    address1: Address,
    address2: Address
  },
  /// [OpCode:8][Address:24]
  Unary {
    opcode: Operation,
    address: Address
  },
  /// [OpCode:8][Reserved:24]
  Nullary(Operation),
}

impl Display for Instruction {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{

      Instruction::BinaryFunctor{opcode, address, functor} => {
        write!(f, "{}({}, {})", opcode, functor, address)
      }

      Instruction::Binary{opcode, address1, address2 } => {
        write!(f, "{}({}, {})", opcode, address1, address2)
      }

      Instruction::Unary { opcode, address} => {
        write!(f, "{}({})", opcode, address)
      }

      Instruction::Nullary(opcode) => {
        write!(f, "{}", opcode)
      }

    }
  }
}

impl Operation{
  pub fn code(&self) -> u8 {
    Into::<u8>::into(*self)
  }

  pub fn is_functor(&self) -> bool {
    self.code() < MAX_FUNCTOR_OPCODE
  }

  pub fn arity(&self) -> Word {
    match self.code() {
      value if value < MAX_DOUBLE_WORD_OPCODE  => 2,
      value if value < MAX_BINARY_OPCODE => 1,
      _value => 0
    }
  }
}
