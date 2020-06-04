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

mod binary;
// mod assembly;

use strum_macros::{Display as StrumDisplay, IntoStaticStr};
use num_enum::{TryFromPrimitive, IntoPrimitive};
// use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

use crate::address::{Address};
pub use binary::{encode_instruction, EncodedInstruction, Word, TwoWords};
use crate::functor::Functor;
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
      Instruction::arity()
      binary::instruction_size()
      binary::try_decode_instruction()
      ```
*/
#[derive(
  StrumDisplay, IntoStaticStr, EnumString, TryFromPrimitive, IntoPrimitive,
  Clone,        Copy,          PartialEq,  Debug,            Hash
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
  Deallocate,        // deallocate

}

/// Holds the unencoded components of an instruction
#[derive(Copy, Clone, Debug)]
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

      Instruction::Binary{opcode, address1: add1, address2: add2 } => {
        write!(f, "{}({}, {})", opcode, add1, add2)
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
  pub fn arity(&self) -> u32 {
    match Into::<u8>::into(*self) {
      value if value < 6  => 2,
      value if value < 12 => 1,
      _value => 0
    }
  }
}
