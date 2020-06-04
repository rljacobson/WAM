//! An `Either` type that holds the address of either a register or memory, with some convenience
//! functions.

use std::ops::Add;
use std::fmt::{Display, Formatter};

use crate::bytecode::Word;
use crate::functor::Functor;
use bimap::BiMap;

// `AddressType` is `usize`, as it is naturally an index into a memory store.
pub type AddressType = usize;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Address{
  /// A "pointer" to a cell is an index into the `HEAP`. We could call it a cell reference.
  Heap(AddressType),
  /// A "pointer" to a register is an index into the register vector `X`.
  Register(AddressType),
  /// An index into code memory
  Code(AddressType),
  /// A virtual address for functor symbols
  Functor(AddressType)
}

impl Address {
  /// Converts the address to an index into the corresponding vector.
  pub fn idx(&self) -> AddressType{
    match self{
      | Address::Heap(i)
      | Address::Functor(i)
      | Address::Code(i) => *i as AddressType,
      // Registers count from 1, so subtract 1 to convert to index.
      Address::Register(i ) => (i-1) as AddressType,
    }
  }

  /// Encodes the address into the bits as they appear in bytecode.
  pub fn enc(&self) -> Word {
    self.idx() as Word
  }

  /// Converts an index into the heap vector to a heap address.
  pub fn from_heap_idx(heap_idx: usize) -> Address{
    Address::Heap(heap_idx as AddressType)
  }

  /// Converts an index into the register vector to a register address.
  pub fn from_reg_idx(reg_idx: usize) -> Address{
    Address::Register((reg_idx + 1) as AddressType)
  }

  /// Converts an index into the heap vector to a heap address.
  #[allow(dead_code)]
  pub fn from_code_idx(heap_idx: usize) -> Address{
    Address::Code(heap_idx as AddressType)
  }

  // Converts a virtual functor address into an `Address:Functor`.
  pub fn from_funct_idx(funct_idx: usize) -> Address{
    Address::Functor(funct_idx as AddressType)
  }

  /// Panics if the address is not a register pointer.
  pub fn require_register(&self){
    if let Address::Register(_) = self{
      return;
    }
    unreachable!(
      "Error: A non-register pointer was given when a register pointer was required: {}",
      self
    );
  }

  /// Panics if the address is not a heap pointer.
  #[allow(dead_code)]
  pub fn require_heap(&self){
    if let Address::Heap(_) = self{
      return;
    }
    unreachable!(
      "Error: A non-heap pointer was given when a heap pointer was required: {}",
      self
    );
  }

  /// Panics if the address is not a code pointer.
  #[allow(dead_code)]
  pub fn require_code(&self){
    if let Address::Code(_) = self{
      return;
    }
    unreachable!(
      "Error: A non-code pointer was given when a code pointer was required: {}",
      self
    );
  }

  pub fn is_register(&self) -> bool {
    match self {
      Address::Register(_) => true,
      _ => false
    }
  }

  pub fn stringify(&self, symbols: BiMap<Functor, Address>) -> String {
    match self {
      Address::Heap(i) => {
        format!("HEAP[{}]", i)
      }
      Address::Register(i) => {
        format!("X[{}]", i)
      }
      Address::Code(i) => {
        format!("CODE[{}]", i)
      }
      Address::Functor(_i) => {
        let functor = symbols.get_by_right(self).unwrap();

        format!("{}", functor)
      }
    }
  }

}


impl Display for Address{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Address::Heap(i) => {
        write!(f, "HEAP[{}]", i)
      },
      Address::Register(i) => {
        write!(f, "X[{}]", i)
      },
      Address::Code(i) => {
        write!(f, "CODE[{}]", i)
      }
      Address::Functor(i) => {
        write!(f, "FUNCT[{}]", i)
      }
    }
  }
}

// Increment an address
impl Add<AddressType> for Address{
  type Output = Address;

  fn add(self, rhs: AddressType) -> Address{
    match self{
      Address::Heap(i) => {
        Address::Heap(i+rhs)
      }
      Address::Register(i) => {
        Address::Register(i+rhs)
      }
      Address::Code(i) => {
        Address::Code(i+rhs)
      }
      Address::Functor(i) => {
        Address::Functor(i+rhs)
      }
    }
  }
}

