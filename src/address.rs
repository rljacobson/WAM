//! An `Either` type that holds the address of either a register or memory, with some convenience
//! functions.

use std::ops::Add;
use std::fmt::{Display, Formatter};

pub type AddressType = usize;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Address{
  /// A "pointer" to a cell is an index into the `HEAP`. We could call it a cell reference.
  CellPtr(AddressType),
  /// A "pointer" to a register is an index into the register vector `X`.
  RegPtr(AddressType)
}

impl Address {
  /// Converts the address to an index into the corresponding vector.
  pub fn idx(&self) -> AddressType{
    match self{
      Address::CellPtr(i) => *i as AddressType,
      // Registers count from 1, so subtract 1 to convert to index.
      Address::RegPtr(i ) => (i-1) as AddressType
    }
  }

  /// Converts an index into the heap vector to a heap address..
  pub fn from_heap_idx(heap_idx: usize) -> Address{
    Address::CellPtr(heap_idx as AddressType)
  }

  /// Converts an index into the register vector to a register address.
  pub fn from_reg_idx(reg_idx: usize) -> Address{
    Address::RegPtr((reg_idx + 1) as AddressType)
  }

  /// Panics if the address is not a register pointer.
  pub fn require_register(&self){
    if let Address::CellPtr(_) = self{
      unreachable!(
        "Error: A heap pointer was given when a register pointer was required: {}",
        self
      );
    }
  }

  /// Panics if the address is not a heap pointer.
  #[allow(dead_code)]
  pub fn require_heap(&self){
    if let Address::RegPtr(_) = self{
      unreachable!(
        "Error: A register pointer was given when a heap pointer was required: {}",
        self);
    }
  }

  pub fn is_register(&self) -> bool {
    match self {
      Address::RegPtr(_) => true,
      _ => false
    }
  }

}


impl Display for Address{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Address::CellPtr(i) => {
        write!(f, "HEAP[{}]", i)
      },
      Address::RegPtr(i) => {
        write!(f, "X[{}]", i)
      }
    }
  }
}

// Increment an address
impl Add<AddressType> for Address{
  type Output = Address;
  fn add(self, rhs: AddressType) -> Address{
    match self{
      Address::CellPtr(i) => {
        Address::CellPtr(i+rhs)
      },
      Address::RegPtr(i) => {
        Address::RegPtr(i+rhs)
      }
    }
  }
}

