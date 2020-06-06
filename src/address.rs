//! An `Either` type that holds the address of either a register or memory, with some convenience
//! functions.

use std::ops::Add;
use std::fmt::{Display, Formatter};

// `AddressType` is `usize`, as it is naturally an index into a memory store.
pub type AddressNumberType = usize;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub enum Address{
  /// A "pointer" to a cell is an index into the `HEAP`. We could call it a cell reference.
  Heap(AddressNumberType),
  /// A "pointer" to a register is an index into the register vector `X`.
  Register(AddressNumberType)
}

impl Address {
  /// Converts the address to an index into the corresponding vector.
  pub fn idx(&self) -> AddressNumberType {
    match self{
      Address::Heap(i) => *i as AddressNumberType,
      // Registers count from 1, so subtract 1 to convert to index.
      Address::Register(i ) => (i-1) as AddressNumberType
    }
  }

  /// Converts an index into the heap vector to a heap address.
  pub fn from_heap_idx(heap_idx: usize) -> Address{
    Address::Heap(heap_idx as AddressNumberType)
  }

  /// Converts an index into the register vector to a register address.
  pub fn from_reg_idx(reg_idx: usize) -> Address{
    Address::Register((reg_idx + 1) as AddressNumberType)
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

  pub fn is_register(&self) -> bool {
    match self {
      Address::Register(_) => true,
      _ => false
    }
  }

}


impl Display for Address{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Address::Heap(i) => {
        write!(f, "HEAP[{}]", i)
      },
      Address::Register(i) => {
        write!(f, "X[{}]", i)
      }
    }
  }
}

// Increment an address
impl Add<AddressNumberType> for Address{
  type Output = Address;
  fn add(self, rhs: AddressNumberType) -> Address{
    match self{
      Address::Heap(i) => {
        Address::Heap(i+rhs)
      },
      Address::Register(i) => {
        Address::Register(i+rhs)
      }
    }
  }
}

