/*!

  A pointer type that knows which among the heap, code, registers, and stack that it points to. The
  memory types are called segments, e.g. heap segment, code segment, and so forth. The
  `Functor::require_*` methods are intended for debug-mode to catch logic errors resulting in an
  address to memory store A being used in a context meant only for an address to memory store B.

  If the address type will ever be refactored in the future to be a simple pointer (`*Word`), the
  API shouldn't have to change, just the implementation details in this file alone.

*/

use std::ops::Add;
use std::fmt::{Display, Formatter};
use std::convert::TryFrom;

use strum_macros::{EnumDiscriminants, Display as StrumDisplay};
use num_enum::{TryFromPrimitive, IntoPrimitive};

use crate::bytecode::Word;

// `AddressType` is `usize`, as it is naturally an index into a memory store.
pub type AddressNumberType = usize;

#[derive(
  EnumDiscriminants,
  Clone,             Copy,             Eq,
  PartialEq,         Debug,            Hash,
)]
#[strum_discriminants(derive(IntoStaticStr, StrumDisplay, IntoPrimitive, TryFromPrimitive))]
#[strum_discriminants(name(Segment))]
#[strum_discriminants(repr(u8))]
#[repr(u8)]
pub enum Address{
  /// A "pointer" to a cell is an index into the `HEAP`. We could call it a cell reference.
  Heap(     AddressNumberType ),
  /// A "pointer" to a register is an index into the register vector `X`.
  Register( AddressNumberType ),
  /// An index into code memory
  Code(     AddressNumberType ),
  /// A virtual address for functor symbols
  Functor(  AddressNumberType ),
}

impl Address {
  /// Converts the address to an index into the corresponding vector.
  pub fn idx(&self) -> AddressNumberType {
    match self{
      | Address::Heap(i)
      | Address::Functor(i)
      | Address::Code(i) => *i as AddressNumberType,
      // Registers count from 1, so subtract 1 to convert to index.
      Address::Register(i) => (i-1) as AddressNumberType,
    }
  }

  pub fn tag(&self) -> Word {
    std::intrinsics::discriminant_value(self) as Word
  }

  /// Encodes the address into the bits as they appear in bytecode.
  pub fn enc(&self) -> Word {
    let index = self.idx() as Word;
    (index << 2) | self.tag()
  }

  pub fn try_decode(word: Word) -> Option<Address>{
    let tag     = word & 0b11;  // First two bits are the tag.
    let index   = word >> 2;

    match Segment::try_from(tag as u8) {
      Ok(Segment::Heap)     => Some(Address::from_heap_idx( index as usize )),
      Ok(Segment::Register) => Some(Address::from_reg_idx(  index as usize )),
      Ok(Segment::Code)     => Some(Address::from_code_idx( index as usize )),
      Ok(Segment::Functor)  => Some(Address::from_funct_idx(index as usize )),

      _                     => { unreachable!(); }
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

  /// Converts an index into the heap vector to a heap address.
  #[allow(dead_code)]
  pub fn from_code_idx(heap_idx: usize) -> Address{
    Address::Code(heap_idx as AddressNumberType)
  }

  /*
  Converts a virtual functor address into an `Address:Functor`.

  Does not look in the symbol table to see if a functor at that address exists. To retrieve a
  functor from the symbol table, use `functor::try_get_interned_functor()`.

  */
  pub fn from_funct_idx(funct_idx: usize) -> Address{
    Address::Functor(funct_idx as AddressNumberType)
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

  /// Panics if the address is not a functor virtual address.
  #[allow(dead_code)]
  pub fn require_functor(&self){
    if let Address::Functor(_) = self{
      return;
    }
    unreachable!(
      "Error: A non-functor address was given when a functor address was required: {}",
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
impl Add<AddressNumberType> for Address{
  type Output = Address;

  fn add(self, rhs: AddressNumberType) -> Address{
    // ToDo: Is there a more compact way to write this since the payload of all variants is the
    //       same?
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

