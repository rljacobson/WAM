/*!

  A `Cell` abstractly represents the data that may be stored at a single location in a memory store.
  The `Cell` enum can encode and decode a concrete binary representation of itself as a `Word`. A
  `Cell` can also produce its tag value used in its binary representation to indicate its datatype.

  The name `Cell` is a misnomer. Technically the word cell refers to a memory cell, a single space
  at a specific location in memory into which data can be stored. We are conflating the data with
  the storage space at which it lives. This is done out of convenience. We use the word address when
  we want to refer to a location in memory and word when we want to refer to the data. (Such use of
  both of these words are again misnomers: an address is not a house, and an acre is not a plot of
  land.)

*/

use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::functor::Functor;
use crate::address::Address;
use crate::bytecode::Word;

pub type RcCell = Rc<Cell>;
pub type CellVec = Vec<Cell>;

/**
  A `Cell` is a piece of data that can be stored at a register or heap `Address`.

  The `Cell::Structure` variant is only stored in registers as an intermediate representation.
*/
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Cell {
  /// A cell containing nothing. Used when growing a memory store so it can be filled out of order.
  Empty,
  /// `<STR, k>` where `k` is the address of a functor `f/n`; a pointer to a functor
  STR(Address),
  /// A pointer to a cell, displayed as `<REF, a>`, where `a` is the address being referenced.
  REF(Address),
  /// A functor, displayed as `f/n`, where `n` is the functor's arity.
  Functor(Functor),
  /// A functor structure with children, used as an intermediate representation in registers. The
  /// first element of the `CellVec` is a `Cell::Functor`; the remaining elements are arguments
  /// in the form of `Cell::REF`s.
  Structure(CellVec),
}



impl Display for Cell{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match &self{

      Cell::REF(a) => {
        write!(f, "<REF, {}>", a)
      }

      Cell::STR(a) => {
        write!(f, "<STR, {}>", a)
      }

      Cell::Functor(functor) => {
        write!(f, "{}", functor)
      }

      Cell::Structure(args) => {
        match args.len() {
          1 => {
            write!(f, "{}", args[0])
          }
          _ => {
            let mut arg_iter = args.iter();
            write!(
              f,
              "{}({})", arg_iter.next().unwrap(),
              arg_iter.map(|c| {c.to_string()})
                      .collect::<Vec<String>>()
                      .join(", ")
            )
          }
        }
      }

      Cell::Empty => {
        write!(f, "")
      }

    }
  }
}

impl Cell {

  /// Returns the binary form of the `Cell` as a `Word`.
  pub fn enc(&self) -> Word{
    match self{
      | Cell::REF(address)
      | Cell::STR(address) => {
        (address.enc() << 8) + self.tag()
      }
      Cell::Functor(functor) => {
        functor.enc()
      }
      Cell::Structure(_) => {
        unreachable!("Error: Cannot encode a `Cell::Structure`.")
      }
      Cell::Empty => {
        self.tag()
      }
    }
  }

  pub fn try_decode(encoded: Word){

  }

  /// Gives the 8-bit numeric value that represents the cell variant. To encode both the variant
  /// and its payload use `Cell::enc()`.
  pub fn tag(&self) -> Word {
    // We use the unsafe `std::intrinsics::discriminant_value()` to avoid a copy.
    // ToDo: Surely there is a better way to coerce the variant to a `u8` using some version of
    //       `Into::<u8>::into(*self) as Word`
    std::intrinsics::discriminant_value(self) as Word
  }

  /// Extracts the address from either `Cell::REF` or `Cell::STR` values.
  pub fn extract_address(&self) -> Option<Address> {
    match self {

      | Cell::REF(address) | Cell::STR(address) => Some(*address),

      _ => None

    }
  }


  /// A convenience function to extract all the addresses from a `Cell::Structure`.
  pub fn extract_arg_addresses(&self) -> Option<HashSet<Address>>{
    match self{

      Cell::Structure(cell_vec) => {
        let addresses =
          cell_vec.iter().filter_map(| c | {
            match &c{

              Cell::REF(address) => Some(*address),

              _ => None

            }
          }).collect();
        Some(addresses)
      }

      _ => None

    }
  }


  /// Extracts the functor from either `Cell::Structure` or `Cell::Functor` values, but not
  /// Cell::STR. For recursively dereferenced functor extraction, use `WMV::extract_functor()`,
  /// which has access to the VM heap, stack, code, and registers required for dereferencing.
  pub fn extract_functor(&self) -> Option<Functor>{
    match self {

      Cell::Functor(functor)    => Some(functor.clone()),

      Cell::Structure(cell_vec) => {
        // `cell_vec` is actually guaranteed to have a `Cell::Functor` at index 0.
        if let Cell::Functor(functor) = &cell_vec[0] {
          Some(functor.clone())
        } else { unreachable!(); }
      }

      _                         => None

    }
  }
}

