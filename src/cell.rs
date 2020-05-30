//! Concrete in-memory representation of terms

use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::address::Address;
use crate::functor::Functor;
use crate::term::RcTerm;

pub type RcCell = Rc<Cell>;
pub type CellVec = Rc<Vec<Rc<Cell>>>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Cell {
  /// `<STR, k>` where `k` is the address of a functor `f/n`; a pointer to a functor
  STR(Address),
  /// A pointer to a cell, displayed as `<REF, a>`, where `a` is the address being referenced.
  REF(Address),
  /// A functor, displayed as `f/n`, where `n` is the functor's arity.
  Functor(Functor),
  /// A functor structure and children, used as an intermediate representation in registers. It
  /// displays as a functor followed by a sequence of its children.
  Structure(CellVec),
  /// Used to record context in an error condition.
  Term(RcTerm),
  /// A cell containing nothing. Used when growing a CellVec so it can be filled out of order.
  Empty
}

impl Display for Cell{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Cell::REF(a) => {
        write!(f, "<REF, {}>", a)
      },
      Cell::STR(a) => {
        write!(f, "<STR, {}>", a)
      },
      Cell::Functor(funct) => {
        write!(f, "{}", funct)
      },
      Cell::Structure(v) => {
        write!(
          f,
          "{}",
          v
            .iter()
            .map(|c| { format!("{}", c)})
            .collect::<Vec<String>>()
            .join(", ")
        )
      },
      Cell::Term(term) => {
        write!(f, "{}", term)
      }
      Cell::Empty => {
        write!(f, "`")
      },
    }
  }
}


/// A convenience function to extract all the addresses from a `CellVec`.
pub fn extract_addresses(cell: RcCell) -> Option<HashSet<Address>>{
  match &*cell{
    Cell::Structure(cell_vec) => {
      let addresses =
        cell_vec.iter().filter_map(| c | {
          match &*c.clone(){
            Cell::REF(address) => Some(*address),
            _ => None
          }
        }).collect();
      Some(addresses)
    }
    _ => None
  }
}
