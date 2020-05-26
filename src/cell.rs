

use std::fmt::{Display, Formatter};
use crate::address::Address;
use crate::functor::Functor;
use crate::term::Term;

/// Concrete in-memory representation of terms
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Cell {
  /// `<STR, k>` where `k` is the address of a functor `f/n`; a pointer to a functor
  STR(Address),
  /// A pointer to a cell
  REF(Address),
  /// A functor
  Functor(Functor),
  /// A Term, used as an intermediate representation in registers for error handling.
  Term(Term),
  /// Unfilled cell.
  Empty,
  String(Vec<Cell>)
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
      Cell::Term(term) => {
        write!(f, "<Term: {}>", term)
      },
      Cell::Empty => {
        write!(f, "`")
      },
      Cell::String(v) => {
        write!(f, "{}", v.iter().map(|c| { format!("{}", c)}).collect::<Vec<String>>().join(", "))
      }
    }
  }
}
