//! A special functor struct is necessary, because we need to know the arity of the functor after
//! we no longer have access to its vector of arguments. Thus, a `Functor` is a name and an arity.

use std::fmt::{Formatter, Display};
use string_cache::DefaultAtom;
use crate::bytecode::Word;

pub type ArityType = u32;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Functor {
  /// A lowercase letter
  pub name  : DefaultAtom,
  pub arity : ArityType,

  // ToDo: It is annoying to have to carry around the arrity when most of the
  //       time the functor is packaged with its arguments. The problem is,
  //       it's separated from its arguments during tokenization. Does it need
  //       to be?
}

impl Functor{
  pub fn from_word(word: Word) -> Functor{
    DefaultAtom::try_from(word & 0xFFFF)
  }
}

impl Display for Functor{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if self.arity == 0{
      write!(f, "{}", self.name)
    }else{
      write!(f, "{}/{}", self.name, self.arity)
    }
  }
}
