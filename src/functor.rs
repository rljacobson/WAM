//! A special functor struct is necessary, because we need to know the arity of the functor after
//! we no longer have access to its vector of arguments. Thus, a `Functor` is a name and an arity.

use std::fmt::{Formatter, Display};

use string_cache::DefaultAtom;

use crate::bytecode::Word;

pub type ArityType = Word;

/**
  The `Functor` struct represents a symbol f/n. Clones are cheap. Functors are interned
  to preserve their names during serializing and deserializing bytecode. As with all
  strings in this codebase, `Functor::name` is interned. Note that f/2 != f/3, i.e.
  functors are the same if and only if both their name and their arity are the same.
*/
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Functor {
  /// A lowercase letter
  pub name  : DefaultAtom,
  pub arity : ArityType,
  /*
  ToDo: It is annoying to have to carry around the arity when most of the
        time the functor is packaged with its arguments. The problem is,
        it's separated from its arguments during tokenization. Does it need
        to be?
  */
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

lazy_static!(
  pub static ref DUMMY_FUNCTOR: Functor = Functor{ name: DefaultAtom::from("dummy"), arity: 0};
);
