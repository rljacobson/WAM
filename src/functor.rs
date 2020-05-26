use std::fmt::{Formatter, Display};

pub type ArityType = u16;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Functor {
  /// A lowercase letter
  pub name: char,
  pub arity: ArityType,
}

impl Display for Functor{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if self.is_constant(){
      write!(f, "{}", self.name)
    }else{
      write!(f, "{}/{}", self.name, self.arity)
    }
  }
}

impl Functor{
  pub fn is_constant(&self) -> bool {
    self.arity == 0
  }
}
