use string_cache::DefaultAtom;

use crate::address::*;
use std::collections::HashMap;
use nom::lib::std::collections::hash_map::Iter;
use std::collections::hash_map::Values;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Variable {
  pub name: DefaultAtom,
  pub is_permanent: bool,
  pub seen: bool,
  pub address: Option<Address>
}

impl Variable{
  fn new(name: DefaultAtom, is_permanent: bool) -> Variable{
    Variable{
      name,
      is_permanent,
      seen: false,
      address: None
    }
  }
}

/**
  A data structure to hold information about the variables in a clause.
*/
pub struct Variables {
  variables: HashMap<DefaultAtom, Variable>,
}

impl Variables{

  pub fn iter(&self) -> Values<DefaultAtom, Variable> {
    self.variables.values()
  }

  pub fn insert()
}
