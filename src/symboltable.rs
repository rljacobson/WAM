use crate::address::Address;
use bimap::BiMap;
use crate::functor::Functor;

/**
  A symbol table for M_1&2 is a mapping between functor symbols of the form `f/n` to the address
  of the functor's procedure in code memory. A symbol table is really just a convenience wrapper
  around a BiMap.

*/

struct SymbolTable{
  table: BiMap<Functor, Address>
}

impl SymbolTable{

  pub fn new() -> SymbolTable {
    SymbolTable{
      table: BiMap::new()
    }
  }

  pub fn get_symbol(&self, address: &Address) -> Option<Functor>{
    self.table.get_by_right(address).cloned()
  }

  pub fn get_address(&self, functor: &Functor) -> Option<Address>{
    address.require_code();
    self.table.get_by_left(functor).cloned()
  }

  pub fn insert(&mut self, functor: Functor, address: Address)
    -> Result<(), (Functor, Address)>{
    address.require_code();
    self.table.insert_no_overwrite(functor, address)
  }
}
