/*!
  This module defines the `Functor` struct, which represents a functor symbol f/n, the binary
  serialization/deserialization methods for `Functor`, and a global thread-safe interner for
  functors.

  A special functor struct is necessary, because we need to know the arity of the functor after
  we no longer have access to its vector of arguments. Thus, a `Functor` is a name and an arity.
  As with strings, we intern functors. The global functor internment is safe to use across
  multiple VMs (but not from multiple threads) because no VM specific information is stored.

*/

use std::fmt::{Formatter, Display};
use std::sync::{Arc, Mutex};

use string_cache::DefaultAtom;
use bimap::BiMap;

use crate::bytecode::Word;
use crate::address::Address;

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
        time the functor is packaged with its arguments. Does it need to?
  */
}


impl Functor{

  /**
    Returns a binary encoding of the functor as a `Word`.
  */
  pub fn enc(&self) -> Word{
    // ToDo: There should be an encoding that sores the functor name and arity rather than the
    //       functor's address in the symbol table. This is easy to do if we give up one of:
    //          1. a byte from the arity, which isn't unreasonable;
    //          2. the idea of every cell value having a tag.
    //       Tagged cell values are necessary for displaying the decoded contents of cells.
    let functor_address = intern_functor(self);

    functor_address.enc()
  }


  /**
    Decodes a `Word` as a functor.

    The method never fails, as every `Word` can be interpret4d as an encoded
    functor. However, a random `Word` is likely to decode to a functor
    with an unusually large arity and an unknown (to the interner) name.
  */
  pub fn dec(word: Word) -> Functor{
    // let functor_address = Address::Functor(word as AddressNumberType);
    let functor_address =
      match Address::try_decode(word & 0xFFFF){

        Some(a) => a,

        // ToDo: What should happen in this arm?
        None => Address::from_funct_idx((word & 0xFFFF) as usize),

      };

    match try_get_interned_functor(&functor_address) {

      Some(functor) => functor.clone(),

      None          => {
        // ToDo: Make a more robust automatic naming scheme. This should only be necessary if the
        //       code/data is read from a binary file rather than being produced from source code.
        let new_name = DefaultAtom::from(
          // ASCII 97 = 'a'.
          ((97u8 + functor_address.idx() as u8) as char).to_string()
        );
        let functor  = Functor {
          name  : new_name,
          arity : (word >> 16) as ArityType
        };
        intern_functor(&functor);
        functor
      }

    }
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


lazy_static!(
  pub static ref DUMMY_FUNCTOR: Functor = Functor{ name: DefaultAtom::from("dummy"), arity: 0};
);


lazy_static! {
  /**

    This symbol table keeps track of the names of functors that have been compiled
    to bytecode or stored on the heap so that the names can be reconstituted for the
    human reader. Otherwise, functors would have auto-generated names. The symbol
    table can be thought of as just another memory store of the VM except with the
    property that it can be transparently shared with other VMs, even across threads.

    The symbol table associates to each unique functor a "virtual address," which is a proxy
    for the functor symbol `f/n`. (This is sometimes called a handle or ID.) This is distinct
    from the internal representation of the interned string of the name, which is handled by
    `string_cache`. As with interned strings, the virtual functor address is not guaranteed
    to be the same between runs. The virtual address is used to encode the functor into its
    binary representation, which is how the functor is represented in memory, either as data
    on the heap or embedded in bytecode instructions that take a functor as an argument.

  */
  // ToDo: Arc -> RwLock
  pub static ref SYMBOLS: Arc<Mutex<BiMap<Functor, Address>>> =
    Arc::new(Mutex::new(BiMap::new()));
}


/// Retrieves the functor for the given functor address if it exists in the symbol table.
/// This function is thread safe.
pub fn try_get_interned_functor(address: &Address) -> Option<Functor>{
  let symbols       = SYMBOLS.lock().unwrap();
  let maybe_functor = symbols.get_by_right(address);

  match maybe_functor {

    Some(functor) => Some(functor.clone()),

    None    => None

  }
}


/// Gives the virtual address of a `Functor`, creating a new entry in the symbol table if
/// necessary. The virtual address is a stand-in for the functor's text name in compiled code.
/// This function is thread safe.
pub fn intern_functor(functor: &Functor) -> Address{
  let mut symbols = SYMBOLS.lock().unwrap();

  match symbols.get_by_left(functor) {

    Some(address) => *address,

    None          => {
      let address = Address::Functor(symbols.len());
      symbols.insert(functor.clone(), address);
      address
    }

  }
}
