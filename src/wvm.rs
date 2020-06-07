//! Structures and functions for the Warren Virtual Machine, what I'm calling an
//! implementation of Warren's Abstract Machine.

use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::usize::MAX;
use std::rc::Rc;

use prettytable::{format as TableFormat, Table};
use string_cache::DefaultAtom;

use crate::address::*;
use crate::cell::*;
use crate::functor::*;
use crate::token::*;
use crate::term::{TermVec, Term};


#[allow(non_snake_case)]
pub struct WVM {

  // Flags
  fail  : bool, // Indication of unification failure
  mode  : Mode, // In Write mode, new elements are built on the heap.

  // Memory Stores
  heap: Vec<Cell>, // The "global stack," a memory store

  // Registers //
  hp        : usize,          // Heap Pointer, a cursor
  rp        : usize,          // Register Pointer, a cursor
  registers : Vec<Cell>, // Registers

  #[cfg(feature = "trace_computation")]
  token_stack: Vec<Token>,
  #[cfg(feature = "trace_computation")]
  current_token: usize
}

impl WVM {

  // region Display methods

  /**
    Gives the possibly intermediate results of unification for the query living at `X[1]`.
  */
  fn memory_to_term(&self, address: &Address) -> Term{

    let data_address = self.dereference(address);
    let cell = self.value_at(&data_address);
    match cell {

      | Cell::Functor(_)
      | Cell::STR(_) => {
        let functor_address = match self.extract_address(&cell) {
          Some(add) => add,
          None => data_address
        };
        let functor = self.extract_functor(&data_address).unwrap();
        let mut args = TermVec::with_capacity((functor.arity) as usize);
        for i in 1..functor.arity+1 {
          // It is possible that not all arguments have been constructed yet.
          if !((functor_address + i as usize).idx() < self.heap.len()) { break; }
          args.push(Rc::new(self.memory_to_term(&(functor_address + i as usize))));
        }

        Term::Structure {
          functor,
          args
        }
      }

      Cell::REF(var_address) if var_address == data_address => {
        // A variable
        Term::Variable(DefaultAtom::from(format!("V{}", var_address.idx())))
      }

      _ => {
        panic!("Error: Could not construct a term from the cell at {}", data_address);
      }

    }
  }

  fn make_register_table<T> (
      name      : &str,
      registers : &[T],
      highlight : usize,
      start     : usize
    ) -> Table
    where T: Display{
    let mut table = Table::new();

    table.set_format(*TABLE_DISPLAY_FORMAT);
    table.set_titles(row![ubr->"Address", ubl->"Contents"]);

    for (i, cell) in registers.iter().enumerate() {
      match i == highlight{

        true => {
          table.add_row(row![r->format!("* --> {}[{}] =", name, i+start), format!("{}", cell)]);
        }
        false => {
          table.add_row(
            row![r->format!("{}[{}] =", name, i+start), format!("{}", cell)]
          );
      }

      } // end match on highlight
    } // end for
    table
  }

  // endregion

  // region Low-level utility methods
  
  pub fn new() -> WVM {
    WVM {
      fail :  false,
      mode        :  Mode::Read, // Arbitrarily chosen.

      heap        :  vec![],
      registers   :  vec![],

      hp:  0,
      rp:  0,

      #[cfg(feature = "trace_computation")]
      token_stack: vec![],
      #[cfg(feature = "trace_computation")]
      current_token: MAX,

    }
  }

  /// Extracts the functor from either `Cell::Structure` or `Cell::STR` values.
  //  This function can't be in `crate::cell`, because it uses `self.value_at`.
  fn extract_functor(&self, address: &Address) -> Option<Functor>{
    match &self.value_at(address) {

      Cell::Functor(functor)   => Some(functor.clone()),
      Cell::STR(inner_address) => self.extract_functor(inner_address),
      _                        => None

    }
  }

  /// Extracts the address from either `Cell::REF` or `Cell::STR` values.
  //  This function isn't in `crate::cell`, because `extract_functor` can't be.
  fn extract_address(&self, cell: &Cell) -> Option<Address>{
    match cell {
      | Cell::REF(address)
      | Cell::STR(address) => Some(*address),
      _ => None
    }
  }

  /// Performs one step of `dereference`, what C programmers think of as dereferencing.
  fn value_at(&self, ptr: &Address) -> Cell{
    match ptr {
      Address::Heap(_)  => self.heap[ptr.idx()].clone(),
      Address::Register(_)   => self.registers[ptr.idx()].clone()
    }
  }

  /**
    Sets the value at the given address, dynamically growing the relevant vector if the address
    is larger than the max index of the vector.

    See `set_value` for differences between the two functions.
  */
  fn set_value_at(&mut self, address: &Address, cell: &Cell){
    match address {

      Address::Register(_) => {
        if address.idx() >= self.registers.len() {
          self.registers.resize(address.idx() + 1, Cell::Empty);
        }
        self.registers[address.idx()] = cell.clone();
      },

      Address::Heap(_) => {
        if address.idx() >= self.heap.len() {
          self.heap.resize(address.idx() + 1, Cell::Empty);
        }
        self.heap[address.idx()] = cell.clone();
      }

    }
  }

  // endregion

  // region Compilation/Interpretation

  /**
    Accepts a program or query as a string and turns it into
    a sequences of operations or an in-memory representation.

    The compilation pipeline is this:
    ```
    text -> [`parser::parse`] -> `Term`s ->⋯

        ┌───────────────[`token::flatten_term`]────────────────┐
    ⋯->*│*-> [`TermIter`] -> `Cell`s -> [`order_registers`] ->*│*->⋯
        └──────────────────────────────────────────────────────┘

    ⋯-> `Token`s -> [`compile_tokens`] -> `Cell`s/instructions ->⋯

    ⋯-> [`unify`] -> Success/Fail
    ```
    The [`unify`] step is part of `compile_tokens` and interprets the
    instructions to build the in-memory `Cell`s. The conversion to
    `Cell`s, then `Token`s, and back to `Cell`s again is for two reasons:

     1. Because the term needs to be flattened and reordered, we would
        have the "conversion" step no matter what.

     2. Conversion from AST to `Cell`s allows us to build in-memory
        representations directly "by hand" should we so desire.

  */
  pub fn compile(&mut self, text: &str){
    match text.starts_with("?-") {

      true => {
        let tokenizer = Tokenizer::new_query(&text[2..]);
        self.compile_tokens(tokenizer, false);
      }

      false => {
        let tokenizer = Tokenizer::new_program(text);
        self.compile_tokens(tokenizer, true);
      }

    }
  }

  /**
    Compiles a flattened query term into its in-memory representation.

    This function assumes that the term has been prepared with `flatten_term`.
  */
  fn compile_tokens(&mut self, tokenizer: Tokenizer, is_program: bool){
    // Contains the register arguments we've seen before.
    let mut seen: HashSet<Address> = HashSet::new();
    self.rp = 0;

    #[cfg(feature = "trace_computation")]
      {
        let tokenizer_copy = tokenizer.clone();
        self.token_stack = tokenizer_copy.collect();
        self.current_token = 1;
        println!("{}", self);
        self.current_token = 0;
      }

    // We iterate over the tokens in the registers in `order`.
    for token in tokenizer {
      match token {

        Token::Assignment(functor, address) => {
          seen.insert(address);
          match is_program {

            true => {  // Program
              self.get_structure(&functor, &address);
            }

            false => { // Query
              self.put_structure(&functor, &address);
            }

          }

        }

        Token::Register(address) => {

          match seen.contains(&address) {

            true => {
              // Already saw this register.
              match is_program {

                true => {  // Program
                  self.unify_value(&address);
                }

                false => { // Query
                  self.set_value(&address);
                }

              }
            }

            false => {
              // Have not seen this register before.
              seen.insert(address);
              match is_program {

                true => {  // Program
                  self.unify_variable(&address);
                }

                false => { // Query
                  self.set_variable(&address);
                }

              }
            }

          } // end if seen address before

        }
      } // end match on token type
      #[cfg(feature = "trace_computation")]
      {
        self.current_token += 1;
        println!("{}", self);
      }

      if self.fail {
        break;
      };
    } // end iterate over tokens
  }

  // endregion  Compilation/Interpretation

  // region VM instruction methods

  /// Dereference a chain of references. Returns either the address of a variable (a
  /// self-reference), an `STR`, or a functor.
  fn dereference(& self, ptr: &Address) -> Address{
    let cell = self.value_at(ptr);
    match cell{

      // Do not dereference variables, which reference themselves.
      Cell::REF(a) if a != *ptr => self.dereference(&a),

      _                         => *ptr,

    }
  }

  /**
    Binds an unbound variable at one address to the other address. If both are unbound, the first
     is bound to the second (arbitrarily).

     address1: heap address
     address2: register address
  */
  fn bind(&mut self, address1: &Address, address2: &Address){
    #[cfg(feature = "trace_computation")] print!("bind({}, {}): ", address1, address2);

    let cell1 = self.value_at(address1);
    let cell2 = self.value_at(address2);

    match (&cell1, &cell2){

      // There are four branches instead of two in order to prefer binding a variable in a
      // register to a variable on the heap over the reverse.
      (Cell::REF(address), _) if address1 == address && !address1.is_register() => {
        // `cell1` is a register variable. Bind to cell2.
        #[cfg(feature = "trace_computation")] println!("Binding {} to {}", address1, address2);
        self.set_value_at(&address1, &Cell::REF(*address2));
      }

      (_, Cell::REF(address)) if address2 == address && !address2.is_register() => {
        // `cell2` is a register variable. Bind to cell1.
        #[cfg(feature = "trace_computation")] println!("Binding {} to {}", address2, address1);
        self.set_value_at(&address2, &Cell::REF(*address1));
      }

      (Cell::REF(address), _) if address1 == address => {
        // `cell1` is a variable. Bind to cell2.
        #[cfg(feature = "trace_computation")] println!("Binding {} to {}", address1, address2);
        self.set_value_at(&address1, &Cell::REF(*address2));
      }

      (_, Cell::REF(address)) if address2 == address => {
        // `cell2` is a variable. Bind to cell1.
        #[cfg(feature = "trace_computation")] println!("Binding {} to {}", address2, address1);
        self.set_value_at(&address2, &Cell::REF(*address1));
      }

      _ => {
        // Neither `cell1` nor `cell2` are variables, an error state.
        unreachable!("Unreachable: bind called without a variable. Found:\n\t{}\n\t{}",
                     cell1, cell2);
      }
    }
  }

  /**
    Push a new `STR` (and adjoining functor) cell onto the heap and copy that cell into the
    allocated register address.

    Note: `structure` is not statically checked to be a `Cell:Structure`.
          Callers must ensure this contract is maintained on their own.
  */
  fn put_structure(& mut self, functor: &Functor, address: &Address) {
    address.require_register();
    #[cfg(feature = "trace_computation")] println!("PutStructure({}, {})", functor, address);

    let cell = Cell::STR(Address::from_heap_idx(self.heap.len() + 1));
    self.heap.push(cell.clone());
    self.heap.push(Cell::Functor(functor.clone()));
    self.set_value_at(address, &cell);
  }

  /**
    Push a new `REF` cell onto the heap containing its own address, and copy it into the given
    register.

    Note: `ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn set_variable(&mut self, address: &Address) {
    address.require_register();

    #[cfg(feature = "trace_computation")]
    println!("set_variable({})", address);

    let cell = Cell::REF( Address::from_heap_idx( self.heap.len()) );
    self.heap.push(cell.clone());
    self.set_value_at(address, &cell);
  }

  /**
    Push the value of the given register onto the heap.

    Note: This differs from `set_value_at` in the following ways:
          1. The given address is the origin rather than the target of the write.
          2. A new cell is created on the top of the `HEAP`.
          3. The data is always written to the cell at the top of the `HEAP`.
          4. The pointer type is guarded.
  */
  fn set_value(&mut self, reg_ptr: &Address) {
    reg_ptr.require_register();

    #[cfg(feature = "trace_computation")]
    println!("set_value({})", reg_ptr);

    self.heap.push(self.value_at(reg_ptr));
  }

  /**
    Either matches a functor, binds a variable to a new functor value, or fails.

    Note: `register_address` must be a pointer to a register, but this contract is not statically checked.
  */
  fn get_structure(&mut self, functor: &Functor, address: &Address){
    address.require_register();

    let target_address = self.dereference(address);
    let target_cell    = self.value_at(&target_address);

    match target_cell {

      Cell::REF(_) => {
        // A variable. Create a new functor structure for `functor` on the stack, bind the
        // variable to the functor, and set `mode` to `Mode::Write`.
        #[cfg(feature = "trace_computation")]
          println!("GetStructure({}, {}): creating struct", functor, address);

        let functor_idx = self.heap.len() + 1;
        let functor_address = Address::from_heap_idx(functor_idx);
        let cell            = Cell::STR(functor_address);
        self.heap.push(cell);
        self.heap.push(Cell::Functor(functor.clone()));

        // Remember that we want to bind to the `STR`, not the `f/n`, so subtract 1..
        self.bind(&target_address, &Address::from_heap_idx(functor_idx - 1));

        self.mode           = Mode::Write;
      },

      Cell::STR(heap_address @ Address::Heap(_)) => {
        // A pointer to a functor.
        if self.heap[heap_address.idx()] == Cell::Functor(functor.clone()) {
          #[cfg(feature = "trace_computation")]
            println!("get_structure({}, {}): functor already on stack", functor, address);

          self.hp   = heap_address.idx() + 1;
          self.mode = Mode::Read;

        } else{
          #[cfg(feature = "trace_computation")]
            println!("get_structure({}, {}): STR points to different functor", functor, address);
          self.fail = true;
        }
      }

      _c => {
        // This is an error condition that should not happen in correct programs.
        #[cfg(feature = "trace_computation")]
          println!("GetStructure({}, {}): neither REF nor STR found: {}",
                   functor, address, _c);
        self.fail = true;
      }
    };
  }

  /**
    Either reads the top of the `HEAP` into register `X[i]` or creates a variable on the `HEAP`
    and copies it to register X[i}.

    Note: `ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn unify_variable(&mut self, address: &Address){
    address.require_register();

    match self.mode {

      Mode::Read  => {
        #[cfg(feature = "trace_computation")]
          println!("UnifyVariable({}):  {} <- H[S={}]", address, address, self.hp);

        let value = self.heap[self.hp].clone();
        self.set_value_at(address, &value);
      }

      Mode::Write => {
        #[cfg(feature = "trace_computation")]
        print!("unify_variable({}):  ", address);

        self.set_variable(address);
      }

    } // end match mode

    self.hp += 1;
  }

  /**
    Either pushes the value of `X[i]` onto the `HEAP` (write) or unifies `X[i]` and the cell at `S`.

    Note: `address` must be a register pointer, but this contract is not statically checked.
  */
  fn unify_value(&mut self, address: &Address){
    address.require_register();

    match self.mode {

      Mode::Read  => {
        #[cfg(feature = "trace_computation")] println!("UnifyValue({}):  unifying", address);
        self.unify(address, &Address::from_heap_idx(self.hp));
      }

      Mode::Write => {
        #[cfg(feature = "trace_computation")] print!("UnifyValue({}):  ", address);
        self.set_value(address);
      }
    }

    self.hp += 1;
  }

  fn unify(&mut self, a1: &Address, a2: &Address){
    // In [Warren] the PDL (push down list) is global.
    let mut pdl: Vec<Address> = Vec::new();

    pdl.push(a1.clone());
    pdl.push(a2.clone());
    self.fail = false;
    while !(pdl.is_empty() || self.fail){
      let b1 = pdl.pop().unwrap();
      let b1 = &self.dereference(&b1);
      let b2 = pdl.pop().unwrap();
      let b2 = &self.dereference(&b2);
      let c1 = self.value_at(b1);
      let c2 = self.value_at(b2);
      if c1 != c2 {
        match (&c1, &c2){

          | (Cell::REF(_),      _      )
          | (      _     , Cell::REF(_)) => {
            // One of `d1` and `d2` is a variable, and the other is either a variable or a
            // `Cell::STR` because of how functors are created on the `HEAP`.
            self.bind(b1, b2);
          },

          _                              => {
            // Neither `d1` nor `d2` are variables. In fact, we know that one of `d1` and `d2` is
            // a `Cell::STR` because of how functors are created on the `HEAP`.
            // println!("d1: {}\nd2: {}", d1, d2);
            let f1 = self.extract_functor(b1);
            let f2 = self.extract_functor(b2);
            if f1 == f2{
              // Since one of `d1` and `d2` is a `Cell::STR`, they both are.
              let v1 = self.extract_address(&c1).unwrap();
              let v2 = self.extract_address(&c2).unwrap();
              for n in 1..f1.unwrap().arity{
                pdl.push(v1 + n as usize);
                pdl.push(v2 + n as usize);
              }
            } else {
              self.fail = true;
              break;
            }
          }

        }
      }
    }
  }
  // endregion

}


lazy_static! {
  static ref TABLE_DISPLAY_FORMAT: TableFormat::TableFormat =
    TableFormat::FormatBuilder::new()
      .column_separator('│')
      .borders(' ')
      .separator(
        TableFormat::LinePosition::Title,
        TableFormat::LineSeparator::new('─', '┼', ' ', ' ')
      )
      .padding(1, 1)
      .build();
}

impl Display for WVM {

  // We print the token_stack if `trace_computation` is on.
  #[cfg(feature = "trace_computation")]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WVM::make_register_table("HEAP", &self.heap, self.hp, 0);
    let x_table = WVM::make_register_table("X", &self.registers, self.rp, 1);
    let token_table = WVM::make_register_table("Y", &self.token_stack, self.current_token - 1, 1);

    let mut combined_table = table!([h_table, x_table, token_table]);
    combined_table.set_titles(row![ub->"Heap", ub->"Registers", ub->"Token Stack"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    if self.registers.is_empty() ||
      (self.value_at(&Address::Register(1)) == Cell::Empty)
    {
      write!(f, "{}", combined_table)
    } else {
      let term = self.memory_to_term(&Address::Register(1));
      write!(f, "{}\n{}\n", combined_table, term.as_expression_string())
    }
  }

  #[cfg(not(feature = "trace_computation"))]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WVM::make_register_table("HEAP", &self.heap, self.hp, 0);
    let x_table = WVM::make_register_table("X", &self.registers, self.rp, 1);

    let mut combined_table = table!([h_table, x_table]);
    combined_table.set_titles(row![ub->"Heap", ub->"Registers"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    if self.registers.is_empty() ||
      (self.value_at(&Address::Register(1)) == Cell::Empty)
    {
      write!(f, "{}", combined_table)
    } else {
      let term = self.memory_to_term(&Address::Register(1));
      write!(f, "{}\n{}\n", combined_table, term.as_expression_string())
    }
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Mode{
  Read,
  Write
}

impl Display for Mode{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Mode::Read => {
        write!(f, "Read")
      },
      Mode::Write => {
        write!(f, "Write")
      }
    }
  }
}
