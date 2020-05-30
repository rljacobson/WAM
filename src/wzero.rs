//! Structures and functions for the Warren Abstract Machine
#![allow(non_snake_case)]
// #![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

use prettytable::{format as TableFormat, Table};

use crate::address::Address;
use crate::cell::{Cell, CellVec};
use crate::functor::Functor;
use crate::token::*;
use std::usize::MAX;

type RcRefCell<T> = Rc<RefCell<T>>;

#[allow(non_snake_case)]
pub struct WZero{
  /// Flag indicating unification failure.
  fail : bool,
  /// Read or Write mode.
  mode : Mode,
  /// A pointer to the next HEAP position to be read; a cursor.
  S    : usize,
  /// A pointer to the current register.
  T    : usize,
  /// The "global stack," a memory store.
  HEAP : Vec<Cell>,
  /// Query Registers
  X    : RcRefCell<Vec<Cell>>,
  #[cfg(feature = "trace_computation")]
  token_stack: CellVec,
  #[cfg(feature = "trace_computation")]
  current_token: usize
}

impl WZero {

  // region Display methods

  fn  make_register_table<T>(name: char, registers: &Vec<T>, highlight: Option<usize>, start: usize)
    -> Table
    where T: Display{
    let mut table = Table::new();

    table.set_format(*TABLE_DISPLAY_FORMAT);
    table.set_titles(row![ubr->"Address", ubl->"Contents"]);

    for (i, cell) in registers.iter().enumerate() {
      match Some(i)== highlight{
        true => {
          table.add_row(row![r->format!("* --> {}[{}] =", name, i+start), format!("{}", cell)]);
        }
        false => {
          table.add_row(row![r->format!("{}[{}] =", name, i+start), format!("{}", cell)]);
        }
      }

    }
    table
  }

  // endregion

  // region Low-level utility methods
  
  pub fn new() ->WZero {
    WZero{
      fail :  false,
      mode :  Mode::Write,
      S    :  0,
      T    :  0,
      HEAP :  vec![],
      X    :  Rc::new(RefCell::new(vec![])),
      #[cfg(feature = "trace_computation")]
      token_stack: Rc::new(vec![]),
      #[cfg(feature = "trace_computation")]
      current_token: MAX,
    }
  }

  /// Extracts the functor from either `Cell::Structure` or `Cell::STR` values.
  //  This function can't be in `crate::cell`, because it uses `self.value_at`.
  fn extract_functor(&self, address: &Address) -> Option<Functor>{
    match &self.value_at(address) {
      Cell::Functor(functor)  => Some(*functor),
      Cell::STR(inner_address)        => {
        self.extract_functor( inner_address )
      }
      _ => {
        None
      }
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
      Address::CellPtr(_)  => self.HEAP[ptr.idx()].clone(),
      Address::RegPtr(_)   => self.X.deref().borrow()[ptr.idx()].clone()
    }
  }

  /**
    Sets the value at the given address, dynamically growing the relevant vector if the address
    is larger than the max index of the vector.

    See `set_value` for differences between the two functions.
  */
  fn set_value_at(&mut self, address: &Address, cell: &Cell){
    match address {

      Address::RegPtr(_) => {
        let mut x_ref = self.X.deref().borrow_mut();
        if address.idx() >= x_ref.len() {
          x_ref.resize(address.idx() + 1, Cell::Empty);
        }
        x_ref[address.idx()] = cell.clone();
      },

      Address::CellPtr(_) => {
        if address.idx() >= self.HEAP.len() {
          self.HEAP.resize(address.idx() + 1, Cell::Empty);
        }
        self.HEAP[address.idx()] = cell.clone();
      }

    }
  }

  // endregion

  // region Compilation/Interpretation

  /**
    Accepts a program or query as a string and turns it into
    a sequences of operations or an in-memory representation.

    The compilation pipeline is this:

      text -> [`parser::parse`] -> `Term`s ->⋯

          ┌───────────────[`token::flatten_term`]────────────────┐
      ⋯->*│*-> [`TermIter`] -> `Cell`s -> [`order_registers`] ->*│*->⋯
          └──────────────────────────────────────────────────────┘

      ⋯-> `Token`s -> [`compile_tokens`] -> `Cell`s/instructions ->⋯

      ⋯-> [`unify`] -> Success/Fail

    The [`unify`] step is part of `compile_tokens` and interprets the
    instructions to build the in-memory `Cell`s. The conversion to
    `Cell`s, then `Token`s, and back to `Cell`s again is for two reasons:

     1. Because the term needs to be flattened and reordered, we would
        have the "conversion" step no matter what.

     2. Conversion from AST to `Cell`s allows us to
        build in-memory representations directly "by hand."

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

    #[cfg(feature = "trace_computation")]
      {
        self.token_stack = Rc::new(
          tokenizer.order
                   .iter()
                   .map( |a| {tokenizer.cell_vec[a.idx()].clone()})
                   .collect()
        );
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

          #[cfg(feature = "trace_computation")]
          {
            self.current_token += 1;
          }
          self.T = address.idx();

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
      println!("{}", self);
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
      Cell::REF(a) if a != *ptr =>
        self.dereference(&a),
      _ => *ptr,
    } // end match cell
  }

  /**
    Binds an unbound variable at one address to the other address. If both are unbound, the first
     is bound to the second (arbitrarily).
  */
  fn bind(&mut self, add1: &Address, add2: &Address){
    #[cfg(feature = "trace_computation")]
    print!("bind({}, {}): ", add1, add2);
    let cell1 = self.value_at(add1);
    let cell2 = self.value_at(add2);

    match (cell1, cell2){
      // There are four branches instead of two in order to prefer binding a variable in a
      // register to a variable on the heap over the reverse.
      (Cell::REF(add), _) if *add1 == add && add1.is_register() => {
        // `cell1` is a register variable. Bind to cell2.
        #[cfg(feature = "trace_computation")]
        println!("Binding {} to {}", add1, add2);
        self.set_value_at(&add1, &Cell::REF(*add2));
      },
      (_, Cell::REF(add)) if *add2 == add && add2.is_register() => {
        // `cell2` is a register variable. Bind to cell1.
        #[cfg(feature = "trace_computation")]
        println!("Binding {} to {}", add2, add1);
        self.set_value_at(&add2, &Cell::REF(*add1));
      },
      (Cell::REF(add), _) if *add1 == add => {
        // `cell1` is a variable. Bind to cell2.
        #[cfg(feature = "trace_computation")]
        println!("Binding {} to {}", add1, add2);
        self.set_value_at(&add1, &Cell::REF(*add2));
      },
      (_, Cell::REF(add)) if *add2 == add => {
        // `cell2` is a variable. Bind to cell1.
        #[cfg(feature = "trace_computation")]
        println!("Binding {} to {}", add2, add1);
        self.set_value_at(&add2, &Cell::REF(*add1));
      },
      _ => {
        // Neither `cell1` nor `cell2` are variables, an error state.
        unreachable!("Unreachable: bind called without a variable.");
      }
    }
  }

  /**
    Push a new `STR` (and adjoining functor) cell onto the heap and copy that cell into the
    allocated register address.

    Note: `structure` is not statically checked to be a `Cell:Structure`.
          Callers must ensure this contract is maintained on their own.
  */
  fn put_structure(& mut self, functor: &Functor, reg_ptr: &Address) {
    reg_ptr.require_register();
    #[cfg(feature = "trace_computation")]
    println!("put_structure({}, {})", functor, reg_ptr);
    let cell = Cell::STR(Address::from_heap_idx(self.HEAP.len() + 1));
    self.HEAP.push(cell.clone());
    self.HEAP.push(Cell::Functor(*functor));

    self.set_value_at(reg_ptr, &cell);
  }

  /**
    Push a new `REF` cell onto the heap containing its own address, and copy it into the given
    register.

    Note: `ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn set_variable(&mut self, reg_ptr: &Address) {
    reg_ptr.require_register();
    #[cfg(feature = "trace_computation")]
    println!("set_variable({})", reg_ptr);
    let cell = Cell::REF( Address::from_heap_idx( self.HEAP.len()) );
    self.HEAP.push(cell.clone());
    self.set_value_at(reg_ptr, &cell);
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
    self.HEAP.push(self.value_at(reg_ptr));
  }

  /**
    Either matches a functor, binds a variable to a new functor value, or fails.

    Note: `reg_ptr` must be a pointer to a register, but this contract is not statically checked.
  */
  fn get_structure(&mut self, funct: &Functor, reg_ptr: &Address){
    reg_ptr.require_register();

    let address = self.dereference(reg_ptr);
    let cell_at_add = self.value_at(&address);
    match cell_at_add {
      Cell::REF(_) => {
        // A variable. Create a new functor structure for `funct` on the stack and bind the
        // variable to the functor.
        #[cfg(feature = "trace_computation")]
        println!("get_structure({}, {}): creating struct", funct, reg_ptr);
        let funct_add = Address::from_heap_idx(self.HEAP.len() + 1);
        let cell = Cell::STR(funct_add);
        self.HEAP.push(cell.clone());
        self.HEAP.push(Cell::Functor(*funct));
        self.bind(&address, &funct_add);
        self.mode = Mode::Write;
      },
      Cell::STR(cell_ptr @ Address::CellPtr(_)) => {
        // A pointer to a functor.
        if self.HEAP[cell_ptr.idx()] == Cell::Functor(*funct) {
          #[cfg(feature = "trace_computation")]
          println!("get_structure({}, {}): functor already on stack", funct, reg_ptr);
          self.S = cell_ptr.idx() + 1;
          self.mode = Mode::Read;
        } else{
          // Failed to unify
          #[cfg(feature = "trace_computation")]
          println!("get_structure({}, {}) - STR points to nonexistent functor", funct, reg_ptr);
          self.fail = true;
        }
      },
      _ => {
        // Failed to unify
        #[cfg(feature = "trace_computation")]
        println!("get_structure({}, {}) - neither REF nor STR found: {}", funct, reg_ptr, address);
        self.fail = true;
      }
    };
  }

  /**
    Either reads the top of the `HEAP` into register `X[i]` or creates a variable on the `HEAP`
    and assigns it to register X[i}.

    Note: `ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn unify_variable(&mut self, reg_ptr: &Address){
    reg_ptr.require_register();

    match self.mode {
      Mode::Read => {
        #[cfg(feature = "trace_computation")]
        println!("unify_variable({}):  {} <- H[S={}]", reg_ptr, reg_ptr, self.S);
        // ToDo: Extra copy here:
        let value = &self.HEAP[self.S].clone();
        self.set_value_at(reg_ptr, value);
      } // end if Mode::Read
      Mode::Write => {
        #[cfg(feature = "trace_computation")]
        print!("unify_variable({}):  ", reg_ptr);
        self.set_variable(reg_ptr)
      } // end if Mode::Write
    } // end match mode

    self.S += 1;
  }

  /**
    Either pushes the value of `X[i]` onto the `HEAP` (write) or unifies `X[i]` and the cell at `S`.

    Note: `reg_ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn unify_value(&mut self, reg_ptr: &Address){
    reg_ptr.require_register();

    match self.mode {
      Mode::Read => {
        #[cfg(feature = "trace_computation")]
        println!("unify_value({}):  unifying", reg_ptr);
        self.unify(*reg_ptr, Address::from_heap_idx(self.S));
      }
      Mode::Write => {
        #[cfg(feature = "trace_computation")]
        print!("unify_value({}):  ", reg_ptr);
        self.set_value(reg_ptr);
      }
    }

    self.S += 1;
  }

  fn unify(&mut self, a1: Address, a2: Address){
    let mut PDL: Vec<Address> = Vec::new();

    PDL.push(a1);
    PDL.push(a2);
    self.fail = false;
    while !(PDL.is_empty() || self.fail){
      let b1 = PDL.pop().unwrap();
      let b1 = &self.dereference(&b1);
      let b2 = PDL.pop().unwrap();
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
          _ => {
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
                PDL.push(v1 + n.into());
                PDL.push(v2 + n.into());
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
    .separator(
      TableFormat::LinePosition::Bottom,
      TableFormat::LineSeparator::new('─', '┴', ' ', ' ')
    )
    .padding(1, 1)
    .build();
}

impl Display for WZero{

  // We print the token_stack if `trace_computation` is on.
  #[cfg(feature = "trace_computation")]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WZero::make_register_table('H', &self.HEAP, Some(self.S), 0);
    let x_table = WZero::make_register_table('X', &self.X.deref().borrow(),Some (self.T), 1);

    let token_table = WZero::make_register_table('Y', &self.token_stack, Some(self.current_token - 1
    ), 1);
    let mut combined_table = table!([h_table, x_table, token_table]);
    combined_table.set_titles(row![ub->"Heap", ub->"Registers", ub->"Token Stack"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    let success = match self.fail{
      true => "Failed to unify.",
      false => "Unifying successfully."
    };

    write!(f, "Mode: {}\n{}\n{}", self.mode, success, combined_table)
  }

  #[cfg(not(feature = "trace_computation"))]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WZero::make_register_table('H', &self.HEAP, Some(self.S), 0);
    let x_table = WZero::make_register_table('X', &self.X.deref().borrow(),Some (self.T), 1);

    let mut combined_table = table!([h_table, x_table]);
    combined_table.set_titles(row![ub->"Heap", ub->"Registers"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    let success = match self.fail{
      true => "Failed to unify.",
      false => "Unifying successfully."
    };

    write!(f, "Mode: {}\t{}\n{}", self.mode, success, combined_table)
  }
}

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
