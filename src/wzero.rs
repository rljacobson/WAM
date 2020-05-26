//! Structures and functions for the Warren Abstract Machine

#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ops::Deref;

use prettytable::{format as TableFormat, Table};
use either::Either;

use crate::cell::Cell;
use crate::address::Address;
use crate::term::{Term, TermIter, make_struct};
use crate::functor::Functor;
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;

type CellOrVec = Either<Cell, Vec<Cell>>;
type RcRefCell<T> = Rc<RefCell<T>>;
const PLACEHOLDER: CellOrVec = CellOrVec::Left(Cell::Empty);

#[allow(non_snake_case)]
pub struct WZero{
  /// Flag indicating unification failure.
  fail : bool,
  /// Read or Write mode.
  mode : Mode,
  /// A pointer to the next HEAP position to be read; a cursor.
  S    : usize,
  T    : usize,
  /// The "global stack," a memory store.
  HEAP : Vec<Cell>,
  /// Query Registers
  X    : Vec<Cell>,
  /// Program Registers
  Y    : Vec<Cell>,
}

impl WZero {

  // region Display methods

  fn  make_register_table(name: char, registers: &Vec<Cell>, highlight: Option<usize>) -> Table{
    let mut table = Table::new();

    table.set_format(*TABLE_DISPLAY_FORMAT);
    table.set_titles(row![ubr->"Address", ubl->"Contents"]);

    for (i, cell) in registers.iter().enumerate() {
      match Some(i)== highlight{
        true => {
          table.add_row(row![r->format!("* --> {}[{}] =", name, i+1), format!("{}", cell)]);
        }
        false => {
          table.add_row(row![r->format!("{}[{}] =", name, i+1), format!("{}", cell)]);
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
      X    :  vec![],
      Y    :  vec![],
    }
  }

  /// Extracts the functor from either `Cell::Structure` or `Cell::STR` values.
  //  This function can't be in `crate::cell`, because it uses `self.value_at`.
  fn extract_functor(&self, cell: &Cell) -> Option<Functor>{
    match cell {
      Cell::Functor(functor)  => Some(*functor),
      Cell::STR(address)        => {
        self.extract_functor( &self.value_at(address) )
      },
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
      Address::RegPtr(_)   => self.X[ptr.idx()].clone()
    }
  }

  /**
    Sets the value at the given address, which address is assumed to exist.

    See `set_value` for differences between the two functions.
  */
  fn set_value_at(&mut self, ptr: &Address, value: &Cell){
    match ptr {
      Address::RegPtr(_) => {
        self.X[ptr.idx()] = value.clone();
      },
      Address::CellPtr(_) => {
        self.HEAP[ptr.idx()] = value.clone();
      }
    }
  }

  // endregion

  // region Compilation/Interpretation

  pub fn compile(&mut self, ast: &Term){
    let mut is_program: bool;
    // The first term determines whether we are compiling a Query or a Program.
    let term = match ast {
      Term::Program(p) => {
        is_program = true;
        p
      },
      Term::Query(q)   => {
        is_program = false;
        q
      },
      _t                     => {
        panic!("Error: Compile takes either a query or a program but was given {}.", _t);
      }
    };

    let mut term_iter = term.deref().borrow_mut().iter();

    // STEP 1: Flatten the term.
    let flattened: RcRefCell<HashMap<Address, CellOrVec>> = Self::flatten_term(&mut term_iter);
    #[cfg(debug_print)]
    {
      println!("Flattened:", );
      for (a, c) in flattened.deref().borrow().iter(){
        match c{
          Either::Left(v) => {
            println!("\t{}: {}", a, v);
          }
          Either::Right(v) => {
            print!("\t{}: ", a);
            for c in v{
              print!(" {}", c);
            }
            println!();
          }
        }
      }
    }

    // The registers are not filled in order, so we need to allocate enough cells.
    let new_size = flattened.deref().borrow().len();
    self.X.resize_with(new_size, | | {Cell::Empty});
    self.Y.resize_with(new_size, | | {Cell::Empty});

    // STEP 2: Order the registers
    let order = Self::order_registers(flattened.clone());
    {
      let f_ref = flattened.deref().borrow();
      for c in order.deref().borrow().iter() {
        self.Y[c.idx()] = match f_ref.get(c) {
          None => Cell::Empty,
          Some(Either::Left(cell)) => {
            cell.clone()
          }
          Some(Either::Right(cell_vec)) => {
            Cell::String(cell_vec.clone())
          }
        }
      }
    }

    // STEP 3: Emit or interpret code.
    match is_program {
      true => {
        order.deref().borrow_mut().reverse();
        #[cfg(debug_print)]
          {
            print!("Flattened order: ");
            for a in order.deref().borrow().iter(){
              print!("{} ", a);
            }
            println!();
          }
        self.compile_flat_term(flattened.clone(), order.deref().borrow().as_ref(), &PROGRAM_OPS);
      }
      false => {
        #[cfg(debug_print)]
        {
          print!("Flattened with order: ");
          for a in order.deref().borrow().iter(){
            print!("{} ", a);
          }
          println!();
        }

        self.compile_flat_term(flattened.clone(), order.deref().borrow().as_ref(), &QUERY_OPS);
      }
    }
  }

  /**
    Computes the flattened form expressed in registers of the given Term AST. The flattened form
    of the AST is the non-variable registers.

    Example:
      The registers containing the flattened form of `p(Z, h(Z, W), f(W))` are
        ```
        X1 = p(X2, X3, X4)
        X2 = Z
        X3 = h(X2, X5)
        X4 = f(X5)
        X5 = W
        ```
    Unfortunately, we can't simultaneously compile the term, because the flattened form needs to
    be ordered in a particular way.
  */

  pub fn flatten_term(term_iter: &mut TermIter) -> RcRefCell<HashMap<Address, CellOrVec>>{
    let mut seen: HashMap<Term, (Address, CellOrVec)> = HashMap::new();
    let mut current_ptr: Address;
    /*
      STEP 1: Flatten the term.

      While we flatten the term, we also tokenize it into a form that can be placed directly into
      cells. To flatten, we iterate over two levels:
        1. The registers, and
        2. The `args` of `Term::Structure`s.
      We do the same thing to both:
        * If it's not already seen, add it.
        * If it is already seen, "replace" the current thing with the cell.

      This method erases variable names.
    */
    for term in term_iter{
      match seen.get(&term) {
        None => {
          // Save an address for this term.
          current_ptr = Address::RegPtr(seen.len() + 1);
          seen.insert(
            term.clone(),
            (current_ptr, PLACEHOLDER)
          );
        }
        Some((address, PLACEHOLDER)) => {
          // Just a placeholder to hold the address. We still need to process this term.
          current_ptr = *address;
        }
        _ => {
          // Already processed
          continue;
        }
      }

      match &term{
        Term::Structure {functor, args} => {
          let mut struct_vec: Vec<Cell> = Vec::new();
          let args_ref = args.deref().borrow();

          struct_vec.reserve(args_ref.len() + 1);
          struct_vec.push(Cell::Functor(*functor)); // the `+ 1`

          for arg in args_ref.deref(){
            // Occurs Check would go here.
            match seen.get(arg){
              Some((address, _)) =>{
                struct_vec.push(Cell::REF(*address));
              }
              None => {
                // Insert a placeholder for arg in order to give it an address.
                let new_address = Address::RegPtr(seen.len() + 1);
                seen.insert(
                  arg.clone(),
                  (new_address, PLACEHOLDER.clone())
                );
                struct_vec.push(Cell::REF(new_address));
              }
            }
          }
          seen.insert(term.clone(), (current_ptr, Either::Right(struct_vec)));
        },
        Term::Variable(_) => {
          seen.insert(
            term,
            (current_ptr, Either::Left(Cell::REF(current_ptr)))
          );
        }
        _t => {
          // Either Register or Empty. Neither should happen.
          seen.insert(
            term.clone(),
            (current_ptr, Either::Left(Cell::Term(term)))
          );
        }
      }// end match term variant
    } // end iterate over terms

    Rc::new( RefCell::new(HashMap::from_iter(seen.values().cloned())))
  }

  /*
  fn flatten_term_old(&mut self, ast: &Term){
    let mut seen: HashMap<Term, Address> = HashMap::new();

    //  We visit the AST breadth first, adding new symbols to `seen` as we go.
    let terms = ast.iter();
    for term in terms{
      if !seen.contains_key(&term){
        let address = Address::RegPtr(seen.len()+1);
        seen.insert(term.clone(), address);
      }
    }

    // Prep the registers.
    self.X.clear();
    self.X.resize(seen.len(), Cell::Empty);

    // Every term has a register assignment. Now populate the registers.
    for (term, ptr) in seen.iter(){
      match term{
        Term::Structure {functor: Functor{name, ..}, args} => {
          let mut new_args =
            args.deref().borrow()
            .iter()
            .map(|t| Term::Register(*seen.get(t).unwrap()))
            .collect::<Vec<Term>>();
          self.X[ptr.idx()] = Cell::Term(make_struct(*name, &mut
            new_args));
        },
        _t => {
          self.X[ptr.idx()] = Cell::Term(_t.deref().clone());
        }
      }
    }
  }
  */

  /// Extracts all the addresses from a `Vec<Cell>` in a CellOrVec,
  /// handling non-struct values as needed.
  fn extract_addresses(cell_or_vec: &CellOrVec) -> Option<HashSet<Address>>{
    match cell_or_vec{
      CellOrVec::Left(_) => None,
      CellOrVec::Right(cell_vec) => {
        let addresses =
        cell_vec.iter().filter_map(| c | {
          match c{
            Cell::REF(address) => Some(address),
            _ => None
          }
        }).cloned().collect();
        Some(addresses)
      }
    }
  }

  /**
    Orders the registers of a flattened term so that registers are assigned to before they are
    used (appear on the RHS of an assignment). The order is returned in a vector.

    Nothing is modified by this function. It assumes that the registers already contain a
    flattened term. Registers containing variable terms are not included in the result.

    > [F]or left-to-right code generation to be well-founded, it is necessary
    > to order a flattened query term to ensure that a register name may
    > not be used in the right-hand side of an assignment (viz., as a subterm)
    > before its assignment, if it has one (viz., being the left-hand side).
  */
  fn order_registers(registers: RcRefCell<HashMap<Address, CellOrVec>>) -> RcRefCell<Vec<Address>>{
    // Contains the register arguments we've seen before.
    let mut seen: HashSet<Address> = HashSet::new();
    let mut unseen: HashSet<Address> = HashSet::new();
    // The vector holding the result
    let ordered: RcRefCell<Vec<Address>> = Rc::new(RefCell::new(Vec::new()));
    let mut ordered_ref = ordered.deref().borrow_mut();

    let reg_ref = registers.deref().borrow();

    // First collect which registers need to be ordered, marking registers containing variables
    // as seen.
    for (address, cell_or_vec) in reg_ref.iter(){
      match cell_or_vec {
        CellOrVec::Left(Cell::REF(address)) => {
          // Once a term is flattened, the variables in the term can be replaced with the
          // register associated to the variable. Thus, we ignore variables.
          seen.insert(*address);
        },
        CellOrVec::Right(_c)=>{
          unseen.insert(*address);
        },
        CellOrVec::Left(_t) => {
          unreachable!(
            "Error: Encountered a non-variable/non-struct cell after flattening a term: {}.",
            _t
          );
        }
      }
    }

    ordered_ref.reserve(unseen.len());

    // Now try to order the registers to maintain the invariant above.
    loop {
      let next_regs: HashSet<Address> =
        unseen.iter().filter_map(
          |address| {
            match Self::extract_addresses(reg_ref.get(address).unwrap()) {
              Some(cell_vec)
                if cell_vec.is_subset(&seen) => {
                Some(address)
              }
              _v => {
                None
              }
            }
          }
        ).cloned().collect();

      for reg in next_regs{
        ordered_ref.push(reg);
        seen.insert(reg);
        unseen.remove(&reg);
      }

      if unseen.is_empty() {
        break;
      }
    }
    ordered.clone()
  }

  /**
    Orders the registers of a flattened term so that registers are assigned to before they are
    used (appear on the RHS of an assignment). The order is returned in a vector.

    Nothing is modified by this function. It assumes that the registers already contain a
    flattened term. Registers containing variable terms are not included in the result.

    > [F]or left-to-right code generation to be well-founded, it is necessary
    > to order a flattened query term so as to ensure that a register name may
    > not be used in the right-hand side of an assignment (viz., as a subterm)
    > before its assignment, if it has one (viz., being the left-hand side).
  */
  /*
  fn order_registers_old(&self) -> Vec<Address>{
    // Contains the register arguments we've seen before.
    let mut seen: HashSet<Address> = HashSet::new();
    let mut unseen: HashSet<Address> = HashSet::new();
    // The vector holding the result
    let mut ordered: Vec<Address> = Vec::new();

    // First collect which registers need to be ordered, marking registers containing variables
    // as seen.
    for (i, cell) in self.X.iter().enumerate(){
      match cell {
        Cell::Term(Term::Variable(_)) => {
          // Once a term is flattened, the variables in the term can be replaced with the
          // register associated to the variable. Thus, we ignore variables.
          seen.insert(Address::from_reg_idx(i));
        },
        Cell::Term(Term::Structure {..})=>{
          unseen.insert(Address::from_reg_idx(i));
        },
        _t => {
          unreachable!(
            "Error: Encountered a non-term cell register after flattening a term: {}.",
            _t
          );
        }
      }
    }

    ordered.reserve(unseen.len());

    // Now try to order the registers to maintain the invariant above.
    loop {
      let next_regs: Vec<Address> =
        unseen.iter().filter_map(
          |address|
            match self
              .value_at(address)
              .clone() {
              Cell::Term(t@Term::Structure { .. })
              if t.struct_register_addresses()
                  .unwrap()
                  .is_disjoint(&unseen) =>
                {
                  Some(*address)
                }
              _c => None
            } // end match on address contents
        ).collect();

      for reg in next_regs{
        ordered.push(reg);
        seen.insert(reg);
        unseen.remove(&reg);
      }

      if unseen.is_empty() {
        break;
      }
    }

    ordered
  }

  /**
    Compiles the query into a sequence of instructions and interprets those instructions.

    In this version of the machine, the "instructions" are just calls to `put_structure`, etc.
    They have no concrete representation.
  */
  pub fn compile_query(&mut self, text: &str){
    let ast =
      match parser::parse(text) {
        Term::Query(term) => term.deref().borrow().clone(),
        _t => {
          eprintln!("Error: Expecting Term::Query, got {}", _t);
          Term::Empty
        }
      };
    self.flatten_term(&ast);
    let order = self.order_registers();
    self.compile_flat_term(&order, &QUERY_OPS);
  }

  /**
    Compiles the program into a sequence of instructions and interprets those instructions,
    unifying with the existing query constructed on the heap and referenced by register `X1`.
  */
  pub fn compile_program(&mut self, text: &str){
    let ast =
      match parser::parse(text) {
        Term::Program(term) => term.deref().borrow().clone(),
        _t => {
          eprintln!("Error: Expecting Term::Program, got {}", _t);
          Term::Empty
        }
      };
    self.flatten_term(&ast);
    let mut order = self.order_registers();
    order.reverse();
    println!("After flattening and ordering:\n{:?}\n{}", order, self);
    self.compile_flat_term(&order, &PROGRAM_OPS);
  }
  */

  /**
    Compiles a flattened query term into its in-memory representation.

    This function assumes that the term has been prepared with `flatten_term`.
  */
  fn compile_flat_term(&mut self, flattened: RcRefCell<HashMap<Address, CellOrVec>>, order: &Vec<Address>, machine_ops: &MachineOps){
    // Contains the register arguments we've seen before.
    let mut seen: HashSet<Address> = HashSet::new();
    let flattened_ref = flattened.deref().borrow();

    // We iterate over the tokens in the registers in `order`.
    'outer: for reg in order {
      self.T = reg.idx();
      #[cfg(debug_print)]
      println!("{}", self);
      // Match the current register as seen.
      seen.insert(*reg);
      match flattened_ref.get(reg).unwrap(){
        CellOrVec::Right(cell_vec)=>{
          let mut args = cell_vec.iter();
          if let Cell::Functor(functor) = args.next().unwrap() {
            // Query:   self.put_structure(&functor, reg);
            // Program: self.get_structure(&functor, reg);
            (machine_ops.structure)(self, &functor, reg);
            if self.fail {
              break 'outer;
            };
          } else{
            unreachable!("Cell vector doesn't start with functor: {:?}", cell_vec);
          }
          // Iterate over register arguments.
          for arg in args{
            #[cfg(debug_print)]
            println!("{}", self);
            match arg {
              Cell::REF(add) if seen.contains(&add) => {
                // Already saw this register.
                // Query:   self.set_value(add);
                // Program: self.unify_value(add);
                (machine_ops.seen_register)(self, &add);
              }
              Cell::REF(add) /*if !seen.contains(add)*/ => {
                // Have not seen this register before.
                seen.insert(*add);
                // Query:   self.set_variable(add);
                // Program: self.unify_variable(add);
                (machine_ops.new_register)(self, &add);
              }
              _ => {
                #[cfg(debug_print)]
                eprint!("EMERGENCY DUMP:\n{}", self);
                unreachable!(
                  "Error: Encountered a non-register argument after flattening a term: {} in {}.",
                  arg,
                  reg
                );
              }  // end if non-register argument
            } // end match on structure argument
            #[cfg(debug_print)]
            println!("{}", self);
            if self.fail {
              break 'outer;
            }
          } // end iterate over structure argument
        } // end match on Term::Structure
        _t => {
          // We should not encounter other cell types in `order`.
          #[cfg(debug_print)]
          eprint!("EMERGENCY DUMP:\n{}", self);
          unreachable!(
            "Error: Encountered a non-structure cell after ordering a flat term: {:?} in {}.",
            _t,
            reg
          );
        }
      } // end match on register values
    } // end iterate over registers
    #[cfg(debug_print)]
    println!("{}", self);
  }


  // endregion  Compilation/Interpretation

  // region VM instruction methods

  /// Dereference a chain of references. Returns either a variable (a self-reference), an `STR`,
  /// or a functor.
  fn dereference(& self, ptr: &Address) -> Cell{
    let cell = self.value_at(ptr);
    match cell{
      // Do not dereference variables, which reference themselves.
      Cell::REF(a) if a != *ptr =>
        self.dereference(&a),
      _ => cell,
    } // end match cell
  }

  /**
    Binds an unbound variable at one address to the other address. If both are unbound, the first
     is bound to the second (arbitrarily).
  */
  fn bind(&mut self, cell1: &Cell, cell2: &Cell){
    #[cfg(debug_print)]
    println!("bind({}, {}): ", cell1, cell2);
    match (cell1, cell2){
      // | (Cell::Empty, Cell::REF(_))
      // | (Cell::Empty, Cell::STR(add))
      (Cell::REF(add), _) if *cell1 == self.value_at(add) => {
        // `cell1` is a variable. Bind to cell2.
        println!("Binding {} to {}", add, cell2);
        self.set_value_at(&add, &cell2);
      },
      // | (_, Cell::Empty)
      (_, Cell::REF(add)) if *cell2 == self.value_at(add) => {
        // `cell2` is a variable. Bind to cell1.
        println!("Binding {} to {}", add, cell1);
        self.set_value_at(&add, &cell1);
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
    #[cfg(debug_print)]
    println!("put_structure({}, {})", functor, reg_ptr);
    let cell = Cell::STR(Address::from_heap_idx(self.HEAP.len() + 1));
    self.HEAP.push(cell.clone());
    self.HEAP.push(Cell::Functor(*functor));
    self.X[reg_ptr.idx()] = cell;
  }

  /**
    Push a new `REF` cell onto the heap containing its own address, and copy it into the given
    register.

    Note: `ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn set_variable(&mut self, reg_ptr: &Address) {
    reg_ptr.require_register();
    #[cfg(debug_print)]
    println!("set_variable({})", reg_ptr);
    let cell = Cell::REF( Address::from_heap_idx( self.HEAP.len()) );
    self.HEAP.push(cell.clone());
    self.X[reg_ptr.idx()] = cell;
  }

  /**
    Push a new cell onto the heap and copy the given register’s value into it.

    Note: This differs from `set_value_at` in two ways:
          1. The value comes from the given address rather than written to the given address.
          2. A new cell is created on the top of the `HEAP`.
          3. The data is always written to the cell at the top of the `HEAP`.
          4. The pointer type is guarded.
  */
  fn set_value(&mut self, reg_ptr: &Address) {
    reg_ptr.require_register();
    #[cfg(debug_print)]
    println!("set_value({})", reg_ptr);
    self.HEAP.push(self.value_at(reg_ptr));
  }

  /**
    Either matches a functor, binds a variable to a new functor value, or fails.

    Note: `reg_ptr` must be a pointer to a register, but this contract is not statically checked.
  */
  fn get_structure(&mut self, funct: &Functor, reg_ptr: &Address){
    reg_ptr.require_register();

    let value = self.dereference(reg_ptr);
    match value {
      Cell::REF(_) => {
        // A variable. Create a new functor structure for `funct` on the stack and bind the
        // variable to the functor.
        #[cfg(debug_print)]
        println!("get_structure({}, {}): creating struct", funct, reg_ptr);
        let cell = Cell::STR(Address::from_heap_idx(self.HEAP.len() + 1));
        self.HEAP.push(cell.clone());
        self.HEAP.push(Cell::Functor(*funct));
        self.bind(&value, &cell);
        self.mode = Mode::Write;
      },
      Cell::STR(cell_ptr @ Address::CellPtr(_)) => {
        // A pointer to a functor.
        if self.HEAP[cell_ptr.idx()] == Cell::Functor(*funct) {
          #[cfg(debug_print)]
          println!("get_structure({}, {}): functor already on stack", funct, reg_ptr);
          self.S = cell_ptr.idx() + 1;
          self.mode = Mode::Read;
        } else{
          // Failed to unify
          #[cfg(debug_print)]
          println!("get_structure({}, {}) - STR points to nonexistent functor", funct, reg_ptr);
          self.fail = true;
        }
      },
      _ => {
        // Failed to unify
        #[cfg(debug_print)]
        println!("get_structure({}, {}) - neither REF nor STR found: {}", funct, reg_ptr, value);
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
        #[cfg(debug_print)]
        println!("unify_variable({}):  {} <- H[S={}]", reg_ptr, reg_ptr, self.S);
        self.X[reg_ptr.idx()] = self.HEAP[self.S].clone();
      } // end if Mode::Read
      Mode::Write => {
        #[cfg(debug_print)]
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
        #[cfg(debug_print)]
        println!("unify_value({}):  unifying", reg_ptr);
        self.unify(*reg_ptr, Address::from_heap_idx(self.S));
      }
      Mode::Write => {
        #[cfg(debug_print)]
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
      let d1 = PDL.pop().unwrap();
      let d1 = &self.dereference(&d1);
      let d2 = PDL.pop().unwrap();
      let d2 = &self.dereference(&d2);
      if d1 != d2 {
        match (d1, d2){
          | (Cell::REF(_),      _      )
          | (      _     , Cell::REF(_)) => {
            // One of `d1` and `d2` is a variable, and the other is either a variable or a
            // `Cell::STR` because of how functors are created on the `HEAP`.
            self.bind(d1, d2);
          },
          _ => {
            // Neither `d1` nor `d2` are variables. In fact, we know that one of `d1` and `d2` is
            // a `Cell::STR` because of how functors are created on the `HEAP`.
            // println!("d1: {}\nd2: {}", d1, d2);
            let f1 = self.extract_functor(d1);
            let f2 = self.extract_functor(d2);
            if f1 == f2{
              // Since one of `d1` and `d2` is a `Cell::STR`, they both are.
              let v1 = self.extract_address(d1).unwrap();
              let v2 = self.extract_address(d2).unwrap();
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
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WZero::make_register_table('H', &self.HEAP, Some(self.S));
    let x_table = WZero::make_register_table('X', &self.X, None);
    let y_table = WZero::make_register_table('Y', &self.Y, Some(self.T));
    let mut table =
      table!([h_table, x_table, y_table]);

    table.set_format(*TABLE_DISPLAY_FORMAT);
    table.set_titles(row![ub->"Heap", ub->"Query Registers", ub->"Program Registers"]);

    write!(f, "Mode: {}\tFailed: {}\n{}", self.mode, self.fail, table)
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

struct MachineOps{
  structure: fn(&mut WZero, &Functor, &Address),
  new_register: fn(&mut WZero, &Address),
  seen_register: fn(&mut WZero, &Address),
}

const QUERY_OPS: MachineOps = MachineOps{
  structure: WZero::put_structure,
  new_register: WZero::set_variable,
  seen_register: WZero::set_value
};

const PROGRAM_OPS: MachineOps = MachineOps{
  structure: WZero::get_structure,
  new_register: WZero::unify_variable,
  seen_register: WZero::unify_value
};


#[cfg(test)]
mod tests{
  use super::*;

  // #[test]
  // Test breaks because IDE eats the whitespace in the copied+pasted expected text.
  fn query_compiles(){
    // The expression below must have a single unique flattened ordering.
    let text = "?-p(Z,h(Z,W),f(h(W, Z)))";
    let mut machine = WZero::new();
    machine.compile_query(text);

    // Check heap contents.
    let table = machine.make_heap_table();
    let answer = format!("{}", table);
    let expected = "  Address   │ Contents
 ───────────┼────────────────
  S ──→ 0:  │ <STR, HEAP[1]>
        1:  │ h/2
        2:  │ <REF, HEAP[2]>
        3:  │ <REF, HEAP[3]>
        4:  │ <STR, HEAP[5]>
        5:  │ f/1
        6:  │ <STR, HEAP[1]>
        7:  │ <STR, HEAP[8]>
        8:  │ p/3
        9:  │ <REF, HEAP[2]>
       10:  │ <STR, HEAP[1]>
       11:  │ <STR, HEAP[5]>
 ───────────┴────────────────
".to_string();
    assert_eq!(answer, expected);

    // Check register contents.
    let table = machine.make_register_table();
    let answer = format!("{}", table);
    let expected = "  Address │ Contents
 ─────────┼────────────────
     X1 = │ <STR, HEAP[8]>
     X2 = │ <REF, HEAP[2]>
     X3 = │ <STR, HEAP[1]>
     X4 = │ <STR, HEAP[5]>
     X5 = │ <REF, HEAP[3]>
 ─────────┴────────────────".to_string();
    assert_eq!(answer, expected);
  }
}
