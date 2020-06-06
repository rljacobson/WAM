//! Structures and functions for the Warren Virtual Machine, what I'm calling an
//! implementation of Warren's Abstract Machine.

use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, Mutex};

use prettytable::{format as TableFormat, Table};
use bimap::BiMap;
use string_cache::DefaultAtom;

use crate::address::*;
use crate::cell::*;
use crate::token::*;
use crate::functor::*;
use crate::bytecode::*;
use crate::parser::parse as parse_source_code;
use crate::term::{Term, TermVec};
use std::rc::Rc;

lazy_static! {
  /**
    This symbol table keeps track of the names of functors that have been compiled to bytecode so
    that the names can be reconstituted for the human reader. Otherwise, functors would have
    auto-generated names. An alternative is to serialize the names to bytecode.
  */
  pub static ref SYMBOLS: Arc<Mutex<BiMap<Functor, Address>>> =
    Arc::new(Mutex::new(BiMap::new()));
}

#[allow(non_snake_case)]
pub struct WVM {

  // Flags
  fail  : bool, // Indication of unification failure
  query : bool, // True if we're running query code, false otherwise.
  mode  : Mode, // In Write mode, new elements are built on the heap.

  // Memory Stores
  heap: Vec<Cell>, // The "global stack," a memory store
  code: Vec<u32>,  // Code memory, a memory store

  // Registers //
  hp        : usize,          // Heap Pointer, a cursor
  rp        : usize,          // Register Pointer, a cursor
  ip        : usize,          // Instruction Pointer
  registers : Vec<Cell>, // Term registers

  // Symbol table mapping line labels to their address in code memory.
  labels    : Vec<(Functor, Address)>,
  // Symbol table mapping functor SYMBOLS `f/n` to a "virtual" address.
  // SYMBOLS   : BiMap<Functor, Address>,

  assembly_buffer: String, // String buffer for emitted code text
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
        for i in 1..functor.arity + 1 {
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
        eprintln!("Error: Could not construct a term from the cell at {}", data_address);
        Term::Empty
      }

    }
  }

  fn make_register_table<T> (
      name      : &str,
      registers : &[T],
      highlight : usize,
      start     : usize
    ) -> Table
    where T: Display
  {

    let mut table = Table::new();

    table.set_format(*TABLE_DISPLAY_FORMAT);
    table.set_titles(row![ubr->"Address", ubl->"Contents"]);

    for (i, cell) in registers.iter().enumerate() {
      match i == highlight{

        true  => {
          table.add_row(
            row![r->format!("* --> {}[{}] =", name, i+start), format!("{}", cell)]
          );
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
      fail        :  false,
      query       :  true,
      mode        :  Mode::Read, // Arbitrarily chosen.

      heap        :  vec![],
      code        :  vec![],
      registers   :  vec![],

      hp          :  0,
      rp          :  0,
      ip          :  0,

      labels      :  Vec::new(),
      assembly_buffer:  String::new(),
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
      _                    => None
    }
  }

  /// Performs one step of `dereference`, what C programmers think of as dereferencing.
  fn value_at(&self, ptr: &Address) -> Cell{
    match ptr {
      Address::Heap(_) => self.heap[ptr.idx()].clone(),
      Address::Register(_) => self.registers[ptr.idx()].clone(),
      | Address::Functor(_)
      | Address::Code(_) => {
        eprintln!("Tried to use `value_at()` with a code address: {}", ptr);
        panic!();
      }
    }
  }

  /**
    Sets the value at the given address, dynamically growing the relevant vector if the address
    is larger than the max index of the vector.

    See `set_value` for differences between the two functions.
  */
  fn set_value_at(&mut self, address: &Address, cell: &Cell){
    match address {

      Address::Register(_)  => {
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
      },

      | Address::Functor(_)
      | Address::Code(_) => {
        eprintln!("Error: Tried to use `set_value_at()` with a code address. Ignoring.");
      }

    }
  }

  pub fn dump_assembly(&self) -> &str {
    self.assembly_buffer.as_str()
  }

  pub fn dump_bytecode(&self) -> &[u32]{
    self.code.as_slice()
  }

  // endregion

  // region Compilation/Interpretation

  /**

    Accepts a program and/or query as a string and turns it into a sequence
    of operations, an in-memory representation, and/or assembly source code.

    The compilation pipeline is this:
    ```
    text -> [`parser::parse`] -> `Term`s ->⋯

        ┌───────────────[`token::flatten_term`]────────────────┐
    ⋯->*│*-> [`TermIter`] -> `Cell`s -> [`order_registers`] ->*│*->⋯
        └──────────────────────────────────────────────────────┘

    ⋯-> `Token`s -> [`compile_tokens`] -> `Cell`s/instructions ->⋯

    ⋯-> [`unify`] -> Success/Fail
    ```
    The [`unify`] step executes the instructions to build the in-memory `Cell`s. The
    conversion to `Cell`s, then `Token`s, and back to `Cell`s again is for two reasons:

     1. Because the term needs to be flattened and reordered, we would
        have the "conversion" step no matter what.

     2. Conversion from AST to `Cell`s allows us to build in-memory
        representations directly "by hand" should we so desire.

  */
  pub fn compile(&mut self, text: &str, to_assembly: bool, execute: bool){

    // Parse the text into a `Term`, which is a tree structure in general.
    let (atoms, queries) = parse_source_code(text);

    // Queries
    for ast in queries{
      let tokenizer = Tokenizer::new(&ast, false);
      self.compile_tokens(tokenizer, false, to_assembly);
    }
    // Programs
    for ast in atoms{
      let tokenizer = Tokenizer::new(&ast, true);
      self.compile_tokens(tokenizer, true, to_assembly);
    }

    #[cfg(feature = "trace_computation")]
      {
        println!("Compiled to {} bytes of bytecode.", self.code.len()*4);
        if to_assembly {
          println!("% Assembly Code Instructions\n{}", self.assembly_buffer);
        }
      }

    if execute {
      self.run();
    }
  }

  /**
    Compiles a single  query or procedure by compiling the
    provided flattened term into its in-memory representation.

    This function assumes that the term has been prepared with `flatten_term`.
  */
  fn compile_tokens(
    &mut self, tokenizer: Tokenizer,
    is_program: bool,
    to_assembly: bool
  ){
    // Contains the register arguments we've seen before.
    let mut seen: HashSet<Address> = HashSet::new();

    // Flag the first token.
    let mut outermost = true;

    // We iterate over the tokens in the registers in `order`.
    for token in tokenizer {

      // Book keeping for the outermost term, which defines the procedure/query
      if outermost {
        // The instruction we are about to push onto `self.code` will
        // be the first instruction of `procedure`. No checking is
        // done to see if there already is a procedure with this name.
        let procedure_address = Address::Code(self.code.len() as AddressNumberType);
        // The first token MUST be a Token::Assignment in M_1, but the compiler doesn't know that.
        if let Token::Assignment(functor, _address) = &token{
          self.labels.push((functor.clone(), procedure_address));
        }

        if to_assembly {
          match is_program{
            true  => self.assembly_buffer.push_str(format!("% Procedure {}\n", &token).as_str()),
            false => self.assembly_buffer.push_str(format!("% Query\n").as_str()),
          }
        }
        outermost = false;
      }

      match &token {

        Token::Assignment(functor, register_address) => {
          seen.insert(*register_address);

          match is_program {

            true  => {  // Program

              // Record functor so its name can be reconstituted from bytecode.
              intern_functor(&functor);
              let instruction =
                Instruction::BinaryFunctor {
                  opcode: Operation::GetStructure,
                  address: *register_address,  // Register address
                  functor: functor.clone(),
                };
              self.emit_bytecode(encode_instruction(&instruction));

              if to_assembly {
                self.emit_assembly(&instruction, &token);
              }
            }

            false => { // Query

              let instruction =
                Instruction::BinaryFunctor {
                  opcode: Operation::PutStructure,
                  address: *register_address,
                  functor: functor.clone()
                };
              self.emit_bytecode(encode_instruction(&instruction));

              if to_assembly {
                self.emit_assembly(&instruction, &token);
              }
            }

          } // end match is_program
        } // end is assignment

        Token::Register(address) => {

          match seen.contains(&address) {

            true  => {
              // Already saw this register.
              match is_program {

                true  => {  // Program
                  let instruction =
                    Instruction::Unary {
                      opcode: Operation::UnifyValue,
                      address: *address
                    };
                  self.emit_bytecode(encode_instruction(&instruction));

                  if to_assembly {
                    self.emit_assembly(&instruction, &token);
                  }
                }

                false => { // Query
                  let instruction =
                    Instruction::Unary {
                      opcode: Operation::SetValue,
                      address: *address
                    };
                  self.emit_bytecode(encode_instruction(&instruction));
                  if to_assembly {
                    self.emit_assembly(&instruction, &token);
                  }
                }

              }
            }

            false => {
              // Have not seen this register before.
              seen.insert(*address);
              match is_program {

                true  => {  // Program
                  let instruction =
                    Instruction::Unary {
                      opcode: Operation::UnifyVariable,
                      address: *address
                    };
                  self.emit_bytecode(encode_instruction(&instruction));

                  if to_assembly {
                    self.emit_assembly(&instruction, &token);
                  }
                }

                false => { // Query
                  let instruction =
                    Instruction::Unary {
                      opcode: Operation::SetVariable,
                      address: *address
                    };
                  self.emit_bytecode(encode_instruction(&instruction));

                  if to_assembly {
                    self.emit_assembly(&instruction, &token);
                  }
                }

              } // end match is_program
            }
          } // end if seen address before
        }
      } // end match on token type

    } // end iterate over tokens

    // For now, we end query construction with a `Proceed` as well.
    let instruction = Instruction::Nullary(Operation::Proceed);
    self.emit_bytecode(encode_instruction(&instruction));
    if to_assembly {
      self.assembly_buffer.push_str(format!(
        "{:30}%   End of atom\n",
        format!("{}", instruction)
      ).as_str());
    }

  }

  fn emit_assembly(&mut self, instruction: &Instruction, token: &Token){
    self.assembly_buffer.push_str(format!("{:30}%   {}\n", format!("{}", instruction), token).as_str());
  }

  /// This method only inserts bytes into `self.code`.
  fn emit_bytecode(&mut self, instruction: EncodedInstruction){
    match instruction {

      EncodedInstruction::Word(word) => {
        self.code.push(word)
      },

      EncodedInstruction::DoubleWord(double_word) => {
        let words: TwoWords;
        unsafe {
          words = std::mem::transmute(double_word);
        }
        self.code.push(words.low);
        self.code.push(words.high);
      }
    }
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
      (Cell::REF(address), _) if address1 == address && address1.is_register() => {
        // `cell1` is a register variable. Bind to cell2.
        #[cfg(feature = "trace_computation")] println!("Binding {} to {}", address1, address2);
        self.set_value_at(&address1, &Cell::REF(*address2));
      }

      (_, Cell::REF(address)) if address2 == address && address2.is_register() => {
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
  fn set_variable(&mut self, reg_ptr: &Address) {
    reg_ptr.require_register();
    #[cfg(feature = "trace_computation")] println!("SetVariable({})", reg_ptr);

    let cell = Cell::REF( Address::from_heap_idx( self.heap.len()) );
    self.heap.push(cell.clone());
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
    #[cfg(feature = "trace_computation")] println!("SetValue({})", reg_ptr);

    self.heap.push(self.value_at(reg_ptr));
  }

  /**
    Either matches a functor, binds a variable to a new functor value, or fails.

    Note: `register_address` must be a pointer to a register, but this contract is not statically checked.
  */
  fn get_structure(&mut self, functor: &Functor, address: &Address){
    address.require_register();

    self.mode = Mode::Read;

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
        self.mode           = Mode::Write;

        self.heap.push(cell);
        self.heap.push(Cell::Functor(functor.clone()));
        // Remember that we want to bind to the `STR`, not the `f/n`, so subtract 1..
        self.bind(&target_address, &Address::from_heap_idx(functor_idx - 1));
      },

      Cell::STR(heap_address @ Address::Heap(_)) => {
        // A pointer to a functor.
        if self.heap[heap_address.idx()] == Cell::Functor(functor.clone()) {
          #[cfg(feature = "trace_computation")]
            println!("GetStructure({}, {}): functor already on stack", functor, address);

          self.hp   = heap_address.idx() + 1;
          self.mode = Mode::Read;

        } else{
          #[cfg(feature = "trace_computation")]
            println!("GetStructure({}, {}) - STR points to different functor", functor, address);
          self.fail = true;
        }
      }

      _c => {
        // This is an error condition that should not happen in correct programs.
        #[cfg(feature = "trace_computation")]
          println!("GetStructure({}, {}) - neither REF nor STR found: {}",
                   functor, address, _c);
        self.fail = true;
      }
    };
  }

  /**
    Either reads the top of the `HEAP` into register `X[i]` or creates a variable on the `HEAP`
    and assigns it to register X[i}.

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
        #[cfg(feature = "trace_computation")] print!("UnifyVariable({}):  ", address);
        self.set_variable(address)
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

  // M_1 Operations

  /**
    put_variable Xn, Ai

      "The first occurrence of a variable in i-th argument position pushes a new unbound REF cell
      onto the heap and copies it into that variable’s register as well as argument register Ai."
  */
  fn put_variable(&mut self, address1: &Address, address2: &Address){
    address1.require_register();
    address2.require_register();
    #[cfg(feature = "trace_computation")] println!("PutVariable({}, {})", address1, address2);

    let cell = Cell::REF(Address::Heap(self.heap.len()));
    self.set_value_at(address1, &cell);
    self.set_value_at(address2, &cell);
    self.heap.push(cell);
  }

  /**
    get_variable Xn, Ai

      "Sets the argument in the Xn position to the value of argument register Ai."
  */
  fn get_variable(&mut self, address1: &Address, address2: &Address){
    address1.require_register();
    address2.require_register();
    #[cfg(feature = "trace_computation")] println!("GetVariable({}, {})", address1, address2);

    self.set_value_at(address1, &self.value_at(address2));
  }

  /**
    put_value Xn Ai

    "A later occurrence copies its value into argument register Ai."
  */
  fn put_value(&mut self, address1: &Address, address2: &Address){
    address1.require_register();
    address2.require_register();
    #[cfg(feature = "trace_computation")] println!("PutValue({}, {})", address1, address2);

    self.set_value_at(address2, &self.value_at(address1));
  }

  /**
    get_value Xn, Ai

    Unifies Xn with Ai.
  */
  fn get_value(&mut self, address1: &Address, address2: &Address){
    self.unify(address1, address2);
  }

  /// A function call to the function with entry point `address`.
  fn call(&mut self, address: &Address) {
    address.require_code();
    #[cfg(feature = "trace_computation")] println!("Call({})", address);

    self.ip = address.idx();
  }

  // endregion

  // region VM control methods

  /**
    Begin execution of the bytecode starting at code address 0. The instruction pointer
    `ip` is reset, and the `fail` flag is set to `false` before execution begins. Otherwise,
    the caller is responsible for setting/resetting the vm registers and memory stores.

    In M_1, every fact is labeled with a functor and has a code address at which the code
    associated with the fact begins. These labeled procedures, as we'll call them, are
    stored in the WVM::Labels array in the order in which they were compiled, with the
    query appearing first. Then `run` iterates over each label, calling them in turn.
    `Proceed` signals end of procedure, reporting success/failure. Upon failure, `ip` is
    set to the next label. We also bail out of a procedure early if we fail partway through.

    The `Proceed` instruction is just for show in M_1, as the bytecode could just as easily run
    linearly. The `Call` instruction is also just for show. It is functional but never used.

    If we cared about speed, we would optimize this function and the functions it calls as much
    as possible. But we don't care about speed. In fact, it's already really fast. Don't
    micro-optimise.
  */
  fn run(&mut self){

    #[cfg(feature = "trace_computation")] {
      println!("Labels:");
      for (f, a) in self.labels.iter(){
        println!("\t{} -> {}", f, a);
      }
      println!();
    }

    let labels = self.labels.clone();
    for (k, (_functor, address)) in labels.iter().enumerate() {
      self.ip   = address.idx();
      self.fail = false;
      // In M_1, query code always appears first, while everything else is fact code.
      self.query = (k == 0);

      let mut words = TwoWords { low: 0, high: 0 };
      while self.ip < self.code.len() {
        words.low = self.code[self.ip];

        let encoded = match is_double_word_instruction(&words.low) {
          true if self.ip + 1 < self.code.len() => {
            words.high = self.code[self.ip + 1];
            // We increment `ip` by the size of the current instruction before we execute the
            // instruction. That way the instruction has an opportunity to change control flow.
            self.ip += 2;
            EncodedInstruction::DoubleWord(words)
          }
          true => {
            // Needs another word, but there are no more.
            panic!("Error: Unexpectedly ran out of bytecode.")
          }
          false => {
            self.ip += 1; // See above comment.
            EncodedInstruction::Word(words.low)
          }
        };

        let instruction: Instruction = match try_decode_instruction(&encoded) {
          Some(Instruction::Nullary(Operation::Proceed)) => {
            // End of procedure, stop executing instructions linearly.
            break;
          },
          Some(i) => i,
          None => {
            eprintln!("Could not decode instruction: ({:X}, {:X})", words.low, words.high);
            panic!();
          }
        };

        self.exec(&instruction);

        // For M_1, every procedure fails to unify immediately at the atom's
        // head except at most one. If that one unifies, `self.fail` is false.
        if self.fail {
          // unification failed, skip the rest of the procedure.
          #[cfg(feature = "trace_computation")] println!("EARLY EXIT");
          break;
        }

        #[cfg(feature = "trace_computation")] println!("{}", self);
      } // end loop over instructions in procedure

      // M_1 only finds at most one success, as there is at most one fact for a given head.
      if !self.fail && self.query == false {

        let term = self.memory_to_term(&Address::Register(1));
        #[cfg(not(feature = "trace_computation"))]
        println!("{}\n", term.as_expression_string());
        println!("TRUE");
        break;
      }
    } // 'outer: finished iterating over procedures

    if self.fail {
      println!("FALSE");
    }

  }

  /**
    Executes a single instruction, incrementing/setting `ip` and other state as appropriate to
    the instruction.
  */
  fn exec(&mut self, instruction: &Instruction){
    use Operation::*;

    match &instruction {
      /*
        See bytecode/instruction.rs for the list of operations. Each opcode match needs:
          1. an `Operation`
          2. that matches the function call, and
          3. to increment `self.ip` according to the number of words in the instruction.
      */

      Instruction::BinaryFunctor { opcode, address, functor } => {
        match opcode {
          PutStructure => { self.put_structure(functor, address);}
          GetStructure => { self.get_structure(functor, address);}
          _            => { unreachable!("Error: The opcode {} was decoded as {}.", opcode, instruction); }
        }
        // Update the register pointer. This is used for display, to point to the active register.
        self.rp = address.idx();
      }
      Instruction::Binary { opcode, address1, address2 } => {
        match opcode {
          PutVariable => { self.put_variable(address1, address2);}
          GetVariable => { self.get_variable(address1, address2);}
          PutValue    => { self.put_value(address1, address2);   }
          GetValue    => { self.get_value(address1, address2);   }
          _           => { unreachable!("Error: The opcode {} was decoded as {}.", opcode, instruction); }
        }
      }
      Instruction::Unary { opcode, address } => {
        match opcode {
          SetVariable   => { self.set_variable(address);  }
          SetValue      => { self.set_value(address);     }
          UnifyVariable => { self.unify_variable(address);}
          UnifyValue    => { self.unify_value(address);   }
          Call          => { self.call(address);          }
          _             => { unreachable!("Error: The opcode {} was decoded as {}.", opcode, instruction); }
        }
      }
      Instruction::Nullary(opcode) => {
        match opcode {
          Proceed => { /* Proceed is a noop in M_1. */ }
          _       => { unreachable!("Error: The opcode {} was decoded as {}.", opcode, instruction); }
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

  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WVM::make_register_table("HEAP", &self.heap,
                                           self.hp, 0);
    let x_table = WVM::make_register_table("X", &self.registers,
                                           self.rp, 1);


    let mut combined_table = table!([h_table, x_table]);

    combined_table.set_titles(row![ub->"Heap", ub->"Registers"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    /*
      Attempt to construct:
        1. the substitutions, and
        2. the resulting expression.

      The second can be recovered from the X[1] register. For the first, it's just a matter of
      recording which variable names went with which heap addresses. Unfortunately, we throw
      away that information during flattening of the term, but the idea is easy in principle.
      We will save that feature for a future machine.
    */
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

pub fn try_get_interned_functor(address: &Address) -> Option<Functor>{
  let symbols       = SYMBOLS.lock().unwrap();
  let maybe_functor = symbols.get_by_right(address);

  match maybe_functor {
    Some(functor) => Some(functor.clone()),
    None          => None
  }
}

/// Gives the virtual address of a `Functor`.
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
