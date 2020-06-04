//! Structures and functions for the Warren Virtual Machine, what I'm calling an
//! implementation of Warren's Abstract Machine.

use std::collections::{HashSet, HashMap};
use std::fmt::{Display, Formatter};
use std::usize::MAX;
use std::sync::{Arc, Mutex};

use prettytable::{format as TableFormat, Table};
use bimap::BiMap;

use crate::address::*;
use crate::cell::*;
use crate::token::*;
use crate::functor::*;
use crate::bytecode::*;
use crate::bytecode::EncodedInstruction;
use crate::parser::parse as parse_source_code;

lazy_static! {
  pub static ref SYMBOLS: Arc<Mutex<BiMap<Functor, Address>>> =
    Arc::new(Mutex::new(BiMap::new()));
}

#[allow(non_snake_case)]
pub struct WVM {

  // Flags
  fail: bool, // Indication of unification failure
  mode: Mode, // Read or Write mode

  // Memory Stores
  heap: Vec<Cell>, // The "global stack," a memory store
  code: Vec<u32>,  // Code memory, a memory store

  // Registers //
  hp        : usize,          // Heap Pointer, a cursor
  rp        : usize,          // Register Pointer, a cursor
  ip        : usize,          // Instruction Pointer
  registers : Vec<Cell>, // Term registers

  // Symbol table mapping line labels to their address in code memory.
  labels    : HashMap<Functor, Address>,
  // Symbol table mapping functor SYMBOLS `f/n` to a "virtual" address.
  // SYMBOLS   : BiMap<Functor, Address>,

  code_buffer : String, // String buffer for emitted code text

  // For tracing computations :
  #[cfg(feature = "trace_computation")] token_stack   :  Vec<Token>,
  #[cfg(feature = "trace_computation")] current_token :  usize

}

impl WVM {

  // region Display methods

  fn make_register_table<T> (
      name      : char,
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
      mode        :  Mode::Read, // Arbitrary value

      heap        :  vec![],
      code        :  vec![],
      registers   :  vec![],

      hp          :  0,
      rp          :  0,
      ip          :  0,

      labels      :  HashMap::new(),
      // symbols     :  BiMap::new(),
      code_buffer :  String::new(),

      // Computation tracing:
      #[cfg(feature = "trace_computation")] token_stack   : vec![],
      #[cfg(feature = "trace_computation")] current_token : MAX,
    }
  }

  /// Gives the virtual address of a `Functor`.
  fn intern_functor(&mut self, functor: &Functor) -> Address{
    let mut symbols = SYMBOLS.lock().unwrap();
    match symbols.get_by_left(functor) {
      Some(address) => *address,
      None => {
        let address = Address::Functor(symbols.len());
        symbols.insert(functor.clone(), address);
        address
      }
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
      Address::Heap(_) => self.heap     [ptr.idx()].clone(),
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

     2. Conversion from AST to `Cell`s allows us to
        build in-memory representations directly "by hand."

  */
  pub fn compile(&mut self, text: &str, to_assembly: bool, interpret: bool){

    // Parse the text into a `Term`, which is a tree structure in general.
    let (atoms, queries) = parse_source_code(text);

    // Programs
    for ast in atoms{
      let tokenizer = Tokenizer::new(&ast, true);
      self.compile_tokens(tokenizer, true, to_assembly, interpret);
    }
    // Queries
    for ast in queries{
      let tokenizer = Tokenizer::new(&ast, false);
      self.compile_tokens(tokenizer, false, to_assembly, interpret);
    }



    #[cfg(feature = "trace_computation")]
      {
        println!("Compiled to {} bytes of bytecode.", self.code.len()*4);
        if to_assembly {
          println!("# Compiled Code Instructions\n{}", self.code_buffer);
        }
      }
  }

  /**
    Compiles a flattened query term into its in-memory representation.

    This function assumes that the term has been prepared with `flatten_term`.
  */
  fn compile_tokens(
    &mut self, tokenizer: Tokenizer,
    is_program: bool,
    to_assembly: bool,
    interpret: bool
  ){
    // Contains the register arguments we've seen before.
    let mut seen: HashSet<Address> = HashSet::new();

    #[cfg(feature = "trace_computation")]
      {
        let stream_copy = tokenizer.clone();
        self.token_stack = stream_copy.collect();
        self.current_token = 1;
        println!("{}", self);
        self.current_token = 0;
      }

    // We iterate over the tokens in the registers in `order`.
    for token in tokenizer {

      #[cfg(feature = "trace_computation")] { self.current_token += 1; }

      match &token {

        Token::Assignment(functor, register_address) => {
          seen.insert(*register_address);

          // The instruction we are about to push onto `self.code`
          // will be the first instruction of `procedure`.
          let procedure_address = self.code.len() as AddressType;
          self.labels.insert(functor.clone(), Address::Code(procedure_address));
          match is_program {

            true  => {  // Program
              self.intern_functor(&functor);
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
              if interpret{
                self.get_structure(functor, &register_address);
              }
            }

            false => { // Query
              let instruction =
                Instruction::Binary {
                  opcode: Operation::PutStructure,
                  address1: self.intern_functor(&functor), // Pointer to code
                  address2: *register_address
                };
              self.emit_bytecode(encode_instruction(&instruction));

              if to_assembly {
                self.emit_assembly(&instruction, &token);
              }
              if interpret{
                self.put_structure(&functor, register_address);
              }
            }

          } // end match is_program

          // Update the register pointer
          self.rp = register_address.idx();

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
                  if interpret{
                    self.unify_value(&address);
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
                  if interpret{
                    self.set_value(&address);
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
                  if interpret{
                    self.unify_variable(address);
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
                  if interpret{
                    self.set_variable(&address);
                  }
                }

              } // end match is_program
            }

          } // end if seen address before

        }
      } // end match on token type

      #[cfg(feature = "trace_computation")] println!("{}", self);

      if self.fail {
        break;
      };
    } // end iterate over tokens
  }

  fn emit_assembly(&mut self, instruction: &Instruction, token: &Token){
    self.code_buffer.push_str(format!("{:30}%   {}\n", format!("{}", instruction), token).as_str());
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
  fn put_structure(& mut self, functor: &Functor, reg_ptr: &Address) {
    reg_ptr.require_register();
    #[cfg(feature = "trace_computation")] println!("PutStructure({}, {})", functor, reg_ptr);

    let cell = Cell::STR(Address::from_heap_idx(self.heap.len() + 1));
    self.heap.push(cell.clone());
    self.heap.push(Cell::Functor(functor.clone()));
    self.set_value_at(reg_ptr, &cell);
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
  fn get_structure(&mut self, functor: &Functor, register_address: &Address){
    register_address.require_register();

    let target_address = self.dereference(register_address);
    let target_cell = self.value_at(&target_address);

    match target_cell {

      Cell::REF(_) => {
        // A variable. Create a new functor structure for `functor` on the stack, bind the
        // variable to the functor, and set `mode` to `Mode::Write`.
        #[cfg(feature = "trace_computation")]
          println!("GetStructure({}, {}): creating struct", functor, register_address);

        let functor_address = Address::from_heap_idx(self.heap.len() + 1);
        let cell      = Cell::STR(functor_address);
        self.mode     = Mode::Write;

        self.heap.push(cell);
        self.heap.push(Cell::Functor(functor.clone()));
        self.bind(&target_address, &functor_address);
      },

      Cell::STR(heap_address @ Address::Heap(_)) => {
        // A pointer to a functor.
        if self.heap[heap_address.idx()] == Cell::Functor(functor.clone()) {
          #[cfg(feature = "trace_computation")]
            println!("GetStructure({}, {}): functor already on stack", functor, register_address);

          self.hp   = heap_address.idx() + 1;
          self.mode = Mode::Read;

        } else{
          #[cfg(feature = "trace_computation")]
            println!("GetStructure({}, {}) - STR points to different functor", functor, register_address);
          self.fail = true;
        }
      }

      _c => {
        // This is an error condition that should not happen in correct programs.
        #[cfg(feature = "trace_computation")]
          println!("GetStructure({}, {}) - neither REF nor STR found: {}",
                   functor, register_address, _c);
        self.fail = true;
      }
    };
  }

  /**
    Either reads the top of the `HEAP` into register `X[i]` or creates a variable on the `HEAP`
    and assigns it to register X[i}.

    Note: `ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn unify_variable(&mut self, register_address: &Address){
    register_address.require_register();

    match self.mode {

      Mode::Read  => {
        #[cfg(feature = "trace_computation")]
          println!("UnifyVariable({}):  {} <- H[S={}]", register_address, register_address, self.hp);
        // ToDo: Extra copy here:
        let value = &self.heap[self.hp].clone();
        self.set_value_at(register_address, value);
      }

      Mode::Write => {
        #[cfg(feature = "trace_computation")] print!("UnifyVariable({}):  ", register_address);
        self.set_variable(register_address)
      }

    } // end match mode

    self.hp += 1;
  }

  /**
    Either pushes the value of `X[i]` onto the `HEAP` (write) or unifies `X[i]` and the cell at `S`.

    Note: `reg_ptr` must be a register pointer, but this contract is not statically checked.
  */
  fn unify_value(&mut self, reg_ptr: &Address){
    reg_ptr.require_register();

    match self.mode {

      Mode::Read  => {
        #[cfg(feature = "trace_computation")] println!("UnifyValue({}):  unifying", reg_ptr);
        self.unify(*reg_ptr, Address::from_heap_idx(self.hp));
      }

      Mode::Write => {
        #[cfg(feature = "trace_computation")] print!("UnifyValue({}):  ", reg_ptr);
        self.set_value(reg_ptr);
      }
    }

    self.hp += 1;
  }

  fn unify(&mut self, a1: Address, a2: Address){
    let mut pdl: Vec<Address> = Vec::new();

    pdl.push(a1);
    pdl.push(a2);
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
      .separator(
        TableFormat::LinePosition::Bottom,
        TableFormat::LineSeparator::new('─', '┴', ' ', ' ')
      )
      .padding(1, 1)
      .build();
}

impl Display for WVM {

  // We print the token_stack if `trace_computation` is on.
  #[cfg(feature = "trace_computation")]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table     = WVM::make_register_table('H', &self.heap,        self.hp,                0);
    let x_table     = WVM::make_register_table('X', &self.registers,   self.rp,                1);
    let token_table = WVM::make_register_table('Y', &self.token_stack, self.current_token - 1, 1);

    let mut combined_table = table!([h_table, x_table, token_table]);

    combined_table.set_titles(row![ub->"Heap", ub->"Registers", ub->"Token Stack"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    let success = match self.fail{
      true  => "Failed to unify.",
      false => "Unifying successfully."
    };

    write!(f, "Mode: {}\n{}\n{}", self.mode, success, combined_table)
  }

  #[cfg(not(feature = "trace_computation"))]
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let h_table = WVM::make_register_table('H', &self.heap,      self.hp, 0);
    let x_table = WVM::make_register_table('X', &self.registers, self.rp, 1);


    let mut combined_table = table!([h_table, x_table]);

    combined_table.set_titles(row![ub->"Heap", ub->"Registers"]);
    combined_table.set_format(*TABLE_DISPLAY_FORMAT);

    let success = match self.fail{
      true  => "Failed to unify.",
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
