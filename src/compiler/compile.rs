/*!
  Functions to produce a compilation artifact from source code input.
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
  The [`unify`] step executes the instructions to build the in-memory `Cell`s.
*/

use std::cell::RefCell;
use std::collections::{HashSet, HashMap};

use string_cache::DefaultAtom;

use crate::address::*;
use crate::cell::*;
use crate::functor::*;
use crate::bytecode::*;
use super::term::*;
use crate::compiler::parser::parse as parse_source_code;
use crate::functor::intern_functor;

const MAXIMUM_ORDER_ATTEMPTS: usize = 200;

/// A `Compilation` is the result of executing `Compilation::compile(source)` and may contain
/// binary bytecode, assembly code, variable bindings, and/or line labels.
pub struct Compilation{
  pub code            : Vec<Word>,  // Code memory, a binary memory store
  // Symbol table mapping line labels to their address in code memory.
  pub labels          : Vec<(Functor, Address)>,
  pub assembly_buffer : String, // String buffer for emitted code text
  pub variable_bindings: Vec<(DefaultAtom, Address)>,
}

impl Compilation {

  /// Constructs the compilation object and immediately passes
  /// control to `compile_driver`, which does the real work.
  //  ToDo: In case of parse error, should we return an error, a partially constructed object, `None`?
  pub fn compile(text: &str, to_assembly: bool) -> Option<Compilation>{
    let mut compilation = Compilation{
      code            : Vec::new(),
      labels          : Vec::new(),
      assembly_buffer : "".to_string(),
      variable_bindings: Vec::new(),
    };

    let result = compilation.compile_driver(text, to_assembly);
    match result {
      Ok(_)  => Some(compilation),
      Err(_) => None
    }
  }

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
    The [`unify`] step executes the instructions to build the in-memory `Cell`s of the resolvent,
     if possible, or fail otherwise.

  */
  fn compile_driver(&mut self, text: &str, to_assembly: bool) -> Result<(), ()>{
    let compilation_time = std::time::Instant::now();

    // Parse the text into a `Term`, which is a tree structure in general.
    let (atoms, query) =
      match parse_source_code(text){
        Ok(tuple) => tuple,
        Err(_) => {
          eprintln!("{}", "Compilation failed.");
          return Err(())
        }
      };

    // Queries
    // if let Some(Term::Query(term_vec)) = query {
    //   self.compile_tokens(&ast, false, to_assembly);
    // }
    // Programs
    for ast in atoms{
      self.compile_tokens(&ast, true, to_assembly);
    }

    self.compile_bytecode_header(to_assembly);

    println!("Compiled to {} bytes of bytecode in {:?}.\n",
             self.code.len()*4, compilation_time.elapsed());

    #[cfg(feature = "trace_computation")]
      {
        if to_assembly {
          println!("% Assembly Code Instructions\n{}\n", self.assembly_buffer);
        }
      }

    Ok(())
  }

  /**

    A helper function for `compile` that prepends `Call` instructions at
    the beginning of `self.code`.

    For each procedure, insert a `Call f/n` instruction at the beginning of
    the bytecode that calls the procedure so that each is called in order.
  */
  fn compile_bytecode_header(&mut self, to_assembly: bool){
    // Offset is the length in `Word`s of the added header. The
    // addresses of each procedure need to be adjusted by this amount.
    let offset = self.labels.len() + 1; // The `+1` is for `Halt`.

    let mut new_encoded: Vec<Bytecode> =
      Vec::with_capacity(offset + self.code.len());
    let mut new_code: Vec<Word> =
      Vec::with_capacity(offset + self.code.len());
    let mut new_assembly: String = String::new();

    new_encoded.extend(
      self.labels.iter().map(
        |(f, a)| {
          let address: Address = *a + (offset as AddressNumberType);
          let instruction = Instruction::Unary {
            opcode: Operation::Call,
            argument: Argument::Address(address),
          };
          if to_assembly {
            new_assembly.push_str(
              format!("{:30}%   {}\n", format!("{}", instruction), f).as_str()
            );
          }
          instruction.encode_instruction()
        })
    );
    // Finally, a `Halt`
    let halt = Instruction::Nullary(Operation::Halt);
    new_encoded.push(halt.encode_instruction());
    if to_assembly {
      new_assembly.push_str(
        format!("{:30}%   {}\n", format!("{}", halt), "End Program").as_str()
      );
    }

    // Switcheroo
    new_code = std::mem::replace(&mut self.code, new_code);
    for i in new_encoded {
      self.emit_bytecode(i);
    }

    self.code.append(&mut new_code);
    if to_assembly{
      new_assembly.push_str(self.assembly_buffer.as_str());
      self.assembly_buffer = new_assembly;
    }
  }

  /**
    Compiles a single  query or procedure by compiling the
    provided flattened term into its in-memory representation.

    This function assumes that the term has been prepared with `flatten_term`.
  */
  fn compile_tokens(
      &mut self, ast: &Term,
      is_program: bool,
      to_assembly: bool
    )
  {

    // Flatten and order the terms, converting to `Cell`s in the process.
    let (cell_vec, mut order, mut vars) = flatten_term(ast);
    // Programs are ordered reverse of queries.
    if is_program {
      order.reverse();
    } else{
      // Store variable bindings for a query.
      self.variable_bindings.append(&mut vars);
    }

    // Contains the register arguments we've seen before.
    let seen: RefCell<HashSet<Address>> = RefCell::new(HashSet::with_capacity(order.len()));

    // A helpful auxiliary so we don't have to repeat ourselves, capturing `is_program` and `seen`
    let match_address_to_instruction = |address: &Address| -> Instruction{
      let was_seen = seen.borrow().contains(&address);
      match was_seen {
        true  => {
          // Already saw this register.
          match is_program {
            // Program
            true     => Instruction::Unary {
              opcode  : Operation::UnifyValue,
              argument: Argument::Address(*address)
            },
            // Query
            false   =>  Instruction::Unary {
              opcode  : Operation::SetValue,
              argument: Argument::Address(*address)
            }
          }
        }

        false => {
          // Have not seen this register before.
          seen.borrow_mut().insert(*address);
          match is_program {
            // Program
            true     => Instruction::Unary {
              opcode  : Operation::UnifyVariable,
              argument: Argument::Address(*address)
            },
            // Query
            false    => Instruction::Unary {
              opcode  : Operation::SetVariable,
              argument: Argument::Address(*address)
            }
          } // end match is_program
        }
      } // end if seen address before
    };

    /*
      Bookkeeping for the outermost term, which defines the procedure/query. The first
      instruction we are about to push onto `self.code` will be the first instruction of
      `procedure`. No checking is done to see if there already is a procedure with this name.
    */
    let procedure_address = Address::Code(self.code.len() as AddressNumberType);
    let functor: Functor  = cell_vec[order[0]].extract_functor().unwrap();
    // The first cell is guaranteed to be a `Cell::Structure`, so `unwrap()` is safe.
    self.labels.push((functor.clone(), procedure_address));

    if to_assembly {
      match is_program{
        true  => self.assembly_buffer.push_str(format!("% Procedure {}\n", functor).as_str()),
        false => self.assembly_buffer.push_str(format!("% Query\n").as_str()),
      }
    }

    // We iterate over the tokens in the registers in `order`.
    for index in order {
      let term = &cell_vec[index];
      match &term {

        Cell::Structure(args) => {
          let register_address  = Address::from_reg_idx(index);
          let functor: Functor  = term.extract_functor().unwrap();
          // Record functor so its name can be reconstituted from bytecode.
          intern_functor(&functor);
          seen.borrow_mut().insert(register_address);

          let mut instruction =
            match is_program {
              // Program
              true     => Instruction::Binary {
                opcode  : Operation::GetStructure,
                address : register_address,
                argument: Argument::Functor(functor)
              },
              // Query
              false    => Instruction::Binary {
                opcode  : Operation::PutStructure,
                address : register_address,
                argument: Argument::Functor(functor)
              }
            }; // end match is_program
          let encoded_instruction = instruction.encode_instruction();
          self.emit_bytecode(encoded_instruction);
          if to_assembly {
            self.emit_assembly(&instruction, &term, &encoded_instruction);
          }

          // Now iterate over the structure's arguments.
          for arg in args[1..].iter(){
            let address = arg.extract_address().unwrap();
            instruction = match_address_to_instruction(&address);
            let encoded_instruction = instruction.encode_instruction();
            self.emit_bytecode(encoded_instruction);
            if to_assembly {
              self.emit_assembly(&instruction, &term, &encoded_instruction);
            }
          }

        } // end is Structure ("Assignment" in [Aït-Kaci])

        Cell::REF(address) => {
          let instruction = match_address_to_instruction(&address);
          let encoded_instruction = instruction.encode_instruction();
          self.emit_bytecode(encoded_instruction);
          if to_assembly {
            self.emit_assembly(&instruction, &term, &encoded_instruction);
          }
        }

        _ => {
          unreachable!();
        }
      } // end match on token type

    } // end iterate over tokens

    // For now, we end query construction with a `Proceed` as well.
    let instruction = Instruction::Nullary(Operation::Proceed);
    let encoded_instruction = instruction.encode_instruction();
    self.emit_bytecode(encoded_instruction);
    if to_assembly {
      self.assembly_buffer.push_str(
        format!("{:30}% {:>11} End of atom\n",
                format!("{}", instruction),
                format!("{}", encoded_instruction),
        ).as_str()
      );
    }

  }

  /// Constructs the compilation object and immediately passes
  /// control to `compile_driver`, which does the real work.
  pub fn from_assemble(text: &str) -> Option<Compilation>{
    let mut compilation = Compilation{
      code            : Vec::new(),
      labels          : Vec::new(),
      assembly_buffer : "".to_string(),
      variable_bindings: Vec::new(),
    };

    match compilation.assembler_driver(text) {

      Ok(())  => Some(compilation),

      Err(()) => None

    }

  }

  /**
    Produces human readable assembly form the the bytecode that `text`
    compiles to. The binary bytecode is produced as well as a side-effect.

    If assembly fails, a vector of error `AssemblySyntax` variants is returned.
  */
  fn assembler_driver(&mut self, text: &str) -> Result<(), ()>{
    let compilation_time =  std::time::Instant::now();
    let assembly         =  match parse_assembly(text){
                              Ok(results) => results,
                              Err(error)  => {
                                vec![AssemblySyntax::FatalParseError(error)]
                              }
                            };

    let mut instructions: Vec<Instruction> = Vec::with_capacity(assembly.len());
    let mut success     : bool             = true;

    for syntax in assembly {
      match syntax {

        AssemblySyntax::Instruction(instruction) if success => {
          instructions.push(instruction);
        }

        error_syntax => {
          eprintln!("{}", error_syntax);
          success = false;
        }

      }
    }

    match success {

      true => {
        for instruction in instructions {
          self.emit_bytecode(instruction.encode_instruction());
        }
        println!("Compiled to {} bytes of bytecode in {:?}.",
                 self.code.len() * 4, compilation_time.elapsed());
        Ok(())
      } // end if succeeded

      false => Err(())

    }
  }

  fn emit_assembly(&mut self, instruction: &Instruction, _cell: &Cell,
                   encoded_instruction: &Bytecode){
    let decoded = match encoded_instruction.try_decode(){
      Some(i) => format!("{}", i),
      None => "Could not decode.".to_string()
    };
    self.assembly_buffer.push_str(
      format!("{:30}% {:>11} {}\n",
              format!("{}", instruction),
              format!("{}", encoded_instruction),
              decoded
      ).as_str()
    );
  }

  /// This method only inserts bytes into `self.code`.
  fn emit_bytecode(&mut self, instruction: Bytecode){
    match instruction {

      Bytecode::Word(word)              => {
        self.code.push(word)
      },

      Bytecode::DoubleWord(double_word) => {
        let words: DoubleWord;
        unsafe {
          words = std::mem::transmute(double_word);
        }
        self.code.push(words.low);
        self.code.push(words.high);
      } // end match DoubleWord
    } // end match instruction
  } // end emit_bytecode
}


/**
  Computes the flattened form expressed of the given Term AST.

  Example:
    The registers containing the flattened form of `p(Z, h(Z, W), f(W))` are
      ```
      X1 = p(X2, X3, X4)
      X2 = Z
      X3 = h(X2, X5)
      X4 = f(X5)
      X5 = W
      ```

  Note that the variable registers may be omitted without loosing semantic meaning. However, we
  record the variable bindings so that we can report what each variable is ultimately bound to
  upon successful unification.

  Despite appearances, we can't simultaneously compile the term, because the flattened form needs
  to be ordered in a particular way for compilation.
*/
pub fn flatten_term(term: &Term) -> (CellVec, Vec<usize>, Vec<(DefaultAtom, Address)>){
  let mut seen: HashMap<&Term, usize> = HashMap::new();
  let mut vars: Vec<(DefaultAtom, Address)> = Vec::new();

  // We visit the AST breadth first, adding new symbols to `seen` as we go. This assigns each
  // term its own register.
  let terms: TermIter = TermIter::new(term);
  for term in terms{
    let new_address = seen.len();
    seen.entry(&term).or_insert(new_address);
  }

  let mut registers = CellVec::with_capacity(seen.len());
  registers.resize(seen.len(), Cell::Empty);

  // Every term has a register assignment. Now populate the "registers".
  for (&term, register) in seen.iter(){
    match term{

      Term::Predicate { functor, args } => {
        // Create a vector of cells out of the argument terms, translating
        // those terms into `Cell::REF`'s that point to the register assigned
        // to the terms.
        let mut new_args = CellVec::with_capacity(args.len() + 1);
        new_args.push(Cell::Functor(functor.clone()));
        new_args.extend(args.iter().map(
          |t| Cell::REF(
            Address::from_reg_idx(*seen.get(t).unwrap())
          )
        ));
        registers[*register] = Cell::Structure(new_args);
      },

      Term::Variable(name) =>{
        let address = Address::from_reg_idx(*register);
        vars.push((name.clone(), address.clone()));
        registers[*register] = Cell::REF(address);
      }

      _t => {
        // This should never happen in correct code during compilation.
        panic!("Error: Illegal term encountered: {}", _t);
      }

    };
  }

  let order = order_registers(&registers);

  // order
  (registers, order, vars)
}


/**
  Orders the registers of a flattened term so that registers are assigned
  to before they are used (appear on the RHS of an assignment). This
  function assumes that the registers already contain a flattened term.

  > [F]or left-to-right code generation to be well-founded, it is necessary
  > to order a flattened query term to ensure that a register name may
  > not be used in the right-hand side of an assignment (viz., as a subterm)
  > before its assignment, if it has one (viz., being the left-hand side).
*/
fn order_registers(flat_terms: &CellVec) -> Vec<usize>{
  // Contains the register arguments we've seen before.
  let mut seen    : HashSet<Address> = HashSet::with_capacity(flat_terms.len());
  let mut unseen  : HashSet<usize>   = HashSet::new();
  // The vector holding the result
  let mut ordered : Vec<usize>       = Vec::with_capacity(flat_terms.len());

  // First collect which registers need to be ordered, marking registers containing variables
  // as seen. At the end, `unseen` is all `Cell::Structure`s, while `seen` is all `Cell::REF`s.
  for (index, cell) in flat_terms.iter().enumerate(){
    match cell {

      Cell::REF(address) => {
        // Once a term is flattened, the variables in the term can be replaced with the
        // register associated to the variable. Thus, we ignore variables.
        seen.insert(*address);
      },

      Cell::Structure(_) =>{
        unseen.insert(index);
      },

      _t => {
        unreachable!(
          "Error: Encountered a non-variable/non-struct cell after flattening a term: {}.",
          _t
        );
      }

    }
  }

  // Now try to order the registers to maintain the invariant in the doc string.
  // Limit to a reasonable maximum number of loops to prevent infinite looping.
  for _loop_count in 0..MAXIMUM_ORDER_ATTEMPTS {
    let next_regs: HashSet<usize> =
    unseen.iter().filter_map(
      |index| {
        match flat_terms[*index].extract_arg_addresses() {

          Some(address_set)
          if address_set.is_subset(&seen) => {
            Some(*index)
          }

          _v => {
            None
          }

        }
      }
    ).collect();

    for reg in next_regs{
      ordered.push(reg);
      unseen.remove(&reg);
      seen.insert(Address::from_reg_idx(reg));
    }

    if unseen.is_empty() {
      break;
    }
  }

  ordered
}
