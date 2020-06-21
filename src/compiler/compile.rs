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
use std::collections::HashSet;

use crate::address::*;
use crate::cell::*;
use crate::functor::*;
use crate::bytecode::*;
use super::term::*;
use crate::compiler::parser::parse as parse_source_code;
use crate::functor::intern_functor;

/// A `Compilation` is the result of executing `Compilation::compile(source)` and may contain
/// binary bytecode, assembly code, and/or line labels.
pub struct Compilation{
  pub code            : Vec<Word>,  // Code memory, a binary memory store
  // Symbol table mapping line labels to their address in code memory.
  pub labels          : Vec<(Functor, Address)>,
  pub assembly_buffer : String, // String buffer for emitted code text
}

impl Compilation {

  /// Constructs the compilation object and immediately passes
  /// control to `compile_driver`, which does the real work.
  //  ToDo: In case of parse error, should we return an error, a partially constructed object, `None`?
  pub fn compile(text: &str, to_assembly: bool) -> Option<Compilation>{
    let mut compilation = Compilation{
      code            : Vec::new(),
      labels          : Vec::new(),
      assembly_buffer : "".to_string()
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
    let (atoms, queries) =
      match parse_source_code(text){
        Ok(tuple) => tuple,
        Err(_) => {
          eprintln!("{}", "Compilation failed.");
          return Err(())
        }
      };

    // Queries
    for ast in queries{
      self.compile_tokens(&ast, false, to_assembly);
    }
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
          encode_instruction(&instruction)
        })
    );
    // Finally, a `Halt`
    let halt = Instruction::Nullary(Operation::Halt);
    new_encoded.push(encode_instruction(&halt));
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
    let (cell_vec, mut order): (CellVec, Vec<usize>) = ast.flatten_term();
    // Programs are ordered reverse of queries.
    if is_program {
      order.reverse();
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

    // Bookkeeping for the outermost term, which defines the procedure/query. The first
    // instruction we are about to push onto `self.code` will be the first instruction of
    // `procedure`. No checking is done to see if there already is a procedure with this name.
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
          let encoded_instruction = encode_instruction(&instruction);
          self.emit_bytecode(encoded_instruction);
          if to_assembly {
            self.emit_assembly(&instruction, &term, &encoded_instruction);
          }

          // Now iterate over the structure's arguments.
          for arg in args[1..].iter(){
            let address = arg.extract_address().unwrap();
            instruction = match_address_to_instruction(&address);
            let encoded_instruction = encode_instruction(&instruction);
            self.emit_bytecode(encoded_instruction);
            if to_assembly {
              self.emit_assembly(&instruction, &term, &encoded_instruction);
            }
          }

        } // end is Structure ("Assignment" in [Aït-Kaci])

        Cell::REF(address) => {
          let instruction = match_address_to_instruction(&address);
          let encoded_instruction = encode_instruction(&instruction);
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
    let encoded_instruction = encode_instruction(&instruction);
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
      assembly_buffer : "".to_string()
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
          self.emit_bytecode(encode_instruction(&instruction));
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
    self.assembly_buffer.push_str(
      format!("{:30}% {:>11}\n",
              format!("{}", instruction),
              format!("{}", encoded_instruction),
              // cell,
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
