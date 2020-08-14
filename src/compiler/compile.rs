/*!
  Functions to produce a compilation artifact from source code input. Accepts a program and/or query
  as a string and turns it into a sequence of operations, an in-memory representation, and/or
  assembly source code.

  The compilation pipeline is:
  ```
  text -> [`parser::parse`] -> `Term`s ->⋯

      ┌───────────────[`token::flatten_term`]────────────────┐
  ⋯->*│*-> [`TermIter`] -> `Cell`s -> [`order_registers`] ->*│*->⋯
      └──────────────────────────────────────────────────────┘

  ⋯-> [`compile_term`] -> `Instruction`s -> bytecode/assembly
  ```
*/

use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::intrinsics::size_of;

use string_cache::DefaultAtom;

use crate::address::*;
use crate::cell::*;
use crate::functor::*;
use crate::bytecode::*;
use super::term::*;
use crate::compiler::parser::parse as parse_source_code;
use crate::functor::intern_functor;


pub const MEMORY_PAGE_SIZE       : usize = 4096;
pub const CODE_MEMORY_SIZE       : usize = MEMORY_PAGE_SIZE / std::mem::size_of::<Word>() ;
const     MAXIMUM_ORDER_ATTEMPTS : usize = 200;
const     ENVIRONMENT_HEADER     : usize = 3;


/// A `Compilation` is the result of executing `Compilation::compile(source)` and may contain
/// binary bytecode, assembly code, variable bindings, and/or line labels.
pub struct Compilation{
  pub code              : Vec<Word>,  // Code memory, a binary memory store
  // Symbol table mapping line labels to their address in code memory.
  pub labels            : Vec<(Functor, Address)>,
  pub query_label       : Address,
  pub assembly_buffer   : String,     // String buffer for emitted code text
  pub query_variables   : Vec<(DefaultAtom, Address)>,
  to_assembly           : bool
}


impl Compilation {

  /*
    Constructs the compilation object from the given source text. First, the text is parsed,
    transforming it to an AST of `Terms`. The the AST is then passed to `compile_driver`, which does
    the compilation.
  */
  pub fn compile(text: &str, to_assembly: bool) -> Option<Compilation>{
    let compilation_time = std::time::Instant::now();

    // Parse the text into a `Term`, which is a tree structure in general.
    let (clauses, maybe_query) =
      match parse_source_code(text){

        Ok(tuple) => tuple,

        Err(_) => {
          // Specific error messages will have already been emitted.
          eprintln!("{}", "Compilation failed.");
          return None;
        }

      };

    let compilation = Compilation::new_empty(to_assembly);
    let result = compilation.compile_driver(clauses, maybe_query);
    match result {

      Ok(_)  => {
        println!("Compiled to {} bytes of bytecode in {:?}.\n",
                 compilation.code.len()*4, compilation_time.elapsed());

        #[cfg(feature = "trace_computation")]
        {
          if to_assembly {
            println!("% Assembly Code Instructions\n{}\n", compilation.assembly_buffer);
          }
        };

        Some(compilation)
      }

      Err(_) => None

    }
  }


  fn new(to_assembly: bool) -> Compilation{
    Compilation{
      code              : Vec::with_capacity(CODE_MEMORY_SIZE),
      labels            : Vec::new(),
      query_label       : Address::from_code_idx(0), // Arbitrary placeholder
      assembly_buffer   : "".to_string(),
      query_variables   : Vec::new(),
      to_assembly
    }
  }


  /*
    For a clause of the form `pred(a1, a2, ..., an) :- ...`, the clause forms a procedure, so we
    label the head. The first instruction we are about to push onto `self.code` will be the first
    instruction of the procedure. No checking is done to see if there already is a procedure with
    this name.
  */
  fn label_predicate(&mut self, head: &Term){
    match head {

      Term::Structure{ functor, ..} => {
        let procedure_address = Address::Code(self.code.len() as AddressNumberType);
        self.labels.push((functor.clone(), procedure_address));
      }

      _ => {
        // Only `Structures` that are heads of a `Rule` need labeling
        return;
      }

    }
  }


  /**
    The compile driver is responsible for
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
  fn compile_driver(&mut self, clauses: &TermVec, maybe_query: Option<&Term>) -> Result<(), ()>{

    // Programs
    for clause in clauses {
      match clause {

        Term::Rule {head, goals} => {
          self.label_predicate(head);
          let mut variables: HashMap<DefaultAtom, (bool, bool)> =
            clause.variables().drain().map(|k, v| (k, (v, false))).collect();
          // Emit allocate
          instruction = Instruction::Unary {
            opcode: Operation::Allocate,
            argument: Argument::Word(variables.len() as Word)
          };
          self.compile_term(&head, &variables, true);
          for goal in goals{
            // Goals are compiled exactly like queries, so `is_program` is false.
            self.compile_term(&goal, &variables, false);
          }
        }

        Term::Structure {functor, args} => {
          // A fact.
          self.label_predicate(clause);
          let variables = HashMap::new();
          self.compile_term(clause, &variables, true);
        }

        _ => {
          let variables = HashMap::new();
          self.compile_term(clause, &variables, true);
        }

      }
    }

    // Record the location of the start of the query. Conveniently, one can check for the
    // presence of a query by checking that `self.query_label != self.code.len()`.
    self.query_label = Address::from_code_idx(self.code.len());

    // Query
    if let Some(query @ &Term::Query(goals)) = maybe_query {
      let variables = query.variables();
      // The compilation of a query is the concatenation of the compilation of its predicates.
      for goal in goals {
        self.compile_term(&goal, &variables, false);
      }
    }


    Ok(())
  }

  /**
    Constructs the appropriate instruction according to whether the address has been seen before.
  */
  fn address_to_instruction(
    seen: &RefCell<HashSet<Address>>,
    address: &Address,
    is_program: bool
  )
    -> Instruction
  {
    let was_seen = seen.borrow().contains(&address);
    match was_seen {
      true => {
        // Already saw this register.
        match is_program {

          // Program
          true => Instruction::Unary {
            opcode: Operation::UnifyValue,
            argument: Argument::Address(*address),
          },

          // Query
          false => Instruction::Unary {
            opcode: Operation::SetValue,
            argument: Argument::Address(*address),
          }
        }
      }

      false => {
        // Have not seen this register before.
        seen.borrow_mut().insert(*address);
        match is_program {

          // Program
          true => Instruction::Unary {
            opcode: Operation::UnifyVariable,
            argument: Argument::Address(*address),
          },

          // Query
          false => Instruction::Unary {
            opcode: Operation::SetVariable,
            argument: Argument::Address(*address),
          }
        } // end match is_program
      }
    } // end match seen address before
  }

  /**
    Compiles a single predicate or variable by compiling the provided flattened term into its
    in-memory representation.

    `Rule`s and `Query`s are compiled by the compile driver, where the variables in scope are
    determined.
  */
  fn compile_term(
      &mut self,
      ast: &Term,                             // The term being compiled
      variables: &HashMap<DefaultAtom, bool>, // The temporary and permanent variables in scope
      is_program: bool,                       // Whether the term should be compiled as a query
    )
  {

    // Flatten and order the terms, converting to `Cell`s in the process.
    let (cell_vec, mut order) = self.flatten_term(ast);
    // Programs are ordered reverse of queries.
    if is_program {
      order.reverse();
    } else{
      // Store variable bindings for a query.
      // self.query_variables.append(&mut vars);
    }

    // Contains the register arguments we've seen before.
    let seen: RefCell<HashSet<Address>> = RefCell::new(HashSet::with_capacity(order.len()));
    


    // Assembly listing header
    if self.to_assembly {
      match is_program{

        true  => self.assembly_buffer.push_str(format!("% Procedure {}\n", functor).as_str()),

        false => self.assembly_buffer.push_str(format!("% Query\n").as_str()),

      }
    }

    // We iterate over the terms in the registers in `order`.
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
          self.emit(&instruction);

          // Now iterate over the structure's arguments.
          for arg in args[1..].iter(){
            let address = arg.extract_address().unwrap();
            instruction = Self::address_to_instruction(&seen, &address, is_program);
            self.emit(&instruction);
          }

        } // end is Structure ("Assignment" in [Aït-Kaci])

        Cell::REF(address) => {
          let instruction = Self::address_to_instruction(&seen, &address, is_program);
          self.emit(&instruction);
        }

        _ => {
          unreachable!();
        }

      } // end match on token type

    } // end iterate over tokens

    // POSTAMBLE
    let instruction = Instruction::Nullary(Operation::Proceed);
    self.emit(&instruction);
  }


  /// Constructs the compilation object and immediately passes
  /// control to `assembler_driver`, which does the real work.
  pub fn from_assemble(text: &str) -> Option<Compilation>{
    let mut compilation = Compilation{
      code              : Vec::with_capacity(CODE_MEMORY_SIZE),
      labels            : Vec::new(),
      query_label       : Address::from_code_idx(0), // Arbitrary placeholder
      assembly_buffer   : "".to_string(),
      query_variables: Vec::new(),
      to_assembly       : false
    };

    match compilation.assembler(text) {

      Ok(())  => Some(compilation),

      Err(()) => None

    }
  }


  /**
    Produces human readable assembly form the the bytecode that `text`
    compiles to. The binary bytecode is produced as well as a side-effect.

    If assembly fails, a vector of error `AssemblySyntax` variants is returned.
  */
  fn assembler(&mut self, text: &str) -> Result<(), ()>{
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
          self.emit_bytecode(&instruction);
        }
        println!("Compiled to {} bytes of bytecode in {:?}.",
                 self.code.len() * 4, compilation_time.elapsed());
        Ok(())
      } // end if succeeded

      false => Err(())

    }
  }


  fn emit_assembly(&mut self, instruction: &Instruction){
    // We use this to debug the instruction codec. They should round trip.
    let encoded_instruction = instruction.bytecode();
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


  fn emit(&mut self, instruction: &Instruction){
    self.emit_bytecode(instruction);
    if self.to_assembly {
      self.emit_assembly(instruction);
    }
  }


  fn emit_bytecode(&mut self, instruction: &Instruction){
    let bytecode = instruction.bytecode();
    match bytecode {

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
  fn flatten_term(&mut self, term: &Term) -> (CellVec, Vec<usize>){
    let mut seen: HashMap<&Term, usize> = HashMap::new();
    // let mut vars: Vec<(DefaultAtom, Address)> = Vec::new();

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

        Term::Structure { functor, args } => {
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
        }

        Term::Variable(name) =>{
          let address = Address::from_reg_idx(*register);
          self.query_variables.push((name.clone(), address.clone()));
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
    (registers, order)
  }

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
      }

      Cell::Structure(_) =>{
        unseen.insert(index);
      }

      _t => {
        unreachable!(
          "Error: Encountered a non-variable/non-struct cell after flattening a term: {}.",
          _t
        );
      }
    } // end match
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
