/*!
  The tokenizer takes the AST and translates it into a token stream that can be processed to turn
   into a series of instructions or an in-memory (cell) representation.

   The AST is first translated to a tree of `Cell`s so that the parser can also be used to write
   expressions directly into memory. Thus, a `Tokenizer` is also a translator from `Cell`s to
   `Token`s.
*/


use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::address::Address;
use crate::cell::{Cell, CellVec, extract_addresses, RcCell};
use crate::term::*;
use crate::functor::Functor;

const MAXIMUM_ORDER_ATTEMPTS: usize = 200;

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Token{
  Register(Address),
  Assignment(Functor, Address)
}

impl Display for Token{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let str_rep = match self{

      Token::Register(address) => {
        format!("{}", address)
      }

      Token::Assignment(functor, address) => {
        format!("{}={}", address, functor)
      }

    };
    write!(f, "{}", str_rep)
  }
}


/**
  Computes the flattened form expressed of the given Term AST. The flattened form
  of the AST contains only the non-variable registers.

  Example:
    The registers containing the flattened form of `p(Z, h(Z, W), f(W))` are
      ```
      X1 = p(X2, X3, X4)
      X2 = Z
      X3 = h(X2, X5)
      X4 = f(X5)
      X5 = W
      ```

  Note that the variable registers may be omitted without loosing
  semantic meaning. We can't simultaneously compile the term,
  because the flattened form needs to be ordered in a particular way.
*/
pub fn flatten_term(ast: &RcTerm) -> (CellVec, Vec<Address>){
  let mut seen: HashMap<RcTerm, Address> = HashMap::new();

  // We visit the AST breadth first, adding new symbols to `seen` as we go. This assigns each
  // term its own register.
  let terms: TermIter = TermIter::new(&ast);
  for term in terms{
    let default = Address::from_reg_idx(seen.len());
    seen.entry(term.clone()).or_insert(default);
  }

  let mut registers = Vec::<RcCell>::new();
  registers.resize(seen.len(), Rc::new(Cell::Empty));

  // Every term has a register assignment. Now populate the "registers".
  for (term, reg_ptr) in seen.iter(){
    match &**term{

      Term::Structure{functor, args} => {
        // Trying to be a bit too clever, probably.
        let mut new_args =
          args.iter()
              .map(|t| Rc::new(Cell::REF(*seen.get(t).unwrap())))
              .collect::<Vec<RcCell>>();
        new_args.insert(0, Rc::new(Cell::Functor(functor.clone())));
        registers[reg_ptr.idx()] = Rc::new(Cell::Structure(new_args.into()));
      },

      Term::Variable(_) =>{
        registers[reg_ptr.idx()] = Rc::new(Cell::REF(*reg_ptr));
      }

      _t => {
        // This should never happen in correct code during compilation but may be useful for
        // other reasons, e.g., testing.
        registers[reg_ptr.idx()] = Rc::new(Cell::Term(term.clone()));
      }

    };
  }

  let rc_registers: CellVec = registers.into();
  let order = order_registers(rc_registers.clone());

  // order
  (rc_registers, order)

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
pub fn order_registers(flat_terms: CellVec) -> Vec<Address>{
  // Contains the register arguments we've seen before.
  let mut seen: HashSet<Address> = HashSet::new();
  let mut unseen: HashSet<Address> = HashSet::new();
  // The vector holding the result
  let mut ordered: Vec<Address> = Vec::new();

  // First collect which registers need to be ordered, marking registers containing variables
  // as seen. At the end, `unseen` is all `Cell::Structure`s, while `seen` is all `Cell::REF`s..
  for (index, cell) in flat_terms.iter().enumerate(){
    match &**cell {

      Cell::REF(address) => {
        // Once a term is flattened, the variables in the term can be replaced with the
        // register associated to the variable. Thus, we ignore variables.
        seen.insert(*address);
      },

      Cell::Structure(_) =>{
        unseen.insert(Address::from_reg_idx(index));
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
    let next_regs: HashSet<Address> =
      unseen.iter().filter_map(
        |address| {
          match extract_addresses(flat_terms[address.idx()].clone()) {

            Some(address_set)
            if address_set.is_subset(&seen) => {
              Some(*address)
            }

            _v => {
              None
            }

          }
        }
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
  A `cell::Tokenizer` produces a stream of `Tokens` that can be used to build a sequence of
  instructions or an in-memory representation of the term it tokenizes.

  Note that a `Tokenizer` cannot process `Term::Program` and `Term::Query` terms. Those should be
   stripped before creating a Tokenizer.
*/
#[derive(Clone)]
pub struct Tokenizer{
  outer_index: usize,
  inner_index: usize,
  #[cfg(feature = "trace_computation")]
  pub cell_vec: CellVec,
  #[cfg(not(feature = "trace_computation"))]
  cell_vec: CellVec,
  #[cfg(feature = "trace_computation")]
  pub order: Vec<Address>,
  #[cfg(not(feature = "trace_computation"))]
  order: Vec<Address>,
}

impl Tokenizer{

  /**
    Creates a new `Tokenizer` out of a term in textual form. It tokenizes in the appropriate
    order according to `is_program`. (Program terms must be the reverse of query terms.) It
    assumes that the leading `"?-"` is trimmed from queries.
  */
  pub fn new(ast: &RcTerm, is_program: bool) -> Self{

    // Flatten and order the terms, converting to `Cell`s in the process.
    let (cell_vec, mut order) = flatten_term(ast);
    // Programs are ordered reverse of queries.
    if is_program {
      order.reverse();
    }

    Tokenizer{
      // The next available index
      outer_index: 0,
      inner_index: 0,
      cell_vec,
      order
    }
  }
}

impl Iterator for Tokenizer{
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    match self.outer_index < self.order.len() {
      false => None,
      true => {
        match &*self.cell_vec[self.order[self.outer_index].idx()]{

          Cell::Structure(args) => {
            match self.inner_index < args.len() {

              true => {
                match &*args[self.inner_index].clone() {

                  Cell::Functor(functor) => {
                    self.inner_index += 1;
                    Some(Token::Assignment(functor.clone(), self.order[self.outer_index]))
                  }

                  Cell::REF(address) => {
                    self.inner_index += 1;
                    Some(Token::Register(*address))
                  }

                  _t => {
                    unreachable!("Error: Non-reference found in a structure argument: {}", _t);
                  }

                }
              }

              false => {
                self.inner_index = 0;
                self.outer_index += 1;
                self.next()
              }

            } // end if more args
          } // end if structure

          Cell::Functor(functor) => {
            // There should be only structures pointed to by addresses in `order`, but we allow
            // it in case it's useful elsewhere.
            Some(Token::Assignment(functor.clone(), Address::from_reg_idx(self.outer_index)))
          }

          _t => {
            unreachable!("Error: Non-reference found in register while tokenizing: {}", _t);
          }

        } // end match on outer cell
      }
    } // end if more outer cells (registers)
  }
}
