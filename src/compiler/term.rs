//! The abstract syntax tree type for programs and queries.

use std::collections::{VecDeque, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use string_cache::DefaultAtom;

use crate::functor::*;
use crate::address::*;
use crate::cell::*;

pub type RcTerm = Rc<Term>;
pub type TermVec = Vec<Term>;


const MAXIMUM_ORDER_ATTEMPTS: usize = 200;

// region Term declarations and definitions

/// Abstract representation
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
  /// An interned string staring with an uppercase letter.
  Variable(DefaultAtom),
  /// Structures hold functors. Constants are represented as functors of arity 0, so `args` might
  /// be an empty `Vec`.
  Structure {
    functor : Functor,
    args    : TermVec
  },
  /// Denoted `?-q` ("What `q`"), a top-level term for queries. Anything else is a program.
  Query(RcTerm),
  /// Flattened terms only have registers for arguments.
  Ref(Address)
}

impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"", &""))
  }
}

impl Term{

  /// Returns a string representation of the term, e.g. `"h(f(f(a)), f(a))"`.
  pub fn as_expression_string(&self) -> String{
    match self {

      Term::Structure{ functor, args } => {
        if args.is_empty() {
          format!("{}", functor.name)
        }else {
          let mut buffer = format!("{}(", functor.name);
          for (i, term) in args.iter().enumerate(){
            buffer.push_str(term.as_expression_string().as_str());
            if i != args.len() - 1 {
              buffer.push_str(", ");
            }
          }
          buffer.push(')');
          buffer
        }
      }

      Term::Variable(name) => name.to_string(),

      Term::Ref(address) => address.to_string(),

      _ => "??".to_string()
    }
  }

  fn fmt_aux(&self, prefix: &str, child_prefix: &str) -> String {
    match self {
      Term::Structure{ functor, args } => {
        let mut buffer =
          if args.is_empty() {
            format!("{}Constant<{}>", prefix, functor.name)
          } else{
            format!("{}Functor<{}(…)>\n", prefix, functor)
          };

        for (i, next) in args.iter().enumerate() {
          if i != args.len() - 1 {
            buffer.push_str(
              next.fmt_aux(
                &format!("{}{}", child_prefix, "            ├── "),
                &format!("{}{}", child_prefix, "            │   ")
              ).as_str()
            );
            buffer.push('\n');
          } else {
            buffer.push_str(
              next.fmt_aux(
                &format!("{}{}", child_prefix, "            └── "),
                &format!("{}{}", child_prefix, "                ")
              ).as_str()
            );
          }
        };
        buffer
      }
      Term::Variable(c) => {
        format!("{}Variable<{}>", prefix, c)
      },
      Term::Query(inner) => {
        format!("{}Query<{}>", prefix, inner)
      },
      Term::Ref(address) => {
        format!("{}Reference<{}>", prefix, address)
      }
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
  pub fn flatten_term(&self) -> (CellVec, Vec<usize>){
    let mut seen: HashMap<&Term, usize> = HashMap::new();

    // We visit the AST breadth first, adding new symbols to `seen` as we go. This assigns each
    // term its own register.
    let terms: TermIter = TermIter::new(self);
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
        },

        Term::Variable(_) =>{
          registers[*register] = Cell::REF(Address::from_reg_idx(*register));
        }

        _t => {
          // This should never happen in correct code during compilation.
          panic!("Error: Illegal term encountered: {}", _t);
        }

      };
    }


    let order = Self::order_registers(&registers);

    // order
    (registers, order)
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

}


// endregion Term

// region TermIter declarations and definitions

#[derive(Debug)]
pub struct TermIter<'t >{
  terms: VecDeque<&'t Term>, // A stack of terms yet to be visited.
}

impl<'t> TermIter<'t>{
  pub fn new(start: &'t Term) -> TermIter {
    TermIter{
      terms: VecDeque::from(vec![start])
    }
  }
}

/// Iterates over the terms in the term tree breadth first.
impl<'t> Iterator for TermIter<'t>{
  type Item = &'t Term;

  fn next(&mut self) -> Option<Self::Item> {
    let option_term = self.terms.pop_front();
    match option_term {

      Some(term) => {
        match term {

          Term::Structure{args, ..} => {
            self.terms.extend(args.iter());
            option_term
          },

          _t => option_term
        }
      }

      None => None
    }
  }
}

// endregion TermIter
