//! The abstract syntax tree type for programs and queries.

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;


use crate::functor::Functor;
use crate::address::Address;

pub type RcTerm = Rc<Term>;
pub type TermVec = Rc<Vec<Rc<Term>>>;

// region Term declarations and definitions

/// Abstract representation
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
  /// An uppercase letter.
  Variable(char),
  Structure {
    functor: Functor,
    args: TermVec
  },
  /*
  /// Top-most term for non-queries.
  Program(RcTerm),
  /// Denoted `?-q` ("What `q`"), a non-variable term.
  Query(RcTerm),
  */
  /// An X followed by a number, used only internally to process terms.
  Register(Address),
  /// Structures hold functors. Constants are represented as functors of arity 0, so `args` might
  /// be an empty `Vec`.
  /// Epsilon is the empty Term, used only to indicate that there was nothing to parse.
  Empty
}

impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"".to_string(), &"".to_string()))
  }
}

impl Term{

  fn fmt_aux(&self, prefix: &String, child_prefix: &String) -> String {
    match self {
      Term::Structure{ functor, args } => {
        let mut buffer =
          if functor.is_constant() {
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
      },
      Term::Empty => {
        format!("{}ε", prefix)
      },
      Term::Register(i) => {
        format!("{}Register<{}>", prefix, i)
      },
      Term::Variable(c) => {
        format!("{}Variable<{}>", prefix, c)
      },
      /*
      Term::Program(term) => {
        format!("Program\n-------\n{}", term)
      },
      Term::Query(term) => {
        format!("Query\n-------\n{}", term)
      }

      */
    } // end match self
  }

}

// endregion Term

// region TermIter declarations and definitions

#[derive(Debug)]
pub struct TermIter{
  terms: VecDeque<RcTerm>, // A stack of terms/tokens yet to be visited.
}

impl TermIter{
  pub fn new(start: &RcTerm) -> TermIter {
    TermIter{
      terms: VecDeque::from(vec![start.clone()])
    }
  }
}

/// Iterates over the terms in the term tree breadth first.
impl Iterator for TermIter{
  type Item = RcTerm;

  fn next(&mut self) -> Option<Self::Item> {
    let current = self.terms.pop_front();
    match &current {
      Some(rc) => {
        match &**rc{
          Term::Structure{args, ..} => {
            for term in args.iter(){
              self.terms.push_back(term.clone());
            };
            current
          },
          _t => {
            current
          }
        }
      },
      None => {
        None
      }
    }
  }
}

// endregion TermIter

#[cfg(test)]
mod tests {
  pub fn make_struct(name: char, v: &TermVec) -> RcTerm {
    let new_vec = v.clone();

    let arity = new_vec.len() as ArityType;

    Rc::new(Term::Structure {
      functor: Functor { name, arity },
      args: new_vec
    })
  }

  pub fn new_struct_of_term(name: char, term: &RcTerm) -> RcTerm {
    Rc::new(Term::Structure {
      functor: Functor { name, arity: 1 },
      args: term.to_term_vec()
    })
  }
}
