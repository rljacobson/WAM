//! The abstract syntax tree type for programs and queries.
#![allow(dead_code)]

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use string_cache::DefaultAtom;

use crate::functor::Functor;

pub type RcTerm = Rc<Term>;
pub type TermVec = Rc<Vec<Rc<Term>>>;

// region Term declarations and definitions

/// Abstract representation
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
  /// An uppercase letter.
  Variable(DefaultAtom),
  Structure {
    functor: Functor,
    args: TermVec
  },
  /// A top-level term for non-queries.
  Program(RcTerm),
  /// Denoted `?-q` ("What `q`"), a top-level term for queries.
  Query(RcTerm),
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
      },
      Term::Empty => {
        format!("{}ε", prefix)
      },
      // Term::Register(i) => {
      //   format!("{}Register<{}>", prefix, i)
      // },
      Term::Variable(c) => {
        format!("{}Variable<{}>", prefix, c)
      },
      Term::Program(inner) => {
        format!("{}Program<{}>", prefix, inner)
      }
      Term::Query(inner) => {
        format!("{}Query<{}>", prefix, inner)
      }
    }
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
    let option_term = self.terms.pop_front();
    match &option_term {

      Some(rc_term) => {
        match &**rc_term {

          Term::Structure{args, ..} => {
            self.terms.extend(args.iter().cloned());
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::functor::ArityType;

  pub fn make_struct(name: &str, v: &TermVec) -> RcTerm {
    let new_vec = v.clone();

    let arity = new_vec.len() as ArityType;

    Rc::new(Term::Structure {
      functor: Functor{ name: DefaultAtom::from(name), arity},
      args: new_vec
    })
  }

  pub fn new_struct_of_term(name: char, term: &RcTerm) -> RcTerm {
    Rc::new(Term::Structure {
      functor: Functor{ name: DefaultAtom::from(name), arity},
      args: term.to_term_vec()
    })
  }
}
