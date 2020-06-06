//! The abstract syntax tree type for programs and queries.

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use string_cache::DefaultAtom;

use crate::functor::Functor;

pub type RcTerm = Rc<Term>;
pub type TermVec = Vec<Rc<Term>>;

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
  /// Epsilon is the empty Term, used only to indicate that there was nothing to parse.
  Empty
}

impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"", &""))
  }
}

impl Term{

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
      },
      Term::Empty => {
        format!("{}ε", prefix)
      },
      Term::Variable(c) => {
        format!("{}Variable<{}>", prefix, c)
      },
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

/*
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

  pub fn new_struct_of_term(name: &str, term: &RcTerm) -> RcTerm {
    Rc::new(Term::Structure {
      functor: Functor{ name: DefaultAtom::from(name), arity: 1},
      args: vc![term]
    })
  }
}
*/
