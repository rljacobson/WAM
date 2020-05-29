
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;


use crate::functor::{Functor, ArityType};
use crate::address::Address;

pub type RcTerm = Rc<Term>;
pub type TermVec = Rc<Vec<Rc<Term>>>;
pub type BoxTerm = Box<Term>;

// region Term declarations and definitions

/// Abstract representation
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
  /// An uppercase letter other than X.
  Variable(char),
  Structure {
    functor: Functor,
    args: TermVec
  },
  /// Top-most term for non-queries.
  Program(RcTerm),
  /// Denoted `?-q` ("What `q`"), a non-variable term.
  Query(RcTerm),
  /// An X followed by a number, used only internally to process terms.
  Register(Address),
  /// Structures hold functors. Constants are represented as functors of arity 0, so `args` might
  /// be an empty `Vec`.
  /// Epsilon is the empty Term, used only to indicate that there was nothing to parse.
  Empty
}
/*
impl Clone for Term{
  fn clone(&self) -> Self {
    match self{
      Term::Program(term_ref) => {
        Term::Program(term_ref.clone())
      },
      Term::Query(term_ref)   => {
        Term::Query(term_ref.clone())
      },
      Term::Structure {functor, args} => {
        Term::Structure {functor: *functor, args: args.clone()}
      },
      Term::Variable(v) => {Term::Variable(*v)},
      Term::Register(v) => {Term::Register(*v)}
      Term::Empty => {Term::Empty }
    }
  }
}


/// Two `Structure`s are equal if and only if they have the same name and the same arity.
/// Equality is necessary for `assert_eq!` in tests.
impl PartialEq for Term {
  fn eq(&self, other: &Self) -> bool {
    match (self, other){
      (Term::Variable(v_self),Term::Variable(v_other)) =>{
        v_self == v_other
      }
      (Term::Register(v_self), Term::Register(v_other)) =>{
        v_self == v_other
      }
      (Term::Empty, Term::Empty) => {
        true
      },
      (Term::Program(bt_self), Term::Program(bt_other))
      | (Term::Query(bt_self), Term::Query(bt_other)) => {
        bt_self == bt_other
      },
      (
        Term::Structure {functor: f_self, ..},
        Term::Structure {functor: f_other, ..}
      ) => {
        f_self == f_other
      },
      _ => {
        false
      }
    }
  }
}

impl Eq for Term{}


/// Two `Structure`s are equal if and only if they have the same name and the same arity. Thus,
/// we don't want the `args` to be involved in the hash.
impl Hash for Term {
   fn hash<H: Hasher>(&self, state: &mut H) {
    match self{
      Term::Variable(v) => {
        discriminant(self).hash(state);
        v.hash(state);
      },
      Term::Register(v) => {
        discriminant(self).hash(state);
        v.hash(state);
      },
      Term::Structure { functor, .. } => {
        discriminant(self).hash(state);
        functor.hash(state);
      },
      Term::Program(v) => {
        discriminant(self).hash(state);
        v.deref().borrow().hash(state);
      },
      Term::Query(v) => {
        discriminant(self).hash(state);
        v.deref().borrow().hash(state);
      },
      Term::Empty => {
        discriminant(self).hash(state);
      }
    }
  }
}
*/

impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"".to_string(), &"".to_string()))
  }
}

impl Term{

  pub fn to_rc_term(&self) -> RcTerm {
    Rc::new(self.clone())
  }

  pub fn to_term_vec(&self) -> TermVec {
    Rc::new(vec![self.to_rc_term()])
  }

  pub fn to_struct_of_term(&self, name: char) -> RcTerm {
    new_struct_of_term(name, &Rc::new(self.clone()))
  }

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
      Term::Program(term) => {
        format!("Program\n-------\n{}", term)
      },
      Term::Query(term) => {
        format!("Query\n-------\n{}", term)
      }
    } // end match self
  }

}


pub fn to_query(term: RcTerm) -> BoxTerm{
  Box::new(Term::Query(
    term
  ))
}

pub fn to_program(term: RcTerm) -> BoxTerm{
  Box::new(Term::Program(
    term
  ))
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

// region Free functions

pub fn new_term_vec() -> TermVec {
  TermVec::new(Vec::<RcTerm>::new())
}

pub fn make_struct(name: char, v: &TermVec) -> RcTerm{
  let new_vec = v.clone();

  let arity = new_vec.len() as ArityType;

  Rc::new(Term::Structure {
    functor: Functor{name, arity},
    args: new_vec
  })
}

pub fn new_struct_of_term(name: char, term: &RcTerm) -> RcTerm {
  Rc::new(Term::Structure {
    functor: Functor{name, arity: 1},
    args: term.to_term_vec()
  })
}

// endregion
