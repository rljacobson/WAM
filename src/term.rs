use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::discriminant;
use std::ops::Deref;
use std::rc::Rc;


use crate::functor::{Functor, ArityType};
use crate::address::Address;

pub type RefTerm = Rc<RefCell<Term>>;
pub type RefTermVec = Rc<RefCell<Vec<Term>>>;

// region Term declarations and definitions

/// Abstract representation
#[derive(Debug)]
pub enum Term {
  /// An uppercase letter other than X.
  Variable(char),
  Structure {
    functor: Functor,
    args: RefTermVec
  },
  /// Top-most term for non-queries.
  Program(RefTerm),
  /// Denoted `?-q` ("What `q`"), a non-variable term.
  Query(RefTerm),
  /// An X followed by a number, used only internally to process terms.
  Register(Address),
  /// Structures hold functors. Constants are represented as functors of arity 0, so `args` might
  /// be an empty `Vec`.
  /// Epsilon is the empty Term, used only to indicate that there was nothing to parse.
  Empty
}

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


impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"".to_string(), &"".to_string()))
  }
}

impl Term{

  pub fn to_ref_term(&self) -> RefTerm{
    Rc::new(RefCell::new(self.clone()))
  }

  pub fn to_term_vec(&self) -> RefTermVec{
    Rc::new(RefCell::new(vec![self.clone()]))
  }

  pub fn to_struct_of_term(&self, name: char) -> Term {
    new_struct_of_term(name, self)
  }

  pub fn to_query(&self) -> Term{
    Term::Query(
      RefTerm::new(RefCell::new(self.clone()))
    )
  }

  pub fn to_program(&self) -> Term{
    Term::Program(
      RefTerm::new(RefCell::new(self.clone()))
    )
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
        let mut next: &Term;
        let args_ref = args.deref().borrow();
        for i in 0..args_ref.len() {
          next = &args_ref[i];
          if i != args_ref.len() - 1 {
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
        format!("Program\n-------\n{}", term.deref().borrow())
      },
      Term::Query(term) => {
        format!("Query\n-------\n{}", term.deref().borrow())
      }
    } // end match self
  }

  pub fn iter(&self) -> TermIter{
    TermIter::new(self.clone())
  }
}

// endregion Term

// region TermIter declarations and definitions

#[derive(Debug)]
pub struct TermIter{
  terms: VecDeque<Term>, // A stack of terms/tokens yet to be visited.
}

impl TermIter{
  pub fn new(start: Term) -> TermIter {
    TermIter{
      terms: VecDeque::from(vec![start])
    }
  }
}

/// Iterates over the terms in the term tree breadth first.
impl Iterator for TermIter{
  type Item = Term;

  fn next(&mut self) -> Option<Self::Item> {
    let current = self.terms.pop_front();

    match current.clone() {
      Some(Term::Structure {args, ..}) => {
        let arg_ref = args.deref().borrow_mut();
        for term in arg_ref.iter(){
          self.terms.push_back(term.clone());
        }
        current
      },
      _t => _t
    }
  }
}

// endregion TermIter

// region Free functions

pub fn new_ref_term_vec() -> RefTermVec{
  RefTermVec::new(RefCell::new(Vec::<Term>::new()))
}

pub fn make_struct(name: char, v: &Vec<Term>) -> Term{
  let new_vec = new_ref_term_vec();
  let mut vec_ref = new_vec.deref().borrow_mut();

  for v_other in v{
    vec_ref.push(v_other.clone());
  }
  // vec_ref.append(v);

  let arity = vec_ref.len() as ArityType;
  drop(vec_ref);

  Term::Structure {
    functor: Functor{name, arity},
    args: new_vec
  }
}

pub fn new_struct_of_term(name: char, term: &Term) -> Term {
  Term::Structure {
    functor: Functor{name, arity: 1},
    args: term.to_term_vec()
  }
}

// endregion


#[cfg(test)]
pub mod tests {
  use super::*;

  // The `term` module is tested along with the `parser` module in the `parser` module.

  // These functions only assist in constructing ASTs by hand. They are likely to only be used for
  // tests but may have other uses.

  pub fn new_term_vec() -> RefTermVec{
    Rc::new(RefCell::new(Vec::<Term>::new()))
  }

  pub fn new_constant(name: char) -> Term {
    Term::Structure {
      functor: Functor{name, arity: 0},
      args: new_term_vec()
    }
  }

  #[test]
  fn print_term(){
    let f = new_struct_of_term(
      'f',
      &Term::Register(Address::RegPtr(6))
    );

    let h = make_struct(
      'h',
      &mut vec![
        Term::Variable('Y'),
        new_struct_of_term(
          'f',
          &new_constant('a')
        )
      ]
    );

    let ast = make_struct(
      'p',
      &mut vec![
        f,
        h,
        Term::Variable('Y')
      ]
    ).to_program();

    let answer = "Program
-------
Functor<p/3(…)>
            ├── Functor<f/1(…)>
            │               └── Register<X[6]>
            ├── Functor<h/2(…)>
            │               ├── Variable<Y>
            │               └── Functor<f/1(…)>
            │                               └── Constant<a>
            └── Variable<Y>";
    assert_eq!(format!("{}", ast), answer);
  }

}
