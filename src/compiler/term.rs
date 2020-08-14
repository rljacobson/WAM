//! The abstract syntax tree type for programs and queries.

use std::collections::VecDeque;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use string_cache::{DefaultAtom, EmptyStaticAtomSet, Atom};

use crate::functor::*;
use crate::address::*;
use nom::lib::std::collections::HashMap;
use std::collections::hash_map::Entry;

pub type RcTerm = Rc<Term>;
pub type TermVec = Vec<Term>;


// region Term declarations and definitions

/// Abstract Syntax Representation
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
  /// An interned string staring with an uppercase letter.
  Variable(DefaultAtom),

  /**
    A `Structure` is a functor with arguments: `f(stuff)`. Constants are represented as functors of
    arity 0, so `args` might be an empty `Vec`. Note that a fact is a single predicate followed by a
    period: `f(stuff).`.
  */
  Structure {
    functor : Functor,
    args    : TermVec
  },

  /// `Rule` of the form `predicate :- predicate, predicate, ..., predicate.`. A so-called chain
  /// is a `Rule` in which `goals` contains a single element.
  Rule{
    head: Box<Term>, //  ToDo: RcTerm? Move to first position of goals?
    goals: TermVec
  },

  /// Denoted `?-q_1, q_2, .., q_m` ("What `q`"), a top-level term for queries. Anything else is a
  /// program.
  Query(TermVec),

  /// Flattened terms only have register pointers for arguments.
  Ref(Address)
}


impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"".to_string(), &"".to_string()))
  }
}


impl Term{

  /// Returns a string representation of the term, e.g. `"h(f(f(a)), f(a))"`.
  pub fn expression_string(&self) -> String{
    match self {

      Term::Variable(name) => name.to_string(),

      Term::Structure { functor, args } => {
        if args.is_empty() {
          format!("{}", functor.name)
        }else {
          let mut buffer = format!("{}(", functor.name);
          for (i, term) in args.iter().enumerate(){
            buffer.push_str(term.expression_string().as_str());
            if i != args.len() - 1 {
              buffer.push_str(", ");
            }
          }
          buffer.push(')');
          buffer
        }
      }

      Term::Rule { head: predicate, goals } => {
        if goals.is_empty() {
          // Should never happen unless we redesign a fact as a `Rule` with empty RHS.
          format!("{}.", predicate)
        }else {
          let mut buffer = format!("{} :- ", predicate);
          for (i, term) in goals.iter().enumerate(){
            buffer.push_str(term.expression_string().as_str());
            if i != goals.len() - 1 {
              buffer.push_str(", ");
            }
          }
          buffer.push('.');
          buffer
        }
      }

      Term::Query(goals) => {
        if goals.is_empty() {
          // Should never happen unless we redesign a fact as a `Rule` with empty RHS.
          "?-.".to_string()
        }else {
          let mut buffer = "?- ".to_string();
          for (i, term) in goals.iter().enumerate(){
            buffer.push_str(term.expression_string().as_str());
            if i != goals.len() - 1 {
              buffer.push_str(", ");
            }
          }
          buffer.push('.');
          buffer
        }
      }

      Term::Ref(address) => address.to_string(),

    }
  }


  /// A helper for `fmt_aux`, to keep things DRY.
  fn fmt_tree_term(buffer: &mut String, children: &TermVec,
                   child_prefix: &String, prefix_space: &String) {
    for (i, next) in children.iter().enumerate() {
      if i != children.len() - 1 {
        buffer.push_str(
          next.fmt_aux(
            &format!("{}{}{}", child_prefix, prefix_space, "├── "),
            &format!("{}{}{}", child_prefix, prefix_space, "│   ")
          ).as_str()
        );
        buffer.push('\n');
      } else {
        buffer.push_str(
          next.fmt_aux(
            &format!("{}{}{}", child_prefix, prefix_space, "└── "),
            &format!("{}{}{}", child_prefix, prefix_space, "    ")
          ).as_str()
        );
      }
    };
  }


  fn fmt_aux(&self, prefix: &String, child_prefix: &String) -> String {
    match self {

      Term::Variable(c)  => format!("{}Variable<{}>", prefix, c),

      Term::Structure { functor, args } => {
        let mut self_token =
          if args.is_empty() {
            format!("{}Constant<{}>", prefix, functor.name)
          } else{
            format!("{}Functor<{}(…)>\n", prefix, functor)
          };
        Self::fmt_tree_term(&mut self_token, args, child_prefix, &" ".repeat(12));
        self_token
      }

      Term::Rule { head: predicate, goals } => {
        //                     "      ┗━━━━━┿━━ "
        //                     "            ├── "
        let mut self_token = format!(
          "{}Rule< … :- ….>\n",
          prefix,
        );
        self_token.push_str(
          predicate.fmt_aux(
            &format!("{}      ┗━━━━┿━━ ", prefix),
            &format!("{}{}{}", " ".repeat(11), "│", " ".repeat(3))
          ).as_str()
        );
        self_token.push('\n');
        Self::fmt_tree_term(&mut self_token, goals,
                            child_prefix,
                            &" ".repeat(11));
        self_token
      }

      Term::Query(goals) => {
        let mut self_token = format!("{}Query<…>\n", prefix);
        Self::fmt_tree_term(&mut self_token, goals, child_prefix, &" ".repeat(6));
        self_token
      },

      Term::Ref(address) => format!("{}Reference<{}>", prefix, address)

    }
  }


  fn iter(&self) -> TermIter{
    TermIter::new(self)
  }


  /// Produces a HashMap of `(name, is_permanent)` for every variable in the term.
  pub fn variables(&self) -> HashMap<DefaultAtom, bool>{

    // Helper function
    fn fold_variable_lists(variables: &mut HashMap<DefaultAtom, bool>, term_list: &TermVec){
      for (i, goal) in term_list.iter().enumerate(){
        // The variables in head are included as variables in the first predicate.
        if i == 0 {
          variables.extend(goal.variables().iter());
        } else{
          for (name, is_permanent) in goal.variables().iter() {
            match variables.entry(name.clone()) {

              Entry::Occupied(mut entry) => {
                // The variable has previously been added. Set `is_permanent` to true for that
                // variable.
                entry.insert(true);
              }

              Entry::Vacant(mut entry) => {
                // Create an entry for this variable initialized to its existing value.
                entry.insert(*is_permanent);
              }

            }
          }
        }
      }
    }

    match self {

      Term::Rule { head, goals } => {
        let mut variables = head.variables();
        fold_variable_lists(&mut variables, goals);
        variables
      }

      Term::Query(goals) => {
        let mut variables = HashMap::new();
        fold_variable_lists(&mut variables, goals);
        variables
      }

      otherwise => {
        let mut variables = HashMap::new();
        // concat only.
        for term in self.iter(){
          if let Term::Variable(name) = term{
            variables.insert(name, false);
          }
        }
        variables
      }
    }
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

          Term::Structure {args, ..} => {
            self.terms.extend(args.iter());
            option_term
          }

          Term::Rule { head: predicate, goals} => {
            self.terms.push_back(predicate);
            self.terms.extend(goals.iter());
            option_term
          }

          Term::Query(term_vec) => {
            self.terms.extend(term_vec.iter());
            option_term
          }

          _t => option_term
        }
      }

      None => None
    }
  }
}

// endregion TermIter
