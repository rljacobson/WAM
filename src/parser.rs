/*!

This module parses a string representing a term.

Terms have the following recursively defined lexical structure:

  * Variables: A single uppercase letter other than `X`
  * Registers: An `X` followed by an integer
  * Constants: A single lowercase letter
  * Functors: A single lowercase letter followed by a comma-delineated list of terms enclosed in
              parentheses
  * A Program: The top-most (outer-most) term with no prefix (cp. Query).
  * A Query: The literal `?-` followed by a top-most (outer-most) term

Whitespace is ignored. It is an error to have two terms adjacent except if separated by a comma
as part of a functor. We allow `X` as a variable name but emit a warning. We allow
comma-delineated lists to have a trailing comma without warning.

The internal representation of a functor is a (functor) structure, which has a name and
a list of arguments, themselves `Terms`. This is the only tree-like (recursive) `Term`
variant. Constants are represented as a structure with a zero-length argument list.

*/

use std::cell::RefCell;
use std::rc::Rc;

use string_cache::DefaultAtom;

use crate::chariter::CharIter;
use crate::term::{Term, TermVec};
use crate::functor::{Functor, ArityType};


/// Parses text to produce abstract syntax trees made of `Term`s.
/// Returns a tuple (atoms, queries).
pub fn parse<'b>(input: &'b str) -> (TermVec, TermVec){
  // For M_1, all atoms are facts.
  let mut atoms   :  TermVec = Vec::new();
  let mut queries :  TermVec = Vec::new();


  if input.is_empty() {
    panic!("Input is empty!")
  }

  // We need multiple mutable borrows for recursive calls to parsing functions.
  let text_ref: Box<RefCell<CharIter>> =
    Box::new(
      RefCell::new(
        CharIter::<'b>::new(input)
      )
    );


  loop {
    let atom = parse_aux(&text_ref).unwrap();
    if let Term::Query(term) = atom {
      queries.push(Rc::try_unwrap(term).unwrap());
    } else {
      atoms.push(atom);
    }
    let mut text = RefCell::borrow_mut(&*text_ref);

    // Allow trailing whitespace, but nothing more.
    text.trim_left();
    if text.is_empty() {
      break;
    }
  } // end loop

  (atoms, queries)
}


fn parse_aux(text_ref: &RefCell<CharIter>) -> Option<Term> {
  let mut text = text_ref.borrow_mut();
  let mut next_char: char;

  // Handle Comments, which require resetting `next_char`.
  loop {
    text.trim_left();
    match text.next(){
      Some(c) => {
        next_char = c;
      },
      None => {
        return None;
      }
    }

    // Single-line comments
    if next_char == '%' {
      // Eat until EOL or EOF.
      while text.peek() != None && text.next() != Some('\n'){
        // Gobble
      }
    }
    // In-line comments
    else if next_char == '/' && text.peek() == Some('*'){
      text.next();
      // Eat until EOL or EOF.
      while text.peek() != None {
        // Gobble
        next_char = text.next().unwrap();
        if next_char == '*' && text.peek() == Some('/'){
          text.next();
          // Break inner while-loop, reset `next_char`
          break;
        }
      }
    } else {
      // Not a comment, break outer loop.
      break;
    }

    //reset `next_char` at the top of the loop.
  } // end comment loop

  // A giant string of if-else's //

  if next_char.is_lowercase(){
    // Functor Structure //
    // Get the rest of the functor's name
    let name =
      match  text.get_prefix_match(char::is_alphanumeric){

        Some(rest) => DefaultAtom::from(
          format!("{}{}", next_char, rest)
        ),

        None => DefaultAtom::from(next_char.to_string())

    };

    // `parse_arg_list` needs mutable access to the `CharIter`.
    drop(text);
    let args = parse_arg_list(text_ref);

    Some(Term::Structure {
      functor: Functor{name, arity: args.len() as ArityType},
      args
    })
  }

  else if next_char == ',' || next_char == ')' {
    // The assumption here is that `parse_aux` was called recursively, and the current term has
    // been completely parsed.
    None
  }

  else if next_char.is_uppercase(){
    // A variable. Get the rest of the name.
    let name =
      match  text.get_prefix_match(char::is_alphanumeric){

        Some(rest)  => DefaultAtom::from(
          format!("{}{}", next_char, rest)
        ),

        None => DefaultAtom::from(next_char.to_string())

      };
    Some(Term::Variable(name))
  }

  else if next_char == '?' {
    // Note: There is no equivalent case for `Term::Program`. Every non-query is a program.
    match text.next() {

      Some('-') => {
        // `parse_aux` needs a mutable borrow of text, so drop this one.
        drop(text);
        let term_option = parse_aux(text_ref);
        match term_option {

          Some(term) => Some(Term::Query(Rc::new(term))),

          None       => None

        }
      }

      _ => {
        eprintln!("Error: Unexpected character `{}`", next_char);
        panic!();
      }

    }
  }

  else {
    eprintln!("Error: Unexpected character `{}`", next_char);
    panic!();
  }
}

/**
  Parses a comma separated list of terms and returns them in a vector. As functors can have zero
  arguments (constants), the vector returned may be empty.
*/
fn parse_arg_list(text_ref: &RefCell<CharIter>) -> TermVec {
  let mut args: TermVec = Vec::new();
  let mut text = text_ref.borrow_mut();

  text.trim_left();
  if let Some(c) = text.peek(){
    if c != '(' {
      // Constants can omit parentheses, as they have no arguments.
      return args;
    } else {
      // Eat `(`
      text.next();
    }
  }

  let mut term_option: Option<Term>;

  loop {
    // `parse_aux` needs a mutable borrow of text, so drop this one.
    drop(text);
    term_option = parse_aux(text_ref);
    text = text_ref.borrow_mut();
    if text.is_empty(){
      eprintln!("Reached EOL while looking for `)`.");
      panic!();
    }
    if term_option != None {
      args.push(term_option.unwrap());
    } else {
      return args;
    }

    text.trim_left();
    match text.peek(){

      Some(',') => {
        // Eat the `,` character.
        text.next();
      },

      Some(')') => {
        // Eat the `)` character and return.
        text.next();
        break;
      },

      _ => {
        // Don't advance.
      }

    } // end match peek
  } // end while
  args
}
